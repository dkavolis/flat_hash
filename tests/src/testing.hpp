/**
 * Copyright (c) 2022 <Daumantas Kavolis>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#pragma once

#include <span>

#include <catch2/catch_tostring.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <flat_hash/dictionary_fwd.hpp>
#include <flat_hash/set_fwd.hpp>

#if defined(FLAT_HASH_USE_FMTLIB)
#  include <fmt/ranges.h>
#endif

namespace Catch {
template <flat_hash::detail::formattable T>
struct StringMaker<T> {
  static auto convert(T const& value) -> std::string { return FLAT_HASH_FORMAT_NS format("{}", value); }
};

template <flat_hash::detail::formattable T>
struct is_range<T> {
  static const bool value = false;
};
}  // namespace Catch

FLAT_HASH_NAMESPACE_BEGIN

namespace testing {

template <class T>
  requires std::is_lvalue_reference_v<T>
[[nodiscard]] constexpr auto mutable_ref(T&& value) noexcept -> decltype(auto) {
  return const_cast<std::remove_cvref_t<T>&>(value);
}

template <class T, std::size_t N>
using aligned_buffer_t = std::aligned_storage_t<sizeof(T) * N, alignof(T)>;

template <std::ranges::input_range R>
class RangeMatcher : public Catch::Matchers::MatcherGenericBase {
  R values_;

 public:
  RangeMatcher(R r) noexcept : values_(std::forward<R>(r)) {}

  [[nodiscard]] constexpr auto values() const noexcept -> decltype(auto) {
    // std::views::filter_view requires mutable reference to iterate but matchers require const members
    // not great solution but each matcher should only be used once
    auto& ref = mutable_ref(values_);
    auto begin = std::ranges::begin(ref);
    auto end = std::ranges::end(ref);
    return std::ranges::subrange(begin, end);
  }
};

template <std::ranges::input_range R>
class ContainsAnyOf : public RangeMatcher<R> {
 public:
  ContainsAnyOf(R r) noexcept : RangeMatcher<R>(std::forward<R>(r)) {}

  template <class Key, class T>
  auto match(set<Key, T> const& s) const -> bool {
    return std::ranges::any_of(this->values(), [&s](auto&& v) { return s.contains(v); });
  }
  [[nodiscard]] auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("contains any of {}", Catch::Detail::stringify(this->values()));
  }
};

template <std::ranges::input_range R>
class ContainsAllOf : public RangeMatcher<R> {
 public:
  ContainsAllOf(R r) noexcept : RangeMatcher<R>(std::forward<R>(r)) {}

  template <class Key, class T>
  auto match(set<Key, T> const& s) const -> bool {
    return std::ranges::all_of(this->values(), [&s](auto&& v) { return s.contains(v); });
  }
  [[nodiscard]] auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("contains all of {}", Catch::Detail::stringify(this->values()));
  }
};

template <std::ranges::input_range R>
class ContainsNoneOf : public RangeMatcher<R> {
 public:
  ContainsNoneOf(R r) noexcept : RangeMatcher<R>(std::forward<R>(r)) {}

  template <class Key, class Traits>
  auto match(set<Key, Traits> const& s) const -> bool {
    return std::ranges::none_of(this->values(), [&s](auto&& v) { return s.contains(v); });
  }
  [[nodiscard]] auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("contains none of {}", Catch::Detail::stringify(this->values()));
  }
};

template <std::ranges::sized_range R>
class Equals : public RangeMatcher<R> {
 public:
  Equals(R r) noexcept : RangeMatcher<R>(std::forward<R>(r)) {}

  template <std::ranges::sized_range Range>
    requires(std::equality_comparable_with<std::ranges::range_reference_t<R>, std::ranges::range_reference_t<Range>>)
  auto match(Range const& s) const -> bool {
    if (std::ranges::size(this->values()) != std::ranges::size(s)) { return false; }

    auto set_begin = std::ranges::begin(s);
    auto set_end = std::ranges::end(s);
    auto begin = std::ranges::begin(this->values());

    while (set_begin != set_end) {
      if (*set_begin++ != *begin++) return false;
    }

    return true;
  }
  [[nodiscard]] auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("matches order of {}", Catch::Detail::stringify(this->values()));
  }
};

template <class T>
class Contains : public Catch::Matchers::MatcherGenericBase {
  T value_;

 public:
  Contains(T value) : value_(std::forward<T>(value)) {}

  template <class Key, class Traits>
  auto match(set<Key, Traits> const& s) const -> bool {
    return s.contains(value_);
  }
  auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("contains {}", Catch::Detail::stringify(value_));
  }
};

template <std::ranges::sized_range R>
class IterEquals : public Catch::Matchers::MatcherGenericBase {
 public:
  using iterator = std::ranges::iterator_t<R const&>;

 private:
  std::reference_wrapper<R const> range_;
  iterator it_;
  mutable iterator last_{};

 public:
  IterEquals(R const& r, iterator it) noexcept : range_(r), it_(it) {}

  auto match(iterator it) const -> bool {
    last_ = it;
    return it == it_;
  }

  [[nodiscard]] auto describe() const -> std::string override {
    auto first = std::ranges::begin(range_.get());
    auto end = std::ranges::cend(range_.get());

    if (it_ == end || last_ == end) {
      // cannot dereference
      return FLAT_HASH_FORMAT_NS format("matches {{?}}: <{:d}: {{?}}> == <{:d}: {{?}}>",
                                        std::ranges::distance(first, last_), std::ranges::distance(first, it_));
    }

    return FLAT_HASH_FORMAT_NS format("matches {{?}}: <{:d}: {}> == <{:d}: {}>", std::ranges::distance(first, last_),
                                      Catch::Detail::stringify(*last_), std::ranges::distance(first, it_),
                                      Catch::Detail::stringify(*it_));
  }
};

template <std::ranges::input_range R, class Alloc = std::allocator<std::ranges::range_value_t<R>>>
[[nodiscard]] auto to_vector(R&& r, Alloc const& alloc = Alloc()) -> std::vector<std::ranges::range_value_t<R>, Alloc> {
  std::vector<std::ranges::range_value_t<R>, Alloc> vector(alloc);
  detail::containers::assign_to_empty(vector, std::forward<R>(r));
  return vector;
}

namespace pmr {
template <std::ranges::input_range R>
[[nodiscard]] auto to_vector(R&& r, std::pmr::polymorphic_allocator<std::ranges::range_value_t<R>> const& alloc = {})
    -> std::pmr::vector<std::ranges::range_value_t<R>> {
  std::pmr::vector<std::ranges::range_value_t<R>> vector(alloc);
  detail::containers::assign_to_empty(vector, std::forward<R>(r));
  return vector;
}
}  // namespace pmr

template <class V, std::convertible_to<V>... Args>
  requires(!std::ranges::input_range<V>)
[[nodiscard]] auto to_vector(V&& value, Args&&... args) -> std::vector<std::remove_cvref_t<V>> {
  std::vector<std::remove_cvref_t<V>> vector;
  vector.reserve(1 + sizeof...(Args));
  vector.emplace_back(std::forward<V>(value));

  std::initializer_list<int>{{(vector.emplace_back(std::forward<Args>(args)), 0)...}};

  return vector;
}
}  // namespace testing

template <std::ranges::input_range R>
[[nodiscard]] auto ContainsAnyOf(R&& r) -> testing::ContainsAnyOf<R> {
  return {std::forward<R>(r)};
}
template <class T>
[[nodiscard]] auto ContainsAnyOf(std::initializer_list<T> r) -> testing::ContainsAnyOf<std::initializer_list<T>> {
  return {r};
}

template <std::ranges::input_range R>
[[nodiscard]] auto ContainsAllOf(R&& r) -> testing::ContainsAllOf<R> {
  return {std::forward<R>(r)};
}
template <class T>
[[nodiscard]] auto ContainsAllOf(std::initializer_list<T> r) -> testing::ContainsAllOf<std::initializer_list<T>> {
  return {r};
}

template <std::ranges::input_range R>
[[nodiscard]] auto ContainsNoneOf(R&& r) -> testing::ContainsNoneOf<R> {
  return {std::forward<R>(r)};
}
template <class T>
[[nodiscard]] auto ContainsNoneOf(std::initializer_list<T> r) -> testing::ContainsNoneOf<std::initializer_list<T>> {
  return {r};
}

template <std::ranges::input_range R>
[[nodiscard]] auto Equals(R&& r) -> testing::Equals<R> {
  return {std::forward<R>(r)};
}
template <class T>
[[nodiscard]] auto Equals(std::initializer_list<T> r) -> testing::Equals<std::initializer_list<T>> {
  return {r};
}

template <class T>
[[nodiscard]] auto Contains(T value) -> testing::Contains<T> {
  return {std::forward<T>(value)};
}

template <std::ranges::range R>
[[nodiscard]] auto IterEquals(R const& r, std::ranges::iterator_t<R const&> iter) -> testing::IterEquals<R> {
  return {r, iter};
}

FLAT_HASH_NAMESPACE_END
