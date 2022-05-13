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
#include <variant>

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

template <class T1, class T2>
  requires(flat_hash::detail::formattable<T1> || flat_hash::detail::formattable<T2>)
struct StringMaker<std::pair<T1, T2>> {
  static auto convert(std::pair<T1, T2> const& value) -> std::string {
    return FLAT_HASH_FORMAT_NS format("({}, {})", flat_hash::detail::maybe_format_arg(value.first),
                                      flat_hash::detail::maybe_format_arg(value.second));
  }
};

template <flat_hash::detail::formattable T>
struct is_range<T> {
  static const bool value = false;
};
}  // namespace Catch

FLAT_HASH_NAMESPACE_BEGIN

namespace testing {

template <class T, class Hashed>
concept comparator_for = requires { requires detail::equality_comparator<T, std::ranges::range_reference_t<Hashed>>; };

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

template <class T>
struct contains_pred {
  static_assert(sizeof(T) == 0, "contains_pred is not specialized!");
};

template <class T>
contains_pred(T&&) -> contains_pred<std::remove_cvref_t<T>>;

template <is_set Set>
struct contains_pred<Set> {
  Set const& s;

  template <detail::hashed_lookup_key<Set> K>
  [[nodiscard]] constexpr auto operator()(K const& key) const -> bool {
    return s.contains(key);
  }
};

template <is_dictionary Dict>
struct contains_pred<Dict> {
  Dict const& d;

  template <detail::hashed_lookup_key<Dict> K>
  [[nodiscard]] constexpr auto operator()(K const& key) const -> bool {
    return d.contains(key);
  }

  template <detail::pair_like P>
    requires(!detail::hashed_lookup_key<P, Dict> &&
             detail::weakly_equality_comparable_with<std::tuple_element_t<1, P> const&,
                                                     typename Dict::mapped_type const&>)
  [[nodiscard]] constexpr auto operator()(P const& pair) const -> bool {
    auto&& [key, value] = pair;
    auto iter = d.find(key);
    if (iter == d.end()) { return false; }
    return iter.value() == value;
  }
};

template <std::ranges::input_range R>
class ContainsAnyOf : public RangeMatcher<R> {
 public:
  ContainsAnyOf(R r) noexcept : RangeMatcher<R>(std::forward<R>(r)) {}

  template <class Hashed>
  auto match(Hashed const& s) const -> bool {
    return std::ranges::any_of(this->values(), contains_pred{s});
  }

  [[nodiscard]] auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("contains any of {}", Catch::Detail::stringify(this->values()));
  }
};

template <std::ranges::input_range R>
class ContainsAllOf : public RangeMatcher<R> {
 public:
  ContainsAllOf(R r) noexcept : RangeMatcher<R>(std::forward<R>(r)) {}

  template <class Hashed>
  auto match(Hashed const& s) const -> bool {
    return std::ranges::all_of(this->values(), contains_pred{s});
  }

  [[nodiscard]] auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("contains all of {}", Catch::Detail::stringify(this->values()));
  }
};

template <std::ranges::input_range R, comparator_for<R> Comp = detail::equal_to>
class ContainsAllOfExcept : public RangeMatcher<R> {
  std::ranges::range_value_t<R> except_;
  Comp compare_;

 public:
  ContainsAllOfExcept(R r, std::ranges::range_value_t<R> value, Comp compare = {}) noexcept
      : RangeMatcher<R>(std::forward<R>(r)), except_(std::move(value)), compare_(compare) {}

  template <class Hashed>
  auto match(Hashed const& s) const -> bool {
    contains_pred pred{s};
    return std::ranges::all_of(
        this->values(), [this, pred](auto&& value) { return compare_(value, except_) ? !pred(value) : pred(value); });
  }

  [[nodiscard]] auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("contains all of {} except {}", Catch::Detail::stringify(this->values()),
                                      Catch::Detail::stringify(except_));
  }
};

template <std::ranges::input_range R>
class ContainsNoneOf : public RangeMatcher<R> {
 public:
  ContainsNoneOf(R r) noexcept : RangeMatcher<R>(std::forward<R>(r)) {}

  template <class Hashed>
  auto match(Hashed const& s) const -> bool {
    return std::ranges::none_of(this->values(), contains_pred{s});
  }
  [[nodiscard]] auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("contains none of {}", Catch::Detail::stringify(this->values()));
  }
};

template <class R, class Comp = detail::equal_to>
class Equals;

template <class T>
Equals(T&&) -> Equals<T, detail::equal_to>;
template <class T, class Comp>
Equals(T&&, Comp) -> Equals<T, Comp>;

template <class R, class Comp>
  requires(!std::ranges::range<R>)
class Equals<R, Comp> : public Catch::Matchers::MatcherGenericBase {
  R item_;
  Comp compare_;

 public:
  Equals(R item, Comp compare = {}) noexcept : item_(std::forward<R>(item)), compare_(compare) {}

  template <class T>
  auto match(T const& s) const -> bool {
    return compare_(s, item_);
  }
  template <class T>
  auto match(std::optional<T> const& s) const -> bool {
    if constexpr (requires { item_ == std::nullopt; }) {
      if (item_ == std::nullopt) { return s == std::nullopt; }
    }
    if (s == std::nullopt) { return false; }
    return compare_(*s, item_);
  }
  [[nodiscard]] auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("equals {}", Catch::Detail::stringify(item_));
  }
};

template <std::ranges::sized_range R, comparator_for<R> Comp>
class Equals<R, Comp> : public RangeMatcher<R> {
  Comp compare_;

 public:
  Equals(R r, Comp compare = {}) noexcept : RangeMatcher<R>(std::forward<R>(r)), compare_(compare) {}

  template <std::ranges::sized_range Range>
  auto match(Range const& s) const -> bool {
    if (std::ranges::size(this->values()) != std::ranges::size(s)) { return false; }

    auto set_begin = std::ranges::begin(s);
    auto set_end = std::ranges::end(s);
    auto begin = std::ranges::begin(this->values());

    while (set_begin != set_end) {
      if (!compare_(*set_begin++, *begin++)) return false;
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

  template <class Hashed>
  auto match(Hashed const& s) const -> bool {
    return contains_pred{s}(value_);
  }
  auto describe() const -> std::string override {
    return FLAT_HASH_FORMAT_NS format("contains {}", Catch::Detail::stringify(value_));
  }
};

template <std::ranges::sized_range R>
class IterEquals : public Catch::Matchers::MatcherGenericBase {
 public:
  using iterator = typename R::const_iterator;
  using difference_type = std::ranges::range_difference_t<R>;

 private:
  std::reference_wrapper<R const> range_;
  // using variant with difference type since the iterators can be invalidated by inserting/erasing elements, offset
  // will obtain a valid iterator at the time of comparison
  std::variant<iterator, difference_type> pos_;
  mutable iterator last_{};  // match/describe have to be const, doesn't matter as matchers are not reused

 public:
  IterEquals(R const& r, iterator pos) noexcept : range_(r), pos_(pos) {}
  IterEquals(R const& r, difference_type offset) noexcept : range_(r), pos_(offset) {}

  auto expected() const -> iterator {
    if (std::holds_alternative<iterator>(pos_)) { return std::get<iterator>(pos_); }
    auto offset = std::get<difference_type>(pos_);
    if (offset < 0) { offset += std::ranges::ssize(range_.get()); }
    auto iter = std::ranges::begin(range_.get());
    std::ranges::advance(iter, offset);
    return iter;
  }

  auto match(iterator it) const -> bool {
    last_ = it;
    return it == expected();
  }

  [[nodiscard]] auto describe() const -> std::string override {
    auto first = std::ranges::begin(range_.get());
    auto end = std::ranges::cend(range_.get());
    auto iter = expected();

    if (iter == end || last_ == end) {
      // cannot dereference
      return FLAT_HASH_FORMAT_NS format("matches {{?}}: <{:d}: {{?}}> == <{:d}: {{?}}>",
                                        std::ranges::distance(first, last_), std::ranges::distance(first, iter));
    }

    return FLAT_HASH_FORMAT_NS format("matches {{?}}: <{:d}: {}> == <{:d}: {}>", std::ranges::distance(first, last_),
                                      Catch::Detail::stringify(*last_), std::ranges::distance(first, iter),
                                      Catch::Detail::stringify(*iter));
  }
};

template <class T1, class T2>
class PairMatcher : public Catch::Matchers::MatcherGenericBase {
  T1 first_;
  T2 second_;

 public:
  PairMatcher(T1 first, T2 second) : first_(std::forward<T1>(first)), second_(std::forward<T2>(second)) {}

  template <detail::pair_like P>
  auto match(P&& pair) const -> bool {
    return first_.match(std::get<0>(std::forward<P>(pair))) && second_.match(std::get<1>(std::forward<P>(pair)));
  }

  auto describe() const -> std::string {
    return FLAT_HASH_FORMAT_NS format("{} and {}", first_.describe(), second_.describe());
  }
};

struct IsTrueMatcher : public Catch::Matchers::MatcherGenericBase {
  auto match(bool v) const noexcept -> bool { return v; }
  auto describe() const -> std::string { return "is true"; }
};

struct IsFalseMatcher : public Catch::Matchers::MatcherGenericBase {
  auto match(bool v) const noexcept -> bool { return !v; }
  auto describe() const -> std::string { return "is false"; }
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

template <std::ranges::input_range R, testing::comparator_for<R> Comp = detail::equal_to>
[[nodiscard]] auto ContainsAllOfExcept(R&& r, std::ranges::range_value_t<R> except, Comp compare = {})
    -> testing::ContainsAllOfExcept<R, Comp> {
  return {std::forward<R>(r), std::move(except), compare};
}
template <class T, testing::comparator_for<std::initializer_list<T>> Comp = detail::equal_to>
[[nodiscard]] auto ContainsAllOfExcept(std::initializer_list<T> r, T except, Comp compare = {})
    -> testing::ContainsAllOfExcept<std::initializer_list<T>, Comp> {
  return {r, std::move(except), compare};
}

template <std::ranges::input_range R>
[[nodiscard]] auto ContainsNoneOf(R&& r) -> testing::ContainsNoneOf<R> {
  return {std::forward<R>(r)};
}
template <class T>
[[nodiscard]] auto ContainsNoneOf(std::initializer_list<T> r) -> testing::ContainsNoneOf<std::initializer_list<T>> {
  return {r};
}

template <class T, detail::equality_comparator<T> Comp = detail::equal_to>
  requires(!std::ranges::input_range<T>)
[[nodiscard]] auto Equals(T&& item, Comp compare = {}) -> testing::Equals<T, Comp> {
  return {std::forward<T>(item), compare};
}

template <std::ranges::input_range R,
          detail::equality_comparator<std::ranges::range_reference_t<R>> Comp = detail::equal_to>
[[nodiscard]] auto Equals(R&& r, Comp compare = {}) -> testing::Equals<R, Comp> {
  return {std::forward<R>(r), compare};
}
template <class T, detail::equality_comparator<T> Comp = detail::equal_to>
[[nodiscard]] auto Equals(std::initializer_list<T> r, Comp compare = {})
    -> testing::Equals<std::initializer_list<T>, Comp> {
  return {r, compare};
}

template <class T>
[[nodiscard]] auto Contains(T value) -> testing::Contains<T> {
  return {std::forward<T>(value)};
}

template <std::ranges::range R>
[[nodiscard]] auto IterEquals(R const& r, std::ranges::iterator_t<R const&> iter) -> testing::IterEquals<R> {
  return {r, iter};
}

template <std::ranges::range R>
[[nodiscard]] auto IterEquals(R const& r, std::ranges::range_difference_t<R> pos) -> testing::IterEquals<R> {
  return {r, pos};
}

template <class T1, class T2>
[[nodiscard]] auto PairMatches(T1&& first, T2&& second) -> testing::PairMatcher<T1, T2> {
  return {std::forward<T1>(first), std::forward<T2>(second)};
}

[[nodiscard]] inline auto IsTrue() noexcept -> testing::IsTrueMatcher { return {}; }
[[nodiscard]] inline auto IsFalse() noexcept -> testing::IsFalseMatcher { return {}; }

FLAT_HASH_NAMESPACE_END
