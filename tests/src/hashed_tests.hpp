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

#include <catch2/generators/catch_generators_adapters.hpp>
#include <catch2/matchers/catch_matchers_predicate.hpp>
#include <catch2/matchers/catch_matchers_quantifiers.hpp>

#include "testing.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace testing {

template <class T, class Hashed>
concept projection_for =
    requires { requires requires(T && proj, typename Hashed::const_reference ref) { std::forward<T>(proj)(ref); }; };

template <class Hashed, bool EmptyTests = true, comparator_for<Hashed> Comp = detail::equal_to>
void test_hashed_constructors(std::initializer_list<typename Hashed::value_type> init_a,
                              std::initializer_list<typename Hashed::value_type> init_b,
                              std::initializer_list<typename Hashed::value_type> duplicates, Comp compare = {}) {
  // duplicates need to be equal to init_a with when duplicate values are removed

  if constexpr (EmptyTests) {
    SECTION("using a default constructor") {
      Hashed h;

      CHECK_THAT(h, Catch::Matchers::IsEmpty());
    }

    SECTION("from a bucket count") {
      Hashed h(16);

      CHECK_THAT(h, Catch::Matchers::IsEmpty());
      CHECK(h.bucket_count() >= 16);
    }
  }

  SECTION("from an initializer_list") {
    Hashed h{init_a};

    CHECK(h.bucket_count() > h.size());
    CHECK_THAT(h, Equals(init_a, compare) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("from a pair of iterators") {
    Hashed h(init_a.begin(), init_a.end());

    CHECK(h.bucket_count() > h.size());
    CHECK_THAT(h, Equals(init_a, compare) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("from an initializer_list with duplicate values") {
    Hashed h{duplicates};

    CHECK(h.bucket_count() > h.size());
    CHECK_THAT(h, Equals(init_a, compare) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("from an pair of iterators with duplicate values") {
    Hashed h{duplicates.begin(), duplicates.end()};

    CHECK(h.bucket_count() > h.size());
    CHECK_THAT(h, Equals(init_a, compare) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("from a value range") {
    std::vector v = init_a;
    Hashed h{std::move(v)};

    CHECK(h.bucket_count() > h.size());
    CHECK_THAT(h, Equals(init_a, compare) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("as a copy") {
    Hashed h{init_a};
    Hashed copy = h;

    CHECK(h.size() == copy.size());
    CHECK(h.bucket_count() == copy.bucket_count());
    CHECK_THAT(h, Equals(copy, compare));
    CHECK_THAT(copy, Equals(h, compare) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("can be assigned an initializer list") {
    if constexpr (EmptyTests) {
      WHEN("the container is empty") {
        Hashed h;
        h = init_a;
        CHECK_THAT(h, Equals(init_a, compare) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
      }
    }

    WHEN("the container is not empty") {
      Hashed h{init_b};
      h = init_a;
      CHECK_THAT(h, Equals(init_a, compare) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
    }
  }
}

template <class Hashed, comparator_for<Hashed> Comp = detail::equal_to>
void test_hashed_swap(std::initializer_list<typename Hashed::value_type> init_a,
                      std::initializer_list<typename Hashed::value_type> init_b, Comp compare = {}) {
  Hashed a{init_a};

  SECTION("swapping with another container swaps contained values") {
    Hashed b{init_b};

    std::ranges::swap(a, b);

    CHECK_THAT(a, Equals(init_b, compare) && ContainsAllOf(init_b) && ContainsNoneOf(init_a));
    CHECK_THAT(b, Equals(init_a, compare) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("Swapping with itself has no effect") {
    auto* ptr = std::addressof(a);
    std::ranges::swap(a, a);
    CHECK(std::addressof(a) == ptr);
  }
}

template <class Hashed>
void test_hashed_max_size() {
  using index_t = Hashed::index_type;

  Hashed h;
  CHECK(h.max_size() < std::numeric_limits<index_t>::max());
  CHECK(std::bit_width(h.max_size()) == sizeof(index_t) * CHAR_BIT - h.probing().reserved_bits());
}

template <class T>
[[nodiscard]] constexpr auto add_const(T& t) noexcept -> T const& {
  return t;
}

template <class Hashed, class HeteroKey = void, bool ConstTest = false, comparator_for<Hashed> Comp = detail::equal_to,
          projection_for<Hashed> Proj = std::identity>
void test_hashed_lookup(std::initializer_list<typename Hashed::value_type> init,
                        std::initializer_list<typename Hashed::value_type> extra, Comp compare = {}, Proj proj = {}) {
  constexpr bool HeteroTest = !std::same_as<void, HeteroKey>;
  static_assert(!HeteroTest || detail::hashed_lookup_key<HeteroKey, Hashed>);
  Hashed h = init;

  SECTION("contains returns true if key is in the container") {
    auto&& key = GENERATE_COPY(map(proj, from_range(init)));
    SECTION("by key_type") { CHECK(h.contains(key)); }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") { CHECK(h.contains(HeteroKey(key))); }
    }
  }

  SECTION("contains returns false if key is not in the container") {
    auto&& key = GENERATE_COPY(map(proj, from_range(extra)));
    SECTION("by key_type") { CHECK_FALSE(h.contains(key)); }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") { CHECK_FALSE(h.contains(HeteroKey(key))); }
    }
  }

  SECTION("count returns 1 if key is in the container") {
    auto&& key = GENERATE_COPY(map(proj, from_range(init)));
    SECTION("by key_type") { CHECK(h.count(key) == 1); }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") { CHECK(h.count(HeteroKey(key)) == 1); }
    }
  }

  SECTION("count returns 0 if key is not in the container") {
    auto&& key = GENERATE_COPY(map(proj, from_range(extra)));
    SECTION("by key_type") { CHECK(h.count(key) == 0); }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") { CHECK(h.count(HeteroKey(key)) == 0); }
    }
  }
  SECTION("find returns iterator to the found element if key is in the container") {
    auto offset = GENERATE_COPY(range(0, static_cast<int>(init.size())));
    auto find_test = [offset](auto& container, auto&& key) {
      CHECK_THAT(container.find(key), IterEquals(container, offset));
      if constexpr (ConstTest) { CHECK_THAT(add_const(container).find(key), IterEquals(container, offset)); }
    };

    auto&& key = proj(*(init.begin() + offset));
    SECTION("by key_type") {
      find_test(h, key);
      if constexpr (ConstTest) { find_test(add_const(h), key); }
    }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") {
        find_test(h, HeteroKey(key));
        if constexpr (ConstTest) { find_test(add_const(h), HeteroKey(key)); }
      }
    }
  }

  SECTION("find returns iterator to the end if key is not in the container") {
    constexpr auto find_test = [](auto& container, auto&& key) {
      CHECK_THAT(container.find(key), IterEquals(container, container.end()));
      if constexpr (ConstTest) { CHECK_THAT(add_const(container).find(key), IterEquals(container, container.cend())); };
    };
    auto&& key = GENERATE_COPY(map(proj, from_range(extra)));
    SECTION("by key_type") { find_test(h, key); }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") { find_test(h, HeteroKey(key)); }
    }
  }

  constexpr static auto convert = [](auto&& iter_pair) noexcept {
    return std::ranges::subrange(iter_pair.first, iter_pair.second);
  };

  SECTION("equal_range returns range to the found element if key is in the container") {
    auto&& value = GENERATE_COPY(from_range(init));
    auto equal_range_test = [&value, compare](auto& container, auto&& key) {
      CHECK_THAT(convert(container.equal_range(key)), Equals(std::views::single(value), compare));
      if constexpr (ConstTest) {
        CHECK_THAT(convert(add_const(container).equal_range(key)), Equals(std::views::single(value), compare));
      }
    };

    auto&& key = proj(value);
    SECTION("by key_type") { equal_range_test(h, key); }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") { equal_range_test(h, HeteroKey(key)); }
    }
  }

  SECTION("equal_range returns empty range if key is not in the container") {
    constexpr auto equal_range_test = [](auto& container, auto&& key) {
      CHECK_THAT(convert(container.equal_range(key)), Catch::Matchers::IsEmpty());
      if constexpr (ConstTest) {
        CHECK_THAT(convert(add_const(container).equal_range(key)), Catch::Matchers::IsEmpty());
      }
    };

    auto&& key = GENERATE_COPY(map(proj, from_range(extra)));
    SECTION("by key_type") { equal_range_test(h, key); }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") { equal_range_test(h, HeteroKey(key)); }
    }
  }

  SECTION("bucket returns value in bucket_count() range if key is in the container") {
    auto&& key = GENERATE_COPY(map(proj, from_range(init)));
    SECTION("by key_type") { CHECK(h.bucket(key) < h.bucket_count()); }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") { CHECK(h.bucket(HeteroKey(key)) < h.bucket_count()); }
    }
  }

  SECTION("bucket returns bucket_count() if key is not in the container") {
    auto&& key = GENERATE_COPY(map(proj, from_range(extra)));
    SECTION("by key_type") { CHECK(h.bucket(key) == h.bucket_count()); }
    if constexpr (HeteroTest) {
      SECTION("by compatible key") { CHECK(h.bucket(HeteroKey(key)) == h.bucket_count()); }
    }
  }

  SECTION("bucket_size returns 1 if key is in the container") {
    auto&& key = GENERATE_COPY(map(proj, from_range(init)));
    CHECK(h.bucket_size(h.bucket(key)) == 1);
  }

  SECTION("bucket_size returns 0 if key is not in the container") {
    auto iota = std::views::iota(0U, h.bucket_count());
    CHECK(std::ranges::count_if(iota, [&h](auto index) -> bool { return h.bucket_size(index) == 1; }) == h.ssize());
  }
}

template <class Hashed>
void test_hashed_reserve(std::initializer_list<typename Hashed::value_type> init) {
  Hashed h = init;
  CHECK(h.capacity() == init.size());
  auto buckets = h.bucket_count();
  auto n = init.size() * 2;

  SECTION("reserving more will increase capacity and may increase the bucket count") {
    h.reserve(n);
    CHECK(h.capacity() == n);
    CHECK(h.bucket_count() > buckets);
    CHECK_THAT(h, ContainsAllOf(init));
  }

  SECTION("reserving less than size() has no effect") {
    h.reserve(1);
    CHECK(h.capacity() == init.size());
    CHECK(h.bucket_count() == buckets);
    CHECK_THAT(h, ContainsAllOf(init));
  }
}

template <class Hashed>
void test_hashed_splicing(std::initializer_list<typename Hashed::value_type> init_a,
                          std::initializer_list<typename Hashed::value_type> init_b) {
  Hashed a = init_a;
  Hashed b = init_b;

  SECTION("two different containers will result in an empty second container") {
    a.splice(b);

    CHECK_THAT(a, ContainsAllOf(init_a) && ContainsAllOf(init_b));
    CHECK_THAT(b, Catch::Matchers::IsEmpty());
  }

  SECTION("duplicate values will be left in the second container") {
    auto ssize = std::ranges::ssize(init_a);
    auto offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto subvalues = std::ranges::subrange(init_a.begin(), init_a.begin() + offset);
    b.merge(subvalues);

    a.splice(b);
    CHECK_THAT(a, ContainsAllOf(init_a) && ContainsAllOf(init_b));
    CHECK_THAT(b, ContainsAllOf(subvalues) && ContainsNoneOf(init_b));
  }
}

struct empty_bucket {
  template <std::unsigned_integral U>
  [[nodiscard]] constexpr auto operator()(U value) const noexcept -> bool {
    return value == std::numeric_limits<U>::max();
  }
};

template <class Hashed>
void test_hashed_clear(std::initializer_list<typename Hashed::value_type> init) {
  Hashed h = init;
  h.clear();
  CHECK_THAT(h, Catch::Matchers::IsEmpty());
  CHECK_THAT(h.table(), Catch::Matchers::AllMatch(Catch::Matchers::Predicate<typename Hashed::index_type>(
                            empty_bucket{}, "bucket is empty")));
}

template <class Hashed, comparator_for<Hashed> Comp = detail::equal_to, class HeteroProj = int>
void test_hashed_insert(std::initializer_list<typename Hashed::value_type> init,
                        std::initializer_list<typename Hashed::value_type> extra, Comp compare = {},
                        HeteroProj proj = {}) {
  using diff_t = Hashed::difference_type;
  Hashed h = init;
  diff_t ssize = h.ssize();

  SECTION("inserting a pair of iterators inserts all new values") {
    h.insert(extra.begin(), extra.end());
    CHECK_THAT(h, ContainsAllOf(init) && ContainsAllOf(extra));
  }

  SECTION("inserting a pair of iterators skips contained values") {
    h.insert(init.begin(), init.end());
    CHECK_THAT(h, Equals(init, compare));
  }

  SECTION("inserting an initializer_list inserts all new values") {
    h.insert(extra);
    CHECK_THAT(h, ContainsAllOf(init) && ContainsAllOf(extra));
  }

  SECTION("inserting an initializer_list skips contained values") {
    h.insert(init);
    CHECK_THAT(h, Equals(init, compare));
  }

  SECTION("inserting a new value inserts it at the end") {
    auto value = GENERATE_COPY(from_range(extra));
    WHEN("inserting a value reference") {
      CHECK_THAT(h.insert(value), PairMatches(IterEquals(h, -1), IsTrue()));
      CHECK_THAT(h, ContainsAllOf(init) && Contains(value));
    }

    WHEN("inserting an rvalue reference") {
      auto v = value;
      CHECK_THAT(h.insert(std::move(v)), PairMatches(IterEquals(h, -1), IsTrue()));
      CHECK_THAT(h, ContainsAllOf(init) && Contains(value));
    }

    if constexpr (!std::same_as<HeteroProj, int>) {
      WHEN("inserting a heterogeneous value") {
        auto v = proj(value);
        CHECK_THAT(h.insert(v), PairMatches(IterEquals(h, -1), IsTrue()));
        CHECK_THAT(h, ContainsAllOf(init) && Contains(value));
      }
    }

    WHEN("inserting into an empty container") {
      Hashed empty;
      REQUIRE_THAT(empty.insert(value), PairMatches(IterEquals(empty, 0), IsTrue()));
      CHECK_THAT(empty, Equals(std::views::single(value), compare) && Contains(value));
    }
  }

  SECTION(
      "inserting a contained value doesn't change the container and returns the iterator to the element preventing "
      "insertion") {
    diff_t offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto&& value = *(init.begin() + offset);
    CHECK_THAT(h.insert(value), PairMatches(IterEquals(h, offset), IsFalse()));
  }

  SECTION("inserting a new value with an iterator inserts it at the specified position") {
    auto value = GENERATE_COPY(from_range(extra));
    diff_t offset = GENERATE_COPY(range(0 * ssize, ssize + 1));
    auto pos = h.cbegin() + offset;
    WHEN("inserting a value reference") {
      CHECK_THAT(h.insert(pos, value), IterEquals(h, offset));
      CHECK_THAT(h, ContainsAllOf(init) && Contains(value));
    }

    WHEN("inserting an rvalue reference") {
      auto v = value;
      CHECK_THAT(h.insert(pos, std::move(v)), IterEquals(h, offset));
      CHECK_THAT(h, ContainsAllOf(init) && Contains(value));
    }

    if constexpr (!std::same_as<HeteroProj, int>) {
      WHEN("inserting a heterogeneous value") {
        auto v = proj(value);
        CHECK_THAT(h.insert(pos, v), IterEquals(h, offset));
        CHECK_THAT(h, ContainsAllOf(init) && Contains(value));
      }
    }
  }

  SECTION(
      "inserting a contained value with an iterator doesn't change the container and returns the iterator to the "
      "element preventing "
      "insertion") {
    diff_t offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto at = GENERATE_COPY(range(0 * ssize, ssize));
    auto&& value = *(init.begin() + offset);
    CHECK_THAT(h.insert(h.cbegin() + at, value), IterEquals(h, offset));
    CHECK_THAT(h, Equals(init, compare));
  }
}

template <class Hashed, class... Args>
void test_hashed_emplace(std::initializer_list<typename Hashed::value_type> init, Args&&... args) {
  using diff_t = Hashed::difference_type;
  Hashed h = init;
  diff_t ssize = h.ssize();
  typename Hashed::value_type expected(args...);

  SECTION("emplace inserts new values at the end") {
    CHECK_THAT(h.emplace(std::forward<Args>(args)...), PairMatches(IterEquals(h, -1), IsTrue()));
    CHECK_THAT(h, ContainsAllOf(init) && Contains(expected));
  }

  SECTION("emplace returns an iterator to the element preventing insertion if key was found") {
    diff_t offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto value = *(init.begin() + offset);
    CHECK_THAT(h.emplace(std::move(value)), PairMatches(IterEquals(h, offset), IsFalse()));
  }

  diff_t offset = GENERATE_COPY(range(0 * ssize, ssize + 1));
  SECTION("emplace_hint inserts new values at the specified positions") {
    CHECK_THAT(h.emplace_hint(h.cbegin() + offset, std::forward<Args>(args)...), IterEquals(h, offset));
    CHECK_THAT(h, ContainsAllOf(init) && Contains(expected));
  }

  SECTION("emplace_hint returns an iterator to the element preventing insertion if key was found") {
    diff_t at = GENERATE_COPY(range(0 * ssize, ssize));
    auto value = *(init.begin() + at);
    CHECK_THAT(h.emplace_hint(h.cbegin() + offset, value), IterEquals(h, at));
  }
}

template <class Hashed>
void test_hashed_table_expands(std::initializer_list<typename Hashed::value_type> init,
                               std::initializer_list<typename Hashed::value_type> extra) {
  Hashed h = init;
  auto old = h.bucket_count();
  h.insert(extra);
  CHECK(h.bucket_count() > old);
  CHECK_THAT(h, ContainsAllOf(init) && ContainsAllOf(extra));
}

template <class Hashed>
void test_hashed_merge(std::initializer_list<typename Hashed::value_type> init,
                       std::initializer_list<typename Hashed::value_type> extra) {
  Hashed a = init;
  Hashed b = extra;
  b.insert(init);

  SECTION("merging two containers results in their union") {
    a.merge(b);
    CHECK(a.size() == init.size() + extra.size());
    CHECK_THAT(a, ContainsAllOf(init) && ContainsAllOf(extra));
  }

  SECTION("merging with a range of unique values results in a union") {
    a.merge(std::ranges::ref_view{extra});
    CHECK(a.size() == init.size() + extra.size());
    CHECK_THAT(a, ContainsAllOf(init) && ContainsAllOf(extra));
  }

  SECTION("merging with a range of duplicate values results ignores duplicates") {
    std::vector values(b.begin(), b.end());
    values.insert(values.cend(), b.begin(), b.end());
    a.merge(values);
    CHECK(a.size() == init.size() + extra.size());
    CHECK_THAT(a, ContainsAllOf(init) && ContainsAllOf(extra));
  }
}

template <class Hashed, comparator_for<Hashed> Comp = detail::equal_to, class KeyProj = std::identity,
          class HeteroProj = int>
void test_hashed_erase(std::initializer_list<typename Hashed::value_type> init,
                       std::initializer_list<typename Hashed::value_type> extra, Comp compare = {},
                       KeyProj key_proj = {}, HeteroProj hetero_proj = {}) {
  Hashed h = init;
  auto ssize = h.ssize();

  SECTION("items can be erased by their iterator") {
    auto offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto pos = h.cbegin() + offset;
    CHECK_THAT(h.erase(pos), IterEquals(h, offset));
    CHECK_THAT(h, ContainsAllOfExcept(init, *(init.begin() + offset), compare));
  }

  SECTION("ranges of items can be erased") {
    auto start = GENERATE_COPY(range(0 * ssize, ssize));
    auto stop = GENERATE_COPY(range(start, ssize + 1));
    CHECK_THAT(h.erase(h.cbegin() + start, h.cbegin() + stop), IterEquals(h, start));
    CHECK_THAT(h, ContainsAllOf(std::ranges::subrange(init.begin(), init.begin() + start)) &&
                      ContainsAllOf(std::ranges::subrange(init.begin() + stop, init.end())) &&
                      ContainsNoneOf(std::ranges::subrange(init.begin() + start, init.begin() + stop)));
  }

  SECTION("items can be removed by their key") {
    auto offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto pos = h.begin() + offset;
    auto&& key = key_proj(*pos);
    CHECK(h.erase(key) == 1);
    CHECK_THAT(h, ContainsAllOfExcept(init, *(init.begin() + offset), compare));
  }

  if constexpr (!std::same_as<HeteroProj, int>) {
    SECTION("items can be removed by a compatible key") {
      auto offset = GENERATE_COPY(range(0 * ssize, ssize));
      auto pos = h.begin() + offset;
      auto&& key = key_proj(hetero_proj(*pos));
      CHECK(h.erase(key) == 1);
      CHECK_THAT(h, ContainsAllOfExcept(init, *(init.begin() + offset), compare));
    }
  }

  SECTION("removing items not in container has no effect") {
    auto&& key = GENERATE_COPY(map(key_proj, from_range(extra)));
    CHECK(h.erase(key) == 0);
    CHECK_THAT(h, Equals(init, compare));
  }
}

template <class Hashed, comparator_for<Hashed> Comp = detail::equal_to, class KeyProj = std::identity,
          class HeteroProj = int>
void test_hashed_extract(std::initializer_list<typename Hashed::value_type> init,
                         std::initializer_list<typename Hashed::value_type> extra, Comp compare = {},
                         KeyProj key_proj = {}, HeteroProj hetero_proj = {}) {
  Hashed h = init;
  auto ssize = h.ssize();

  SECTION("items can be extracted by their iterator") {
    auto offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto pos = h.cbegin() + offset;
    auto&& item = *(init.begin() + offset);
    CHECK_THAT(h.extract(pos), Equals(item, compare));
    CHECK_THAT(h, ContainsAllOfExcept(init, item, compare));
  }

  SECTION("items can be removed by their key") {
    auto offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto pos = h.begin() + offset;
    auto&& key = key_proj(*pos);
    auto&& item = *(init.begin() + offset);
    CHECK_THAT(h.extract(key), Equals(item, compare));
    CHECK_THAT(h, ContainsAllOfExcept(init, item, compare));
  }

  if constexpr (!std::same_as<HeteroProj, int>) {
    SECTION("items can be removed by a compatible key") {
      auto offset = GENERATE_COPY(range(0 * ssize, ssize));
      auto pos = h.begin() + offset;
      auto&& key = key_proj(hetero_proj(*pos));
      auto&& item = *(init.begin() + offset);
      CHECK_THAT(h.extract(key), Equals(item, compare));
      CHECK_THAT(h, ContainsAllOfExcept(init, item, compare));
    }
  }

  SECTION("removing items not in container has no effect") {
    auto&& key = GENERATE_COPY(map(key_proj, from_range(extra)));
    CHECK(h.extract(key) == std::nullopt);
    CHECK_THAT(h, Equals(init, compare));
  }
}

}  // namespace testing

FLAT_HASH_NAMESPACE_END
