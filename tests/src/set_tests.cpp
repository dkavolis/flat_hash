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

#include <memory_resource>

#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <catch2/matchers/catch_matchers_container_properties.hpp>
#include <flat_hash/set.hpp>

#include "testing.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace testing {

template <set_traits Traits, ordering_policy Policy, probing::probing_policy Probing>
struct traits : public Traits {
  constexpr static ordering_policy ordering = Policy;
  using probing_policy = Probing;
};

template <class Key, probing::probing_policy Probing, ordering_policy Policy>
using set = ::flat_hash::set<Key, traits<dynamic_set_traits<Key>, Policy, Probing>>;

template <class Key, probing::probing_policy Probing, ordering_policy Policy>
using fixed_set = ::flat_hash::set<Key, traits<fixed_set_traits<Key, 5>, Policy, Probing>>;

template <class Key, probing::probing_policy Probing, ordering_policy Policy>
using inline_set = ::flat_hash::set<Key, traits<inline_set_traits<Key, 64>, Policy, Probing>>;

template <class Key, probing::probing_policy Probing, ordering_policy Policy>
using set_view = ::flat_hash::set<Key, traits<set_view_traits<Key>, Policy, Probing>>;

template <class Key, probing::probing_policy Probing, ordering_policy Policy>
using fixed_set_view = ::flat_hash::set<Key, traits<set_view_traits<Key, 5>, Policy, Probing>>;

namespace pmr {
template <class Key, probing::probing_policy Probing, ordering_policy Policy>
using set = ::flat_hash::set<Key, traits<dynamic_set_traits<Key>, Policy, Probing>>;
}

#define FLAT_HASH_SET_SIGS_1(K, P) (K, P, ordering_policy::preserved), (K, P, ordering_policy::relaxed)
#define FLAT_HASH_SET_SIGS(K)                                                            \
  FLAT_HASH_SET_SIGS_1(K, probing::quadratic), FLAT_HASH_SET_SIGS_1(K, probing::python), \
      FLAT_HASH_SET_SIGS_1(K, probing::robin_hood)
#define FLAT_HASH_DYNAMIC_SETS testing::set, testing::inline_set
#define FLAT_HASH_STATIC_SETS testing::fixed_set
#define FLAT_HASH_SETS FLAT_HASH_DYNAMIC_SETS, FLAT_HASH_STATIC_SETS

#define FLAT_HASH_TEST_CASE_SIG(Name, Tags, Types, ...)                                                       \
  TEMPLATE_PRODUCT_TEST_CASE_SIG(Name, Tags, ((typename K, typename P, ordering_policy O), K, P, O), (Types), \
                                 (FLAT_HASH_SET_SIGS(__VA_ARGS__)))

#define FLAT_HASH_DYNAMIC_SET_TEST_CASE(Name, Tags, ...) \
  FLAT_HASH_TEST_CASE_SIG(Name, Tags, FLAT_HASH_DYNAMIC_SETS, __VA_ARGS__)
#define FLAT_HASH_STATIC_SET_TEST_CASE(Name, Tags, ...) \
  FLAT_HASH_TEST_CASE_SIG(Name, Tags, FLAT_HASH_STATIC_SETS, __VA_ARGS__)
#define FLAT_HASH_SET_TEST_CASE(Name, Tags, ...) FLAT_HASH_TEST_CASE_SIG(Name, Tags, FLAT_HASH_SETS, __VA_ARGS__)

}  // namespace testing

using dynamic_set = testing::set<int, probing::quadratic, ordering_policy::preserved>;
using span_set = testing::set_view<int, probing::quadratic, ordering_policy::preserved>;
using array_set = testing::fixed_set<int, probing::quadratic, ordering_policy::preserved>;
using fixed_span_set = testing::fixed_set_view<int, probing::quadratic, ordering_policy::preserved>;
using tiny_set = testing::inline_set<int, probing::quadratic, ordering_policy::preserved>;

template class set<int>;
template class set<int, inline_set_traits<int, 64>>;

TEST_CASE("Provided set traits satisfy concepts", "[traits][concepts][set]") {
  STATIC_REQUIRE(set_traits<dynamic_set_traits<int>>);
  STATIC_REQUIRE(set_traits_for<dynamic_set_traits<int>, int>);

  STATIC_REQUIRE(set_traits<set_view_traits<int>>);
  STATIC_REQUIRE(set_traits_for<set_view_traits<int>, int>);

  STATIC_REQUIRE(set_traits<fixed_set_traits<int, 5>>);
  STATIC_REQUIRE(set_traits_for<fixed_set_traits<int, 5>, int>);

  STATIC_REQUIRE(set_traits<inline_set_traits<int, 5>>);
  STATIC_REQUIRE(set_traits_for<inline_set_traits<int, 5>, int>);
}

TEST_CASE("Types can be tested if they derive from set or set_iterator", "[concepts][set][derived]") {
  STATIC_REQUIRE(is_set_iterator<set_iterator<int*>>);
  STATIC_REQUIRE(is_set_iterator<set_iterator<int*>&>);
  STATIC_REQUIRE(is_set_iterator<set_iterator<int*> const&>);
  STATIC_REQUIRE(is_set_iterator<set_iterator<int*>&&>);

  STATIC_REQUIRE_FALSE(is_set_iterator<int*>);
  STATIC_REQUIRE_FALSE(is_set_iterator<int*&>);
  STATIC_REQUIRE_FALSE(is_set_iterator<int* const&>);
  STATIC_REQUIRE_FALSE(is_set_iterator<int*&&>);

  STATIC_REQUIRE(is_set<dynamic_set>);
  STATIC_REQUIRE(is_set<span_set>);
  STATIC_REQUIRE(is_set<array_set>);
  STATIC_REQUIRE(is_set<fixed_span_set>);
  STATIC_REQUIRE(is_set<tiny_set>);

  STATIC_REQUIRE_FALSE(is_set<std::vector<int>>);
}

TEST_CASE("set_iterator propagates contiguous_iterator", "[concepts][set][iterator]") {
  STATIC_REQUIRE(std::same_as<std::iter_reference_t<typename dynamic_set::iterator>, int const&>);
  STATIC_REQUIRE(std::same_as<decltype(std::declval<typename dynamic_set::iterator>().operator->()), int const*>);

  STATIC_REQUIRE(std::contiguous_iterator<typename dynamic_set::iterator>);
  STATIC_REQUIRE(std::contiguous_iterator<typename span_set::iterator>);
  STATIC_REQUIRE(std::contiguous_iterator<typename array_set::iterator>);
  STATIC_REQUIRE(std::contiguous_iterator<typename tiny_set::iterator>);
}

FLAT_HASH_DYNAMIC_SET_TEST_CASE("Dynamic sets are constructible", "[set][construct]", int) {
  using set_type = TestType;

  constexpr static std::initializer_list<int> init_a = {0, 1, 2, 3};
  constexpr static std::initializer_list<int> init_b = {4, 5, 6, 7};

  SECTION("using a default constructor") {
    set_type s;

    CHECK_THAT(s, Catch::Matchers::IsEmpty());
    CHECK(s.bucket_count() >= 0);
  }

  SECTION("from a bucket count") {
    set_type s(16);

    CHECK_THAT(s, Catch::Matchers::IsEmpty());
    CHECK(s.bucket_count() >= 16);
  }

  SECTION("from an initializer_list") {
    set_type s{init_a};

    CHECK(s.size() == 4);
    CHECK(s.bucket_count() > s.size());
    CHECK_THAT(s, Equals(init_a) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("from an initializer_list with duplicate values") {
    set_type s{std::initializer_list<int>{0, 1, 2, 3, 3, 2, 0}};

    CHECK(s.size() == 4);
    CHECK(s.bucket_count() > s.size());
    CHECK_THAT(s, Equals(init_a) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("from a key vector") {
    set_type s{std::vector<int>{init_a}};

    CHECK(s.size() == 4);
    CHECK(s.bucket_count() > s.size());
    CHECK_THAT(s, Equals(init_a) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("as a copy") {
    set_type s{init_a};
    set_type copy = s;

    CHECK(s.size() == copy.size());
    CHECK(s.bucket_count() == copy.bucket_count());
    CHECK(s == copy);
    CHECK_THAT(copy, Equals(s) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("can be assigned an initializer list") {
    WHEN("the set is empty") {
      set_type s;
      s = init_a;
      CHECK_THAT(s, Equals(init_a) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
    }

    WHEN("the set is not empty") {
      set_type s{init_b};
      s = init_a;
      CHECK_THAT(s, Equals(init_a) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
    }
  }
}

TEST_CASE("Fixed sets are constructible", "[set][construct][fixed]") {
  using set_type = fixed_set<int, 4>;

  constexpr static std::initializer_list<int> init_a = {0, 1, 2, 3};
  constexpr static std::initializer_list<int> init_b = {4, 5, 6, 7};

  SECTION("from an initializer_list") {
    set_type s{init_a};

    CHECK(s.size() == 4);
    CHECK(s.bucket_count() > s.size());
    CHECK_THAT(s, Equals(init_a) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("from an initializer_list with duplicate values") {
    set_type s{std::initializer_list<int>{0, 1, 2, 3, 3, 2, 0}};

    CHECK(s.size() == 4);
    CHECK(s.bucket_count() > s.size());
    CHECK_THAT(s, Equals(init_a) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("from a key vector") {
    constexpr static std::array<int, 4> arr = {0, 1, 2, 3};
    set_type s{arr};

    CHECK(s.size() == 4);
    CHECK(s.bucket_count() > s.size());
    CHECK_THAT(s, Equals(arr) && ContainsAllOf(arr) && ContainsNoneOf(init_b));
  }

  SECTION("as a copy") {
    set_type s{init_a};
    set_type copy = s;

    CHECK(s.size() == copy.size());
    CHECK(s.bucket_count() == copy.bucket_count());
    CHECK(s == copy);
    CHECK_THAT(copy, Equals(s) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("can be assigned an initializer list") {
    WHEN("the set is not empty") {
      set_type s{init_b};
      s = init_a;
      CHECK_THAT(s, Equals(init_a) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
    }
  }
}

template <class Set>
void test_swap(std::initializer_list<std::ranges::range_value_t<Set>> init_a,
               std::initializer_list<std::ranges::range_value_t<Set>> init_b) {
  Set a{init_a};

  SECTION("swapping with another set swaps contained values") {
    Set b{init_b};

    std::ranges::swap(a, b);

    CHECK_THAT(a, Equals(init_b) && ContainsAllOf(init_b) && ContainsNoneOf(init_a));
    CHECK_THAT(b, Equals(init_a) && ContainsAllOf(init_a) && ContainsNoneOf(init_b));
  }

  SECTION("Swapping with itself has no effect") {
    auto* ptr = std::addressof(a);
    std::ranges::swap(a, a);
    CHECK(std::addressof(a) == ptr);
  }
}

FLAT_HASH_DYNAMIC_SET_TEST_CASE("sets can be swapped", "[set][swap]", int) {
  constexpr static std::initializer_list<int> init_a = {0, 1, 2, 3};
  constexpr static std::initializer_list<int> init_b = {4, 5, 6};

  test_swap<TestType>(init_a, init_b);
}

FLAT_HASH_STATIC_SET_TEST_CASE("sets can be swapped", "[set][swap]", int) {
  constexpr static std::initializer_list<int> init_a = {0, 1, 2, 3, 4};
  constexpr static std::initializer_list<int> init_b = {5, 6, 7, 8, 9};

  test_swap<TestType>(init_a, init_b);
}

TEST_CASE("Allocators are passed to allocator aware containers", "[set][allocator]") {
  using set_type = pmr::set<int>;
  using options_type = set_type::options_type;

  STATIC_REQUIRE(detail::containers::gettable_allocator<typename set_type::key_container>);
  STATIC_REQUIRE(detail::containers::gettable_allocator<typename set_type::index_container>);

  testing::aligned_buffer_t<int, 16> key_storage;
  std::pmr::monotonic_buffer_resource key_resource(&key_storage, sizeof(key_storage));

  testing::aligned_buffer_t<default_index_type, 32> index_storage;
  std::pmr::monotonic_buffer_resource index_resource(&index_storage, sizeof(index_storage));

  options_type options{
      .key_allocator = &key_resource,
      .index_allocator = &index_resource,
  };

  set_type s(options);

  CHECK(s.get_allocator().resource() == &key_resource);
  CHECK(s.get_hash_table_allocator().resource() == &index_resource);
  CHECK(s.table().get_allocator().resource() == &index_resource);
}

TEMPLATE_TEST_CASE("set::max_size returns reasonable values", "[set][max_size]", probing::quadratic, probing::python,
                   probing::robin_hood) {
  testing::set<int, TestType, ordering_policy::preserved> s;

  CHECK(s.max_size() < std::numeric_limits<std::uint32_t>::max());
  CHECK(std::bit_width(s.max_size()) == 32U - s.probing().reserved_bits());
}

FLAT_HASH_DYNAMIC_SET_TEST_CASE("Set lookup tests", "[set][lookup]", std::string) {
  std::initializer_list<std::string> init = {"0", "1", "2", "3", "4", "5"};
  std::initializer_list<std::string> extra = {"6", "7", "8", "9", "10"};

  TestType s = init;

  SECTION("set::contains returns true if key is in the set") {
    auto&& key = GENERATE_COPY(from_range(init));
    SECTION("by key_type") { CHECK(s.contains(key)); }
    SECTION("by compatible key") { CHECK(s.contains(std::string_view(key))); }
  }

  SECTION("set::contains returns false if key is not in the set") {
    auto&& key = GENERATE_COPY(from_range(extra));
    SECTION("by key_type") { CHECK_FALSE(s.contains(key)); }
    SECTION("by compatible key") { CHECK_FALSE(s.contains(std::string_view(key))); }
  }

  SECTION("set::count returns 1 if key is in the set") {
    auto&& key = GENERATE_COPY(from_range(init));
    SECTION("by key_type") { CHECK(s.count(key) == 1); }
    SECTION("by compatible key") { CHECK(s.count(std::string_view(key)) == 1); }
  }

  SECTION("set::count returns 0 if key is not in the set") {
    auto&& key = GENERATE_COPY(from_range(extra));
    SECTION("by key_type") { CHECK(s.count(key) == 0); }
    SECTION("by compatible key") { CHECK(s.count(std::string_view(key)) == 0); }
  }

  SECTION("set::find returns iterator to the found element if key is in the set") {
    auto offset = GENERATE_COPY(range(0, static_cast<int>(init.size())));
    auto& key = *(init.begin() + offset);
    SECTION("by key_type") { CHECK_THAT(s.find(key), IterEquals(s, s.cbegin() + offset)); }
    SECTION("by compatible key") { CHECK_THAT(s.find(std::string_view(key)), IterEquals(s, s.cbegin() + offset)); }
  }

  SECTION("set::find returns iterator to the end if key is not in the set") {
    auto&& key = GENERATE_COPY(from_range(extra));
    SECTION("by key_type") { CHECK_THAT(s.find(key), IterEquals(s, s.cend())); }
    SECTION("by compatible key") { CHECK_THAT(s.find(std::string_view(key)), IterEquals(s, s.cend())); }
  }

  constexpr static auto convert = [](auto&& iter_pair) noexcept {
    return std::ranges::subrange(iter_pair.first, iter_pair.second);
  };

  SECTION("set::equal_range returns range to the found element if key is in the set") {
    auto&& key = GENERATE_COPY(from_range(init));
    SECTION("by key_type") { CHECK_THAT(convert(s.equal_range(key)), Equals(std::views::single(key))); }
    SECTION("by compatible key") {
      CHECK_THAT(convert(s.equal_range(std::string_view(key))), Equals(std::views::single(key)));
    }
  }

  SECTION("set::equal_range returns empty range if key is not in the set") {
    auto&& key = GENERATE_COPY(from_range(extra));
    SECTION("by key_type") { CHECK_THAT(convert(s.equal_range(key)), Catch::Matchers::IsEmpty()); }
    SECTION("by compatible key") {
      CHECK_THAT(convert(s.equal_range(std::string_view(key))), Catch::Matchers::IsEmpty());
    }
  }

  SECTION("set::bucket returns value in set::bucket_count() range if key is in the set") {
    auto&& key = GENERATE_COPY(from_range(init));
    SECTION("by key_type") { CHECK(s.bucket(key) < s.bucket_count()); }
    SECTION("by compatible key") { CHECK(s.bucket(std::string_view(key)) < s.bucket_count()); }
  }

  SECTION("set::bucket returns set::bucket_count() if key is not in the set") {
    auto&& key = GENERATE_COPY(from_range(extra));
    SECTION("by key_type") { CHECK(s.bucket(key) == s.bucket_count()); }
    SECTION("by compatible key") { CHECK(s.bucket(std::string_view(key)) == s.bucket_count()); }
  }

  SECTION("set::bucket_size returns 1 if key is in the set") {
    auto&& key = GENERATE_COPY(from_range(init));
    CHECK(s.bucket_size(s.bucket(key)) == 1);
  }

  SECTION("set::bucket_size returns 0 if key is not in the set") {
    auto iota = std::views::iota(0U, s.bucket_count());
    CHECK(std::ranges::count_if(iota, [&s](auto index) -> bool { return s.bucket_size(index) == 1; }) == s.ssize());
  }
}

FLAT_HASH_DYNAMIC_SET_TEST_CASE("Set tests for contains", "[set][contains]", int) {
  std::initializer_list<int> init = {0, 1, 2, 3};
  std::initializer_list<int> extra = {4, 5, 6};
  TestType s = init;

  CHECK_THAT(s, ContainsAllOf(init) && ContainsNoneOf(extra));
}

FLAT_HASH_DYNAMIC_SET_TEST_CASE("Set tests for contains using heterogeneous lookup", "[set][contains][heterogeneous]",
                                std::string) {
  using namespace std::literals;
  std::initializer_list<std::string> init = {"first", "second", "third"};
  std::initializer_list<std::string_view> extra = {"fourth"sv, "fifth"sv, "sixth"sv};
  TestType s = init;

  CHECK_THAT(s, ContainsAllOf(init | std::views::transform([](auto& str) -> std::string_view { return str; })) &&
                    ContainsNoneOf(extra));
}

TEST_CASE("std::vector based set can have memory reserved", "[set][reserve]") {
  constexpr static std::initializer_list<int> init = {1, 2, 3, 4};
  set<int> s = init;
  CHECK(s.capacity() == 4);
  auto buckets = s.bucket_count();

  SECTION("reserving more will increase capacity and may increase the bucket count") {
    s.reserve(15);
    CHECK(s.capacity() == 15);
    CHECK(s.bucket_count() > buckets);
    CHECK_THAT(s, ContainsAllOf(init));
  }

  SECTION("reserving less than size() has no effect") {
    s.reserve(2);
    CHECK(s.capacity() == 4);
    CHECK(s.bucket_count() == buckets);
    CHECK_THAT(s, ContainsAllOf(init));
  }

  s.reserve(10);
  buckets = s.bucket_count();

  SECTION("reserving less than capacity has no effect on std::vector") {
    s.reserve(5);
    CHECK(s.capacity() == 10);
    CHECK(s.bucket_count() == buckets);
    CHECK_THAT(s, ContainsAllOf(init));
  }
}

FLAT_HASH_DYNAMIC_SET_TEST_CASE("Sets can be mathematically compared", "[set][comparison]", int) {
  TestType a{{0, 1, 2, 3}};
  TestType b{{4, 0, 1, 2, 3}};
  TestType c{{1, 2, 3}};

  SECTION("equality") {
    CHECK(a == TestType{{3, 2, 1, 0}});
    CHECK(a == a);
  }

  SECTION("inequality") {
    CHECK(a != b);
    CHECK(b != a);
    CHECK(a != c);
    CHECK(c != a);
  }

  SECTION("proper subset") {
    CHECK(a < b);
    CHECK(b > a);
    CHECK_FALSE(a > b);
    CHECK_FALSE(b < a);

    CHECK_FALSE(a < a);

    CHECK(a > c);
    CHECK(c < a);
    CHECK_FALSE(a < c);
    CHECK_FALSE(c > a);
  }

  SECTION("subset") {
    CHECK(a <= b);
    CHECK(b >= a);
    CHECK_FALSE(b <= a);
    CHECK_FALSE(a >= b);

    CHECK(a >= a);

    CHECK_FALSE(a <= c);
    CHECK_FALSE(c >= a);
    CHECK(c <= a);
    CHECK(a >= c);
  }
}

struct ConvertibleToInt {
  int a = 0;
  int b = 0;

  [[nodiscard]] constexpr explicit operator int() const noexcept { return a + b; }
};

template <is_set Set, class K>
static void test_splicing(std::initializer_list<typename Set::key_type> init_a, std::initializer_list<K> init_b) {
  Set a = init_a;
  Set b(init_b);

  SECTION("two different sets will result in an empty second set") {
    a.splice(b);

    CHECK_THAT(a, ContainsAllOf(init_a) && ContainsAllOf(init_b));
    CHECK_THAT(b, Catch::Matchers::IsEmpty());
  }

  SECTION("duplicate values will be left in the second set") {
    auto ssize = std::ranges::ssize(init_a);
    auto offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto subvalues = std::ranges::subrange(init_a.begin(), init_a.begin() + offset);
    b.merge(subvalues);

    a.splice(b);
    CHECK_THAT(a, ContainsAllOf(init_a) && ContainsAllOf(init_b));
    CHECK_THAT(b, ContainsAllOf(subvalues) && ContainsNoneOf(init_b));
  }
}

FLAT_HASH_DYNAMIC_SET_TEST_CASE("Sets can be mutated", "[set][mutation]", std::string) {
  using namespace std::literals;
  static std::initializer_list<std::string> values = {"-2", "8", "2", "3", "10", "5", "6", "-1", "17"};
  static std::initializer_list<std::string_view> extra = {"-100", "-10", "-5", "-6"};

  TestType a{values};

  SECTION("new values can be inserted") {
    auto value = GENERATE_COPY(from_range(extra));
    std::string str(value);
    SECTION("with set::insert") {
      auto [it, inserted] = a.insert(std::move(str));
      CHECK(*it == value);
      CHECK(inserted);
      CHECK_THAT(it, IterEquals(a, a.begin() + a.ssize() - 1));
      CHECK(a.size() == values.size() + 1);

      CHECK_THAT(a, ContainsAllOf(values) && Contains(value));
    }

    SECTION("heterogeneous values with set::insert") {
      auto [it, inserted] = a.insert(value);
      CHECK(*it == value);
      CHECK(inserted);
      CHECK_THAT(it, IterEquals(a, a.begin() + a.ssize() - 1));
      CHECK_THAT(a, ContainsAllOf(values) && Contains(value));
    }

    SECTION("with set::emplace") {
      auto [it, inserted] = a.emplace(value);
      CHECK(*it == value);
      CHECK(inserted);
      CHECK_THAT(it, IterEquals(a, a.begin() + a.ssize() - 1));
      CHECK_THAT(a, ContainsAllOf(values) && Contains(value));
    }

    SECTION("at specific positions") {
      auto ssize = a.ssize();
      auto offset = GENERATE_COPY(range(ssize * 0, ssize));

      SECTION("with set::insert") {
        auto iter = a.insert(a.begin() + offset, std::move(str));
        CHECK(*iter == value);
        CHECK_THAT(iter, IterEquals(a, a.begin() + offset));
        CHECK_THAT(a, ContainsAllOf(values) && Contains(value));
      }

      SECTION("with heterogeneous set::insert") {
        auto iter = a.insert(a.begin() + offset, value);
        CHECK(*iter == value);
        CHECK_THAT(iter, IterEquals(a, a.begin() + offset));
        CHECK_THAT(a, ContainsAllOf(values) && Contains(value));
      }

      SECTION("with set::emplace_hint") {
        auto iter = a.emplace_hint(a.begin() + offset, value);
        CHECK(*iter == value);
        CHECK_THAT(iter, IterEquals(a, a.begin() + offset));
        CHECK_THAT(a, ContainsAllOf(values) && Contains(value));
      }
    }
  }

  SECTION(
      "inserting contained values doesn't change the set and returns the iterator to the element preventing "
      "insertion") {
    auto ssize = a.ssize();
    auto& str = GENERATE_COPY(from_range(values));
    auto [iter, inserted] = a.insert(str);
    CHECK_FALSE(inserted);
    CHECK(*iter == str);
    CHECK_THAT(a, Equals(values));

    auto offset = GENERATE_COPY(range(ssize * 0, ssize));
    a.insert(a.begin() + offset, str);
    CHECK_THAT(a, Equals(values));
  }

  SECTION("ranges of values can be inserted") {
    WHEN("from an initializer_list") {
      a.insert(extra);
      CHECK_THAT(a, ContainsAllOf(values) && ContainsAllOf(extra));
    }

    WHEN("from a pair of iterators") {
      a.insert(extra.begin(), extra.end());
      CHECK_THAT(a, ContainsAllOf(values) && ContainsAllOf(extra));
    }
  }

  if constexpr (!detail::static_sized<typename TestType::traits_type::index_container>) {
    SECTION("hash table expands when load factor is reached") {
      auto old = a.bucket_count();
      CHECK(a.bucket_count() >= 16);

      a.insert(extra);

      CHECK(a.bucket_count() > old);
      CHECK(a.bucket_count() >= 32);
      CHECK(a.size() == values.size() + extra.size());

      CHECK_THAT(a, ContainsAllOf(values) && ContainsAllOf(extra));
    }
  }

  SECTION("values can be removed") {
    auto const& i = GENERATE_COPY(from_range(values));

    SECTION("with set::erase") {
      CHECK(a.erase(i) == 1);
      CHECK_THAT(a, ContainsAllOf(a) && !Contains(i));
    }

    SECTION("with heterogeneous set::erase") {
      std::string_view str = i;
      CHECK(a.erase(str) == 1);

      CHECK_THAT(a, ContainsAllOf(a) && !Contains(str));
    }

    SECTION("by their iterator") {
      auto ssize = a.ssize();
      auto offset = GENERATE_COPY(range(ssize * 0, ssize));

      auto erased = *(a.begin() + offset);
      auto iter = a.erase(a.begin() + offset);
      CHECK(iter == a.begin() + offset);
      CHECK_THAT(a, ContainsAllOf(a) && !Contains(erased));
    }
  }

  SECTION("removing values not in set has no effect") {
    auto& value = GENERATE_COPY(from_range(extra));
    CHECK(a.erase(std::string(value)) == 0);
    CHECK_THAT(a, Equals(values));

    CHECK(a.erase(value) == 0);
    CHECK_THAT(a, Equals(values));
  }

  SECTION("ranges of values can be erased") {
    auto ssize = a.ssize();
    auto start = GENERATE_COPY(range(ssize * 0, ssize));
    auto stop = GENERATE_COPY(range(start, ssize));

    auto iter = a.erase(a.cbegin() + start, a.cbegin() + stop);
    CHECK_THAT(iter, IterEquals(a, a.begin() + start));
    CHECK_THAT(a, ContainsAllOf(std::ranges::subrange(values.begin(), values.begin() + start)) &&
                      ContainsAllOf(std::ranges::subrange(values.begin() + stop, values.end())));
  }

  SECTION("values can be removed based on a predicate") {
    constexpr static auto pred = [](std::string_view s) { return s.size() % 2 == 0; };
    erase_if(a, pred);
    CHECK_THAT(a, ContainsAllOf(values | std::views::filter([](std::string_view v) { return !pred(v); })));
  }

  SECTION("erase_if with false predicate will not change the set") {
    constexpr static auto pred = [](std::string_view) { return false; };
    erase_if(a, pred);
    CHECK_THAT(a, Equals(values));
  }

  SECTION("sets can be cleared") {
    a.clear();
    CHECK_THAT(a, Catch::Matchers::IsEmpty());
  }

  SECTION("values can be extracted") {
    auto ssize = a.ssize();
    auto offset = GENERATE_COPY(range(0 * ssize, ssize));
    auto pos = a.begin() + offset;
    auto expected = *pos;

    SECTION("by key") {
      CHECK(a.extract(expected) == expected);
      CHECK_THAT(a, ContainsAllOf(values | std::views::filter([expected](auto&& v) { return v != expected; })) &&
                        !Contains(expected));
    }

    SECTION("by heterogeneous key") {
      std::string_view key = expected;
      CHECK(a.extract(key) == expected);
      CHECK_THAT(a, ContainsAllOf(values | std::views::filter([expected](auto&& v) { return v != expected; })) &&
                        !Contains(expected));
    }

    SECTION("by iterator") {
      // a is a copy so pos is invalid
      CHECK(a.extract(a.begin() + offset) == expected);
      CHECK_THAT(a, ContainsAllOf(values | std::views::filter([expected](auto&& v) { return v != expected; })) &&
                        !Contains(expected));
    }
  }

  SECTION("sets can be merged") {
    SECTION("with other sets") {
      TestType b(extra);
      b.insert(values);
      a.merge(b);
      CHECK(a.size() == values.size() + extra.size());
      CHECK_THAT(a, ContainsAllOf(values) && ContainsAllOf(extra));
    }

    SECTION("with unique ranges") {
      a.merge(std::ranges::ref_view{extra});
      CHECK(a.size() == values.size() + extra.size());
      CHECK_THAT(a, ContainsAllOf(values) && ContainsAllOf(extra));
    }

    SECTION("with duplicate ranges") {
      std::vector b = extra;
      b.insert(b.cend(), extra.begin(), extra.end());
      a.merge(b);
      CHECK(a.size() == values.size() + extra.size());
      CHECK_THAT(a, ContainsAllOf(values) && ContainsAllOf(extra));
    }
  }

  SECTION("sets can be spliced") { test_splicing<TestType>(values, extra); }
}

TEST_CASE("Sets can be formatted", "[set][format]") {
  STATIC_REQUIRE(detail::formattable<int, char>);
  dynamic_set a{0, 1, 2, 3};

  CHECK(FLAT_HASH_FORMAT_NS format("{}", a) == "{0, 1, 2, 3}");
  CHECK(FLAT_HASH_FORMAT_NS format("{:}", a) == "{0, 1, 2, 3}");
  CHECK(FLAT_HASH_FORMAT_NS format("{:l}", a) == "{\n\t0,\n\t1,\n\t2,\n\t3,\n}");
  CHECK(FLAT_HASH_FORMAT_NS format("{::d}", a) == "{0, 1, 2, 3}");
  CHECK(FLAT_HASH_FORMAT_NS format("{::02d}", a) == "{00, 01, 02, 03}");
  CHECK(FLAT_HASH_FORMAT_NS format("{:l:02d}", a) == "{\n\t00,\n\t01,\n\t02,\n\t03,\n}");

  using namespace std::literals;
  set s = {"1"sv, "42"sv};

  CHECK(FLAT_HASH_FORMAT_NS format("{}", s) == "{\"1\", \"42\"}");
  CHECK(FLAT_HASH_FORMAT_NS format("{:}", s) == "{\"1\", \"42\"}");
  CHECK(FLAT_HASH_FORMAT_NS format("{:l}", s) == "{\n\t\"1\",\n\t\"42\",\n}");
  CHECK(FLAT_HASH_FORMAT_NS format("{::s}", s) == "{\"1\", \"42\"}");
  CHECK(FLAT_HASH_FORMAT_NS format("{::>2s}", s) == "{\" 1\", \"42\"}");
}

TEST_CASE("Sets are convertible to equivalent sets", "[set][convertible]") {
  constexpr static std::initializer_list<int> values = {0, 1, 4, 6, 10};

  dynamic_set vector{values};
  array_set array{values};
  tiny_set small{values};
  constexpr auto implicitly = [](auto& s) noexcept -> span_set { return s; };
  constexpr auto add_const = []<class T>(T& v) noexcept -> T const& { return v; };

  SECTION("dynamic sets can be implicitly converted to dynamic set views") {
    WHEN("using std::vector based set") {
      STATIC_REQUIRE(std::constructible_from<span_set, dynamic_set&>);
      STATIC_REQUIRE(std::constructible_from<span_set, dynamic_set const&>);
      STATIC_REQUIRE(std::convertible_to<dynamic_set&, span_set>);
      STATIC_REQUIRE(std::convertible_to<dynamic_set const&, span_set>);

      span_set view = implicitly(vector);
      CHECK_THAT(view, Equals(vector) && Equals(values) && ContainsAllOf(values));
      CHECK_THAT(implicitly(add_const(vector)), Equals(values) && ContainsAllOf(values));
    }

    WHEN("using inline set") {
      STATIC_REQUIRE(std::constructible_from<span_set, tiny_set&>);
      STATIC_REQUIRE(std::constructible_from<span_set, tiny_set const&>);
      STATIC_REQUIRE(std::convertible_to<tiny_set&, span_set>);
      STATIC_REQUIRE(std::convertible_to<tiny_set const&, span_set>);

      span_set view = implicitly(small);
      CHECK_THAT(view, Equals(small) && Equals(values) && ContainsAllOf(values));
      CHECK_THAT(implicitly(add_const(small)), Equals(values) && ContainsAllOf(values));
    }
  }

  SECTION("dynamic sets can be explicitly converted to static set views") {
    WHEN("using std::vector based set") {
      STATIC_REQUIRE(std::constructible_from<fixed_span_set, dynamic_set&>);
      STATIC_REQUIRE(std::constructible_from<fixed_span_set, dynamic_set const&>);
      STATIC_REQUIRE_FALSE(std::convertible_to<dynamic_set&, fixed_span_set>);
      STATIC_REQUIRE_FALSE(std::convertible_to<dynamic_set const&, fixed_span_set>);

      auto view = static_cast<fixed_span_set>(vector);
      CHECK_THAT(view, Equals(vector) && Equals(values) && ContainsAllOf(values));
      CHECK_THAT(static_cast<fixed_span_set>(add_const(vector)), Equals(values) && ContainsAllOf(values));
    }

    WHEN("using inline set") {
      STATIC_REQUIRE(std::constructible_from<fixed_span_set, tiny_set&>);
      STATIC_REQUIRE(std::constructible_from<fixed_span_set, tiny_set const&>);
      STATIC_REQUIRE_FALSE(std::convertible_to<tiny_set&, fixed_span_set>);
      STATIC_REQUIRE_FALSE(std::convertible_to<tiny_set const&, fixed_span_set>);

      auto view = static_cast<fixed_span_set>(small);
      CHECK_THAT(view, Equals(small) && Equals(values) && ContainsAllOf(values));
      CHECK_THAT(static_cast<fixed_span_set>(add_const(small)), Equals(values) && ContainsAllOf(values));
    }
  }

  SECTION("static sets can be implicitly converted to dynamic set views") {
    STATIC_REQUIRE(std::constructible_from<span_set, array_set&>);
    STATIC_REQUIRE(std::constructible_from<span_set, array_set const&>);
    STATIC_REQUIRE(std::convertible_to<array_set&, span_set>);
    STATIC_REQUIRE(std::convertible_to<array_set const&, span_set>);
    span_set view = implicitly(array);
    CHECK_THAT(view, Equals(array) && Equals(values) && ContainsAllOf(values));
    CHECK_THAT(implicitly(add_const(array)), Equals(values) && ContainsAllOf(values));
  }

  SECTION("static sets can be implicitly converted to static set views") {
    STATIC_REQUIRE(std::constructible_from<fixed_span_set, array_set&>);
    STATIC_REQUIRE(std::constructible_from<fixed_span_set, array_set const&>);
    STATIC_REQUIRE(std::convertible_to<array_set&, fixed_span_set>);
    STATIC_REQUIRE(std::convertible_to<array_set const&, fixed_span_set>);

    constexpr auto sized_implicitly = [](auto& s) noexcept -> fixed_span_set { return s; };
    auto view = sized_implicitly(array);
    CHECK_THAT(view, Equals(array) && Equals(values) && ContainsAllOf(values));
    CHECK_THAT(sized_implicitly(add_const(array)), Equals(values) && ContainsAllOf(values));
  }
}

struct bad_hash {
  [[nodiscard]] constexpr auto operator()(int v) const noexcept -> std::size_t { return static_cast<std::size_t>(v); }
};

template <ordering_policy O, class P>
struct collision_traits : dynamic_set_traits<int> {
  constexpr static ordering_policy ordering = O;
  using probing_policy = P;
  using hasher = bad_hash;
};

template <class Key, probing::probing_policy Probing, ordering_policy Policy>
using collision_set = set<Key, collision_traits<Policy, Probing>>;

FLAT_HASH_TEST_CASE_SIG("Sets remain valid even with lots of collisions", "[set][collisions]", collision_set, int) {
  typename TestType::options_type options{};
  if constexpr (std::same_as<typename TestType::probing_policy, probing::robin_hood>) {
    options.policy = probing::robin_hood(2);  // use small value to trigger width change
  }

  TestType s(4, options);  // construct with 4 buckets
  REQUIRE(s.bucket_count() == 4);

  constexpr static std::initializer_list<int> values = {
      0, 16, 32, 48, 64, 80, 96, 112, 128, 144, 160,
  };
  constexpr static auto ssize = static_cast<int>(values.size());

  SECTION("inserting all colliding values doesn't invalidate set state") {
    auto offset = GENERATE(range(1, ssize));
    auto subvalues = values | std::views::take(offset);
    s.insert(subvalues);
    CHECK_THAT(s, Equals(subvalues) && ContainsAllOf(subvalues));
  }

  SECTION("removing colliding values doesn't invalidate set state") {
    auto stop = GENERATE(range(1, ssize));
    auto start = GENERATE_COPY(range(0, stop));
    auto subvalues = values | std::views::take(stop);
    s.insert(subvalues);
    s.erase(s.begin() + start, s.begin() + stop);

    // std::views::drop uses throwing path in GCC 11.1 which doesn't compile without exceptions
    CHECK_THAT(s, ContainsAllOf(values | std::views::take(start)) &&
                      ContainsNoneOf(std::ranges::subrange(values.begin() + stop, values.end())));
  }
}

TEMPLATE_TEST_CASE("set::rehash can be used to increase the number of buckets", "[set][rehash]", probing::quadratic,
                   probing::python, probing::robin_hood) {
  constexpr static std::initializer_list values = {1, 2, 3, 4, 5};
  SECTION("std::vector sets increase the number of buckets") {
    testing::set<int, TestType, ordering_policy::preserved> s = values;
    CHECK(s.bucket_count() == 8);
    s.rehash(32);
    CHECK(s.bucket_count() == 32);
    CHECK_THAT(s, Equals(values));
  }

  SECTION("rehash has no observable effect on valid inline sets") {
    testing::inline_set<int, TestType, ordering_policy::preserved> s = values;
    CHECK(s.bucket_count() == 128);
    s.rehash(32);
    CHECK(s.bucket_count() == 128);
    CHECK_THAT(s, Equals(values));
  }
}

FLAT_HASH_NAMESPACE_END
