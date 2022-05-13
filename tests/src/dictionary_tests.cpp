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
#include <flat_hash/dictionary.hpp>

#include "hashed_tests.hpp"
#include "testing.hpp"

FLAT_HASH_NAMESPACE_BEGIN

constexpr inline std::size_t InlineSize = 10;
constexpr inline std::size_t StaticSize = 4;
using key_type = std::string;
using mapped_type = std::string;
using hetero_key_type = std::string_view;

struct pair_equal {
  // pair operator<=> is not picked up or is not implemented yet...
  template <class T1, class T2, class U1, class U2>
  [[nodiscard]] constexpr auto operator()(std::pair<T1, T2> const& lhs, std::pair<U1, U2> const& rhs) const -> bool {
    detail::equal_to comp{};
    return comp(lhs.first, rhs.first) && comp(lhs.first, lhs.first);
  }
};

struct key_proj {
  template <detail::pair_like P>
  [[nodiscard]] constexpr auto operator()(P&& pair) const noexcept {
    // returning value since key is const qualified
    return std::get<0>(std::forward<P>(pair));
  }
};

struct hetero_value_proj {
  template <detail::pair_like P>
  [[nodiscard]] constexpr auto operator()(P&& pair) const noexcept {
    return std::make_pair(hetero_key_type(std::get<0>(std::forward<P>(pair))), std::get<1>(std::forward<P>(pair)));
  }
};

struct mutable_proj {
  template <class T1, class T2>
  [[nodiscard]] constexpr auto operator()(std::pair<T1, T2> const& pair) const
      -> std::pair<std::remove_cvref_t<T1>, std::remove_cvref_t<T2>> {
    return {pair.first, pair.second};
  }
};

[[nodiscard]] static auto testing_lists() {
  using value_type = std::pair<key_type const, mapped_type>;
  static std::initializer_list<value_type> a = {{"1", "11"}, {"2", "22"}, {"3", "33"}, {"4", "44"}};
  static std::initializer_list<value_type> duplicates = {{"1", "11"}, {"2", "22"}, {"1", "11"},
                                                         {"3", "33"}, {"2", "22"}, {"4", "44"}};
  static std::initializer_list<value_type> b = {{"5", "55"}, {"6", "66"}, {"7", "77"}, {"8", "88"}};

  return std::make_tuple(a, b, duplicates);
}

[[nodiscard]] static auto value_type_args() {
  using namespace std::literals;
  return std::make_tuple("10"sv, "1010"sv);
}

namespace testing {

template <dictionary_traits Traits, ordering_policy Policy, probing::probing_policy Probing>
struct traits : public Traits {
  constexpr static ordering_policy ordering = Policy;
  using probing_policy = Probing;
};

template <class Key, class Value, probing::probing_policy Probing, ordering_policy Policy>
using dictionary = ::flat_hash::dictionary<Key, Value, traits<dynamic_dictionary_traits<Key, Value>, Policy, Probing>>;

template <class Key, class Value, probing::probing_policy Probing, ordering_policy Policy>
using fixed_dictionary =
    ::flat_hash::dictionary<Key, Value, traits<fixed_dictionary_traits<Key, Value, StaticSize>, Policy, Probing>>;

template <class Key, class Value, probing::probing_policy Probing, ordering_policy Policy>
using inline_dictionary =
    ::flat_hash::dictionary<Key, Value, traits<inline_dictionary_traits<Key, Value, InlineSize>, Policy, Probing>>;

template <class Key, class Value, probing::probing_policy Probing, ordering_policy Policy>
using dictionary_view =
    ::flat_hash::dictionary<Key, Value, traits<dictionary_view_traits<Key, Value>, Policy, Probing>>;

template <class Key, class Value, probing::probing_policy Probing, ordering_policy Policy>
using fixed_dictionary_view =
    ::flat_hash::dictionary<Key, Value, traits<dictionary_view_traits<Key, Value, StaticSize>, Policy, Probing>>;

namespace pmr {
template <class Key, class Value, probing::probing_policy Probing, ordering_policy Policy>
using dictionary =
    ::flat_hash::dictionary<Key, Value,
                            traits<::flat_hash::pmr::dynamic_dictionary_traits<Key, Value>, Policy, Probing>>;
}

#define FLAT_HASH_DICTIONARY_MAKE_SIGS_FULL(K, V, P) \
  (K, V, P, ordering_policy::preserved), (K, V, P, ordering_policy::relaxed)
#define FLAT_HASH_DICTIONARY_MAKE_SIGS_NO_ORDER(K, V, P) (K, V, P, ordering_policy::preserved)

#define FLAT_HASH_DICTIONARY_MAKE_SIGS(Macro, K, V) \
  Macro(K, V, probing::quadratic), Macro(K, V, probing::python), Macro(K, V, probing::robin_hood)
#define FLAT_HASH_DICTIONARY_SIGS(K, V) FLAT_HASH_DICTIONARY_MAKE_SIGS(FLAT_HASH_DICTIONARY_MAKE_SIGS_FULL, K, V)
#define FLAT_HASH_DICTIONARY_SIGS_NO_ORDER(K, V) \
  FLAT_HASH_DICTIONARY_MAKE_SIGS(FLAT_HASH_DICTIONARY_MAKE_SIGS_NO_ORDER, K, V)

#define FLAT_HASH_DYNAMIC_DICTIONARIES testing::dictionary, testing::inline_dictionary
#define FLAT_HASH_STATIC_DICTIONARIES testing::fixed_dictionary
#define FLAT_HASH_DICTIONARIES FLAT_HASH_DYNAMIC_DICTIONARIES, FLAT_HASH_STATIC_DICTIONARIES

#define FLAT_HASH_TEST_CASE_SIG(Name, Tags, Types, ...)                                                             \
  TEMPLATE_PRODUCT_TEST_CASE_SIG(Name, Tags, ((typename K, typename V, typename P, ordering_policy O), K, V, P, O), \
                                 (Types), (__VA_ARGS__))

#define FLAT_HASH_DYNAMIC_DICTIONARY_TEST_CASE(Name, Tags, ...) \
  FLAT_HASH_TEST_CASE_SIG(Name, Tags, FLAT_HASH_DYNAMIC_DICTIONARIES, FLAT_HASH_DICTIONARY_SIGS(__VA_ARGS__))
#define FLAT_HASH_STATIC_DICTIONARY_TEST_CASE(Name, Tags, ...) \
  FLAT_HASH_TEST_CASE_SIG(Name, Tags, FLAT_HASH_STATIC_DICTIONARIES, FLAT_HASH_DICTIONARY_SIGS(__VA_ARGS__))
#define FLAT_HASH_DICTIONARY_TEST_CASE(Name, Tags, ...) \
  FLAT_HASH_TEST_CASE_SIG(Name, Tags, FLAT_HASH_DICTIONARIES, FLAT_HASH_DICTIONARY_SIGS(__VA_ARGS__))

}  // namespace testing

using dynamic_dictionary = testing::dictionary<key_type, mapped_type, probing::quadratic, ordering_policy::preserved>;
using span_dictionary = testing::dictionary_view<key_type, mapped_type, probing::quadratic, ordering_policy::preserved>;
using array_dictionary =
    testing::fixed_dictionary<key_type, mapped_type, probing::quadratic, ordering_policy::preserved>;
using fixed_span_dictionary =
    testing::fixed_dictionary_view<key_type, mapped_type, probing::quadratic, ordering_policy::preserved>;
using tiny_dictionary =
    testing::inline_dictionary<key_type, mapped_type, probing::quadratic, ordering_policy::preserved>;

template class dictionary<key_type, mapped_type>;
template class dictionary<key_type, mapped_type, inline_dictionary_traits<key_type, mapped_type, InlineSize>>;

FLAT_HASH_DYNAMIC_DICTIONARY_TEST_CASE("Dynamic dictionaries are constructible", "[dictionary][dynamic][construct]",
                                       key_type, mapped_type) {
  using dict = TestType;
  auto [a, b, duplicates] = testing_lists();
  testing::test_hashed_constructors<dict, true>(a, b, duplicates, pair_equal{});
}

FLAT_HASH_STATIC_DICTIONARY_TEST_CASE("Static dictionaries are constructible", "[dictionary][static][construct]",
                                      key_type, mapped_type) {
  using dict = TestType;
  auto [a, b, duplicates] = testing_lists();
  testing::test_hashed_constructors<dict, false>(a, b, duplicates, pair_equal{});
}

FLAT_HASH_DICTIONARY_TEST_CASE("Dictionaries can be swapped", "[dictionary][swap]", key_type, mapped_type) {
  using dict = TestType;
  auto [a, b, duplicates] = testing_lists();
  testing::test_hashed_swap<dict>(a, b, pair_equal{});
}

#define FH_MAKE_RESOURCE(NAME, SIZE, ...)                                           \
  STATIC_REQUIRE(detail::containers::gettable_allocator<__VA_ARGS__>);              \
  alignas(std::ranges::range_value_t<__VA_ARGS__>)                                  \
      std::array<std::byte, sizeof(std::ranges::range_value_t<__VA_ARGS__>) * SIZE> \
          NAME##_storage;                                                           \
  std::pmr::monotonic_buffer_resource NAME(NAME##_storage.data(), sizeof(NAME##_storage))

TEST_CASE("Dictionary propagates allocators to containers", "[dictionary][allocators]") {
  using dict_t = pmr::dictionary<key_type, mapped_type>;
  using options_t = dict_t::options_type;

  STATIC_REQUIRE(std::same_as<std::pmr::vector<key_type>, typename dict_t::key_container>);
  STATIC_REQUIRE(std::same_as<std::pmr::vector<mapped_type>, typename dict_t::value_container>);
  STATIC_REQUIRE(std::same_as<std::pmr::vector<default_index_type>, typename dict_t::index_container>);

  FH_MAKE_RESOURCE(key_resource, StaticSize, typename dict_t::key_container);
  FH_MAKE_RESOURCE(mapped_resource, StaticSize, typename dict_t::value_container);
  FH_MAKE_RESOURCE(index_resource, default_table_size_for(StaticSize), typename dict_t::index_container);

  options_t options{
      .key_allocator = &key_resource,
      .value_allocator = &mapped_resource,
      .index_allocator = &index_resource,
  };

  dict_t s(options);
  CHECK(s.get_allocator().resource() == &mapped_resource);
  CHECK(s.get_keys_allocator().resource() == &key_resource);
  CHECK(s.get_hash_table_allocator().resource() == &index_resource);
  CHECK(s.table().get_allocator().resource() == &index_resource);
}

TEMPLATE_TEST_CASE("dictionary::max_size returns reasonable values", "[dictionary][max_size]", probing::quadratic,
                   probing::python, probing::robin_hood) {
  using dict_t = testing::dictionary<key_type, mapped_type, TestType, ordering_policy::preserved>;
  testing::test_hashed_max_size<dict_t>();
}

// ordering has no effect on lookup once container is constructed
FLAT_HASH_TEST_CASE_SIG("Dictionary lookup tests", "[dictionary][lookup]", FLAT_HASH_DICTIONARIES,
                        FLAT_HASH_DICTIONARY_SIGS_NO_ORDER(key_type, mapped_type)) {
  auto [a, b, dups] = testing_lists();
  testing::test_hashed_lookup<TestType, hetero_key_type, true>(a, b, pair_equal{}, key_proj{});

  TestType dict = a;
  SECTION("at returns the mapped reference") {
    auto&& pair = GENERATE_COPY(from_range(a));
    SECTION("with key_type") {
      CHECK(dict.at(pair.first) == pair.second);
      CHECK(testing::add_const(dict).at(pair.first) == pair.second);
    }

    SECTION("with heterogeneous key") {
      hetero_key_type key(pair.first);
      CHECK(dict.at(key) == pair.second);
      CHECK(testing::add_const(dict).at(key) == pair.second);
    }
  }

#ifdef __cpp_exceptions
  SECTION("at throws when the key is not found") {
    auto&& pair = GENERATE_COPY(from_range(b));
    SECTION("with key_type") {
      CHECK_THROWS_AS(dict.at(pair.first), std::out_of_range);
      CHECK_THROWS_AS(testing::add_const(dict).at(pair.first), std::out_of_range);
    }

    SECTION("with heterogeneous key") {
      hetero_key_type key(pair.first);
      CHECK_THROWS_AS(dict.at(key), std::out_of_range);
      CHECK_THROWS_AS(testing::add_const(dict).at(key), std::out_of_range);
    }
  }
#endif

  if constexpr (detail::containers::resizable<typename TestType::key_container> &&
                detail::containers::resizable<typename TestType::value_container>) {
    SECTION("subscript operator returns the mapped reference when the key is found") {
      auto&& pair = GENERATE_COPY(from_range(a));
      SECTION("with key_type") { CHECK(dict[pair.first] == pair.second); }

      SECTION("with heterogeneous key") {
        hetero_key_type key(pair.first);
        CHECK(dict[key] == pair.second);
      }
    }

    SECTION("subscript operator returns default constructed reference when the key is not found") {
      mapped_type default_value{};
      auto&& pair = GENERATE_COPY(from_range(b));
      SECTION("with key_type") { CHECK(dict[pair.first] == default_value); }
      SECTION("with rvalue key_type") {
        typename TestType::key_type key = pair.first;
        CHECK(dict[std::move(key)] == default_value);
      }

      SECTION("with heterogeneous key") {
        hetero_key_type key(pair.first);
        CHECK(dict[key] == default_value);
      }
    }
  }
}

TEST_CASE("Dictionaries can be formatted", "[dictionary][format]") {
  using FLAT_HASH_FORMAT_NS format;
  using dict_t = inline_dictionary<int, int, InlineSize>;
  using pair_t = std::pair<typename dict_t::iterator, bool>;

  dict_t dict = {
      {1, 42},
      {2, -42},
  };

  CHECK(format("{}", dict) == "{1: 42, 2: -42}");
  CHECK(format("{:}", dict) == "{1: 42, 2: -42}");
  CHECK(format("{:l}", dict) == "{\n\t1: 42,\n\t2: -42,\n}");
  CHECK(format("{::{:02d} -> {:+02d}}", dict) == "{01 -> +42, 02 -> -42}");
  CHECK(format("{::{}{}}", dict) == "{1: 42, 2: -42}");
  CHECK(format("{::{}}", dict) == "{1: 42, 2: -42}");
  CHECK(format("{::{} -> {}}", dict) == "{1 -> 42, 2 -> -42}");
  CHECK(format("{::{} -> }", dict) == "{1 -> 42, 2 -> -42}");
  CHECK(format("{:: -> }", dict) == "{1 -> 42, 2 -> -42}");

#if defined(__cpp_exceptions) && defined(FLAT_HASH_USE_FMTLIB)
  // would need to use std::vformat to achieve runtime format strings, fmt is just easier
  // paths taken are independent of format library used anyway
  CHECK_THROWS_AS(fmt::format(fmt::runtime("{::{ }{}}")), fmt::format_error);
  CHECK_THROWS_AS(fmt::format(fmt::runtime("{::{}{ }}")), fmt::format_error);
  CHECK_THROWS_AS(fmt::format(fmt::runtime("{::{}{} }")), fmt::format_error);
  CHECK_THROWS_AS(fmt::format(fmt::runtime("{:: {}{}}")), fmt::format_error);
#endif
}

TEST_CASE("Dictionary values can be accessed by iterator operator->", "[dictionary][iterator]") {
  inline_dictionary<int, int, InlineSize> dict{{1, 42}};

  auto iter = dict.begin();
  STATIC_REQUIRE(std::same_as<decltype(iter->first), int const&>);
  STATIC_REQUIRE(std::same_as<decltype(iter->second), int&>);
  CHECK(iter->first == 1);
  CHECK(iter->second == 42);
}

TEST_CASE("Dictionary can have memory reserved", "[dictionary][reserve]") {
  testing::test_hashed_reserve<dictionary<key_type, mapped_type>>(std::get<0>(testing_lists()));
}

FLAT_HASH_DYNAMIC_DICTIONARY_TEST_CASE("Dictionaries can be cleared", "[dictionary][clear]", key_type, mapped_type) {
  testing::test_hashed_clear<TestType>(std::get<0>(testing_lists()));
}

FLAT_HASH_DYNAMIC_DICTIONARY_TEST_CASE("Values can be inserted into dictionaries", "[dictionary][insert]", key_type,
                                       mapped_type) {
  auto [a, b, dups] = testing_lists();
  testing::test_hashed_insert<TestType>(a, b, pair_equal{}, hetero_value_proj{});
}

FLAT_HASH_DYNAMIC_DICTIONARY_TEST_CASE("Values can be emplaced into dictionaries", "[dictionary][emplace]", key_type,
                                       mapped_type) {
  auto [init, extra, dups] = testing_lists();
  auto [key, value] = value_type_args();
  testing::test_hashed_emplace<TestType>(init, key, value);

  TestType h = init;
  auto ssize = h.ssize();
  SECTION("try_emplace inserts new elements at the end") {
    CHECK_THAT(h.try_emplace(key, value), PairMatches(IterEquals(h, -1), IsTrue()));
    CHECK_THAT(h, Contains(std::make_pair(key, value)));
  }

  SECTION("try_emplace returns an iterator to the element preventing insertion and does not move its arguments") {
    auto [k, v] = mutable_proj{}(*init.begin());
    CHECK_THAT(h.try_emplace(std::move(k), std::move(v)), PairMatches(IterEquals(h, 0), IsFalse()));
    CHECK(k == init.begin()->first);
    CHECK(v == init.begin()->second);
  }

  SECTION("try_emplace inserts new elements at the specified position") {
    auto offset = GENERATE_COPY(range(0 * ssize, ssize));
    CHECK_THAT(h.try_emplace(h.cbegin() + offset, key, value), IterEquals(h, offset));
    CHECK_THAT(h, Contains(std::make_pair(key, value)));
  }

  SECTION(
      "try_emplace with a specified positions returns an iterator to the element preventing insertion and does not "
      "move its arguments") {
    auto [k, v] = mutable_proj{}(*init.begin());
    auto offset = GENERATE_COPY(range(0 * ssize, ssize));
    CHECK_THAT(h.try_emplace(h.cbegin() + offset, std::move(k), std::move(v)), IterEquals(h, 0));
    CHECK(k == init.begin()->first);
    CHECK(v == init.begin()->second);
  }
}

TEST_CASE("Dictionaries expand when load factor is exceeded", "[dictionary][expand]") {
  auto [a, b, dups] = testing_lists();
  testing::test_hashed_table_expands<
      testing::dictionary<key_type, mapped_type, probing::quadratic, ordering_policy::preserved>>(a, b);
}

FLAT_HASH_DYNAMIC_DICTIONARY_TEST_CASE("Dictionaries can be merged", "[dictionary][merge]", key_type, mapped_type) {
  auto [a, b, dups] = testing_lists();
  testing::test_hashed_merge<TestType>(a, b, mutable_proj{});
}

FLAT_HASH_DYNAMIC_DICTIONARY_TEST_CASE("Dictionaries can be spliced", "[dictionary][splice]", key_type, mapped_type) {
  auto [a, b, dups] = testing_lists();
  testing::test_hashed_splicing<TestType>(a, b);
}

FLAT_HASH_DYNAMIC_DICTIONARY_TEST_CASE("Items can be erased from dictionaries", "[dictionary][erase]", key_type,
                                       mapped_type) {
  auto [a, b, dups] = testing_lists();
  testing::test_hashed_erase<TestType>(a, b, pair_equal{}, key_proj{}, hetero_value_proj{});
}

FLAT_HASH_DYNAMIC_DICTIONARY_TEST_CASE("Items can be extracted from dictionaries", "[dictionary][extract]", key_type,
                                       mapped_type) {
  auto [a, b, dups] = testing_lists();
  testing::test_hashed_extract<TestType>(a, b, pair_equal{}, key_proj{}, hetero_value_proj{});
}

FLAT_HASH_NAMESPACE_END
