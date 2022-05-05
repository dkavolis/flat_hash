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

#include <functional>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_container_properties.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>
#include <flat_hash/detail/bits.hpp>
#include <flat_hash/detail/config.hpp>
#include <flat_hash/detail/containers.hpp>
#include <flat_hash/detail/equal_to.hpp>
#include <flat_hash/detail/format.hpp>
#include <flat_hash/detail/inline_vector.hpp>
#include <flat_hash/detail/transparent_hash.hpp>

#include "container_testing.hpp"
#include "testing.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {

TEST_CASE("Internal concepts tests", "[internal][concepts]") {
  SECTION("mutable references are converted to immutable") {
    STATIC_REQUIRE(std::same_as<add_const_if_ref_t<int>, int>);
    STATIC_REQUIRE(std::same_as<add_const_if_ref_t<int&>, int const&>);

    using reference = add_const_if_ref_t<std::ranges::range_reference_t<std::span<int>>>;
    STATIC_REQUIRE(std::same_as<reference, int const&>);
  }

  SECTION("types are checked if they are formattable") {
    STATIC_REQUIRE(formattable<int, char>);
    STATIC_REQUIRE(formattable<std::string, char>);
    STATIC_REQUIRE(formattable<void*, char>);
    STATIC_REQUIRE_FALSE(formattable<no_allocator, char>);
  }

  SECTION("integral hashers are transparent") {
    STATIC_REQUIRE(transparent_functor<hash<int>> && hash_for<hash<int>, int>);
    STATIC_REQUIRE(transparent_functor<hash<int>> && hash_for<hash<int>, short>);
    STATIC_REQUIRE(transparent_functor<hash<int>> && hash_for<hash<int>, long>);
    STATIC_REQUIRE(transparent_functor<hash<int>> && hash_for<hash<int>, unsigned short>);
    STATIC_REQUIRE(valid_key<int, int, hash<int>, equal_to>);
    STATIC_REQUIRE(valid_key<short, int, hash<int>, equal_to>);
    STATIC_REQUIRE(valid_key<long, int, hash<int>, equal_to>);
    STATIC_REQUIRE(valid_key<unsigned, int, hash<int>, equal_to>);

    STATIC_REQUIRE(transparent_functor<hash<unsigned>> && hash_for<hash<unsigned>, unsigned>);
    STATIC_REQUIRE(transparent_functor<hash<unsigned>> && hash_for<hash<unsigned>, unsigned short>);
    STATIC_REQUIRE(transparent_functor<hash<unsigned>> && hash_for<hash<unsigned>, unsigned long>);
    STATIC_REQUIRE(valid_key<unsigned, int, hash<unsigned>, equal_to>);
    STATIC_REQUIRE(valid_key<unsigned short, int, hash<unsigned>, equal_to>);
    STATIC_REQUIRE(valid_key<unsigned long, int, hash<unsigned>, equal_to>);
  }

  SECTION("string hashers are transparent") {
    STATIC_REQUIRE(transparent_functor<hash<std::string>> && hash_for<hash<std::string>, std::string_view>);
    STATIC_REQUIRE(transparent_functor<hash<std::string>> && hash_for<hash<std::string>, char const*>);
    STATIC_REQUIRE(transparent_functor<hash<std::string>> && hash_for<hash<std::string>, char[]>);
    STATIC_REQUIRE(valid_key<std::string_view, std::string, hash<std::string>, equal_to>);
    STATIC_REQUIRE(valid_key<char const*, std::string, hash<std::string>, equal_to>);
    STATIC_REQUIRE(valid_key<char[], std::string, hash<std::string>, equal_to>);

    STATIC_REQUIRE(transparent_functor<hash<std::string_view>> && hash_for<hash<std::string_view>, std::string>);
    STATIC_REQUIRE(transparent_functor<hash<std::string_view>> && hash_for<hash<std::string_view>, char const*>);
    STATIC_REQUIRE(transparent_functor<hash<std::string_view>> && hash_for<hash<std::string_view>, char[]>);
    STATIC_REQUIRE(valid_key<std::string, std::string_view, hash<std::string_view>, equal_to>);
    STATIC_REQUIRE(valid_key<char const*, std::string_view, hash<std::string_view>, equal_to>);
    STATIC_REQUIRE(valid_key<char[], std::string_view, hash<std::string_view>, equal_to>);
  }

  SECTION("pointer hashers are transparent") {
    STATIC_REQUIRE(transparent_functor<hash<int const*>> && hash_for<hash<int const*>, int*>);
    STATIC_REQUIRE(transparent_functor<hash<int const*>> && hash_for<hash<int const*>, std::unique_ptr<int>>);
    STATIC_REQUIRE(transparent_functor<hash<int const*>> && hash_for<hash<int const*>, std::shared_ptr<int>>);

    STATIC_REQUIRE(valid_key<int*, int const*, hash<int const*>, equal_to>);
    STATIC_REQUIRE(valid_key<std::unique_ptr<int>, int const*, hash<int const*>, equal_to>);
    STATIC_REQUIRE(valid_key<std::shared_ptr<int>, int const*, hash<int const*>, equal_to>);
  }

  SECTION("smart pointer hashers are transparent") {
    STATIC_REQUIRE(transparent_functor<hash<std::unique_ptr<int>>> && hash_for<hash<std::unique_ptr<int>>, int*>);
    STATIC_REQUIRE(transparent_functor<hash<std::unique_ptr<int>>> &&
                   hash_for<hash<std::unique_ptr<int>>, std::unique_ptr<int>>);
    STATIC_REQUIRE(transparent_functor<hash<std::unique_ptr<int>>> &&
                   hash_for<hash<std::unique_ptr<int>>, std::shared_ptr<int>>);
    STATIC_REQUIRE(valid_key<int const*, std::unique_ptr<int>, hash<std::unique_ptr<int>>, equal_to>);
    STATIC_REQUIRE(valid_key<std::unique_ptr<int>, std::unique_ptr<int>, hash<std::unique_ptr<int>>, equal_to>);
    STATIC_REQUIRE(valid_key<std::shared_ptr<int>, std::unique_ptr<int>, hash<std::unique_ptr<int>>, equal_to>);

    STATIC_REQUIRE(transparent_functor<hash<std::shared_ptr<int>>> && hash_for<hash<std::shared_ptr<int>>, int*>);
    STATIC_REQUIRE(transparent_functor<hash<std::shared_ptr<int>>> &&
                   hash_for<hash<std::shared_ptr<int>>, std::unique_ptr<int>>);
    STATIC_REQUIRE(transparent_functor<hash<std::shared_ptr<int>>> &&
                   hash_for<hash<std::shared_ptr<int>>, std::shared_ptr<int>>);

    STATIC_REQUIRE(valid_key<int const*, std::shared_ptr<int>, hash<std::shared_ptr<int>>, equal_to>);
    STATIC_REQUIRE(valid_key<std::unique_ptr<int>, std::shared_ptr<int>, hash<std::shared_ptr<int>>, equal_to>);
    STATIC_REQUIRE(valid_key<std::shared_ptr<int>, std::shared_ptr<int>, hash<std::shared_ptr<int>>, equal_to>);
  }

  SECTION("misc concepts") {
    STATIC_REQUIRE(mutable_range<std::span<int>>);
    STATIC_REQUIRE(mutable_range<std::span<int> const>);
    STATIC_REQUIRE(mutable_range<std::vector<int>>);
    STATIC_REQUIRE(mutable_range<std::array<int, 2>>);

    STATIC_REQUIRE_FALSE(mutable_range<std::span<int const>>);
    STATIC_REQUIRE_FALSE(mutable_range<std::span<int const> const>);
    STATIC_REQUIRE_FALSE(mutable_range<std::vector<int> const>);
    STATIC_REQUIRE_FALSE(mutable_range<std::array<int, 2> const>);

    STATIC_REQUIRE(pointer<int*>);
    STATIC_REQUIRE(pointer<int const*>);
    STATIC_REQUIRE_FALSE(pointer<int const&>);

    STATIC_REQUIRE(smart_pointer<std::unique_ptr<int>>);
    STATIC_REQUIRE(smart_pointer<std::shared_ptr<int>>);
    STATIC_REQUIRE(smart_pointer_to<std::shared_ptr<int>, int*>);
    STATIC_REQUIRE_FALSE(smart_pointer_to<std::shared_ptr<int const>, int*>);
    STATIC_REQUIRE(smart_pointer_to<std::shared_ptr<int>, int const*>);
    STATIC_REQUIRE(smart_pointer_to<std::shared_ptr<int const>, int const*>);
  }

  SECTION("static container sizes are available at compile time") {
    STATIC_REQUIRE(static_size_v<std::array<int, 2>> == 2);
    STATIC_REQUIRE(static_size_v<std::span<int, 2>> == 2);
    STATIC_REQUIRE(static_size_v<int[2]> == 2);
  }
}

TEST_CASE("Generic container concepts", "[internal][concepts][containers]") {
  SECTION("std::vector") {
    using range = std::vector<int>;
    STATIC_REQUIRE(traits_resizable<range>);
    STATIC_REQUIRE(traits_resizable_fill<range>);
    STATIC_REQUIRE(traits_reservable<range>);
    STATIC_REQUIRE(traits_capacity<range>);
    STATIC_REQUIRE(traits_nothrow_capacity<range>);
    STATIC_REQUIRE(traits_get_allocator<range>);
    STATIC_REQUIRE(traits_nothrow_get_allocator<range>);
    STATIC_REQUIRE(traits_clearable<range>);
    STATIC_REQUIRE(traits_nothrow_clearable<range>);
    STATIC_REQUIRE(traits_assignable<range, std::initializer_list<int>>);
    STATIC_REQUIRE(traits_back_emplaceable<range, int>);
    STATIC_REQUIRE(traits_erasable<range>);
    STATIC_REQUIRE(traits_range_erasable<range>);
    STATIC_REQUIRE(traits_insertible<range, int>);
    STATIC_REQUIRE(traits_back_popable<range>);
    STATIC_REQUIRE_FALSE(traits_front_popable<range>);
    STATIC_REQUIRE(subscriptable<range, std::size_t>);
    STATIC_REQUIRE(std::same_as<allocator_t<range>, std::allocator<int>>);

    STATIC_REQUIRE(resizable<range>);
    STATIC_REQUIRE(reservable<range>);
    STATIC_REQUIRE(gettable_allocator<range>);
    STATIC_REQUIRE(nothrow_gettable_allocator<range>);
    STATIC_REQUIRE(clearable<range>);
    STATIC_REQUIRE(back_emplaceable<range, int>);
    STATIC_REQUIRE(erasable<range>);
    STATIC_REQUIRE(insertible_to<int, range>);
    STATIC_REQUIRE(insertible_to<range, range>);
    STATIC_REQUIRE(back_popable<range>);
    STATIC_REQUIRE_FALSE(front_popable<range>);
    STATIC_REQUIRE(popable<range>);
    STATIC_REQUIRE(extractable<range>);
    STATIC_REQUIRE(sized_constructible<range>);
    STATIC_REQUIRE(appendable_to<range, range>);
    STATIC_REQUIRE(assignable_to<range, range>);
    STATIC_REQUIRE_FALSE(appendable_from_capacity<range>);
  }

  SECTION("std::array") {
    using range = std::array<int, 2>;
    STATIC_REQUIRE_FALSE(traits_resizable<range>);
    STATIC_REQUIRE_FALSE(traits_resizable_fill<range>);
    STATIC_REQUIRE_FALSE(traits_reservable<range>);
    STATIC_REQUIRE_FALSE(traits_capacity<range>);
    STATIC_REQUIRE_FALSE(traits_nothrow_capacity<range>);
    STATIC_REQUIRE_FALSE(traits_get_allocator<range>);
    STATIC_REQUIRE_FALSE(traits_nothrow_get_allocator<range>);
    STATIC_REQUIRE_FALSE(traits_clearable<range>);
    STATIC_REQUIRE_FALSE(traits_nothrow_clearable<range>);
    STATIC_REQUIRE_FALSE(traits_assignable<range, std::initializer_list<int>>);
    STATIC_REQUIRE_FALSE(traits_back_emplaceable<range, int>);
    STATIC_REQUIRE_FALSE(traits_erasable<range>);
    STATIC_REQUIRE_FALSE(traits_range_erasable<range>);
    STATIC_REQUIRE_FALSE(traits_insertible<range, int>);
    STATIC_REQUIRE_FALSE(traits_back_popable<range>);
    STATIC_REQUIRE_FALSE(traits_front_popable<range>);
    STATIC_REQUIRE(subscriptable<range, std::size_t>);
    STATIC_REQUIRE(std::same_as<allocator_t<range>, void>);

    STATIC_REQUIRE_FALSE(resizable<range>);
    STATIC_REQUIRE_FALSE(reservable<range>);
    STATIC_REQUIRE_FALSE(gettable_allocator<range>);
    STATIC_REQUIRE_FALSE(nothrow_gettable_allocator<range>);
    STATIC_REQUIRE_FALSE(clearable<range>);
    STATIC_REQUIRE_FALSE(back_emplaceable<range, int>);
    STATIC_REQUIRE_FALSE(erasable<range>);
    STATIC_REQUIRE_FALSE(insertible_to<int, range>);
    STATIC_REQUIRE_FALSE(insertible_to<range, range>);
    STATIC_REQUIRE_FALSE(back_popable<range>);
    STATIC_REQUIRE_FALSE(front_popable<range>);
    STATIC_REQUIRE_FALSE(popable<range>);
    STATIC_REQUIRE_FALSE(extractable<range>);
    STATIC_REQUIRE_FALSE(sized_constructible<range>);
    STATIC_REQUIRE_FALSE(appendable_to<range, range>);
    STATIC_REQUIRE_FALSE(assignable_to<range, range>);
    STATIC_REQUIRE_FALSE(appendable_from_capacity<range>);
  }

  SECTION("std::span") {
    using range = std::span<int, 2>;
    STATIC_REQUIRE_FALSE(traits_resizable<range>);
    STATIC_REQUIRE_FALSE(traits_resizable_fill<range>);
    STATIC_REQUIRE_FALSE(traits_reservable<range>);
    STATIC_REQUIRE_FALSE(traits_capacity<range>);
    STATIC_REQUIRE_FALSE(traits_nothrow_capacity<range>);
    STATIC_REQUIRE_FALSE(traits_get_allocator<range>);
    STATIC_REQUIRE_FALSE(traits_nothrow_get_allocator<range>);
    STATIC_REQUIRE_FALSE(traits_clearable<range>);
    STATIC_REQUIRE_FALSE(traits_nothrow_clearable<range>);
    STATIC_REQUIRE_FALSE(traits_assignable<range, std::initializer_list<int>>);
    STATIC_REQUIRE_FALSE(traits_back_emplaceable<range, int>);
    STATIC_REQUIRE_FALSE(traits_erasable<range>);
    STATIC_REQUIRE_FALSE(traits_range_erasable<range>);
    STATIC_REQUIRE_FALSE(traits_insertible<range, int>);
    STATIC_REQUIRE_FALSE(traits_back_popable<range>);
    STATIC_REQUIRE_FALSE(traits_front_popable<range>);
    STATIC_REQUIRE(subscriptable<range, std::size_t>);
    STATIC_REQUIRE(std::same_as<allocator_t<range>, void>);

    STATIC_REQUIRE_FALSE(resizable<range>);
    STATIC_REQUIRE_FALSE(reservable<range>);
    STATIC_REQUIRE_FALSE(gettable_allocator<range>);
    STATIC_REQUIRE_FALSE(nothrow_gettable_allocator<range>);
    STATIC_REQUIRE_FALSE(clearable<range>);
    STATIC_REQUIRE_FALSE(back_emplaceable<range, int>);
    STATIC_REQUIRE_FALSE(erasable<range>);
    STATIC_REQUIRE_FALSE(insertible_to<int, range>);
    STATIC_REQUIRE_FALSE(insertible_to<range, range>);
    STATIC_REQUIRE_FALSE(back_popable<range>);
    STATIC_REQUIRE_FALSE(front_popable<range>);
    STATIC_REQUIRE_FALSE(popable<range>);
    STATIC_REQUIRE_FALSE(extractable<range>);
    STATIC_REQUIRE_FALSE(sized_constructible<range>);
    STATIC_REQUIRE_FALSE(appendable_to<range, range>);
    STATIC_REQUIRE_FALSE(assignable_to<range, range>);
    STATIC_REQUIRE_FALSE(appendable_from_capacity<range>);
  }

  SECTION("minimal interface container") {
    using range = inline_vector<int, 5>;
    STATIC_REQUIRE(traits_resizable<range>);
    STATIC_REQUIRE(traits_resizable_fill<range>);
    STATIC_REQUIRE_FALSE(traits_reservable<range>);
    STATIC_REQUIRE(traits_capacity<range>);
    STATIC_REQUIRE(traits_nothrow_capacity<range>);
    STATIC_REQUIRE_FALSE(traits_get_allocator<range>);
    STATIC_REQUIRE_FALSE(traits_nothrow_get_allocator<range>);
    STATIC_REQUIRE(traits_clearable<range>);
    STATIC_REQUIRE(traits_nothrow_clearable<range>);
    STATIC_REQUIRE_FALSE(traits_assignable<range, std::initializer_list<int>>);
    STATIC_REQUIRE(traits_back_emplaceable<range, int>);
    STATIC_REQUIRE_FALSE(traits_erasable<range>);
    STATIC_REQUIRE_FALSE(traits_range_erasable<range>);
    STATIC_REQUIRE_FALSE(traits_insertible<range, int>);
    STATIC_REQUIRE(traits_back_popable<range>);
    STATIC_REQUIRE_FALSE(traits_front_popable<range>);
    STATIC_REQUIRE_FALSE(subscriptable<range, std::size_t>);
    STATIC_REQUIRE(std::same_as<allocator_t<range>, void>);

    STATIC_REQUIRE(resizable<range>);
    STATIC_REQUIRE_FALSE(reservable<range>);
    STATIC_REQUIRE_FALSE(gettable_allocator<range>);
    STATIC_REQUIRE_FALSE(nothrow_gettable_allocator<range>);
    STATIC_REQUIRE(clearable<range>);
    STATIC_REQUIRE(back_emplaceable<range, int>);
    STATIC_REQUIRE(erasable<range>);
    STATIC_REQUIRE(insertible_to<int, range>);
    STATIC_REQUIRE(insertible_to<range, range>);
    STATIC_REQUIRE(back_popable<range>);
    STATIC_REQUIRE_FALSE(front_popable<range>);
    STATIC_REQUIRE(popable<range>);
    STATIC_REQUIRE(extractable<range>);
    STATIC_REQUIRE(sized_constructible<range>);
    STATIC_REQUIRE(appendable_to<range, range>);
    STATIC_REQUIRE(assignable_to<range, range>);
    STATIC_REQUIRE(appendable_from_capacity<range>);
  }
}

TEST_CASE("Generic container functions", "[internal][containers]") {
  constexpr static std::initializer_list<int> init = {0, 1, 2, 3};
  SECTION("std::vector") {
    std::vector<int> v = init;

    SECTION("construction") {
      WHEN("can construct from size") { testing::test_construct(std::vector<int>(4), 4U); }
      WHEN("can construct from size and value") { testing::test_construct(std::vector<int>(4, 1), 4U, 1); }
      WHEN("can construct from size, value and allocator") {
        CHECK(testing::test_construct(std::vector<int>(4, 1), 4U, 1, v.get_allocator()).get_allocator() ==
              v.get_allocator());
      }
    }

    testing::test_allocators<testing::throwing::no>(v, v.get_allocator());
    testing::test_resize(v, -1);
    testing::test_reserve(v);
    testing::test_clear(v);
    testing::test_assign(v, init);
    testing::test_append(v, init);
    testing::test_emplace_back(v, -1);
    testing::test_erase_after(v);
    testing::test_erase(v);
    testing::test_insert(v, init);
    testing::test_pop_back(v);
    testing::test_extract(v);
  }

  SECTION("vector of strings") {
    static std::initializer_list<std::string> strings = {"first", "second", "third", "fourth", "fifth"};
    std::vector<std::string> v = strings;

    SECTION("construction") {
      WHEN("can construct from size") { testing::test_construct(std::vector<std::string>(4), 4U); }
      WHEN("can construct from size and value") {
        testing::test_construct(std::vector<std::string>(4, *strings.begin()), 4U, *strings.begin());
      }
    }

    testing::test_allocators<testing::throwing::no>(v, v.get_allocator());
    testing::test_resize(v, "42");
    testing::test_reserve(v);
    testing::test_clear(v);
    testing::test_assign(v, strings);
    testing::test_append(v, strings);
    testing::test_emplace_back(v, "42");
    testing::test_erase_after(v);
    testing::test_erase(v);
    testing::test_insert(v, strings);
    testing::test_pop_back(v);
    testing::test_extract(v);

    // check that long strings are also properly returned
    emplace_back(v, "some very long string");
    CHECK(pop_back_extract(v) == "some very long string");
  }

  SECTION("inline vector") {
    inline_vector<int, 10> v = init;

    SECTION("construction") {
      WHEN("can construct from size") { testing::test_construct(inline_vector<int, 10>(4), 4U); }
      WHEN("can construct from size and value") { testing::test_construct(inline_vector<int, 10>(4, 1), 4U, 1); }
    }

    testing::test_allocators<testing::throwing::no>(v, no_allocator{});
    testing::test_resize<false>(v, -1);
    testing::test_clear(v);
    testing::test_assign(v, init);
    testing::test_append<false>(v, init);
    testing::test_emplace_back(v, -1);
    testing::test_erase_after(v);
    testing::test_erase(v);
    testing::test_insert(v, init);
    testing::test_pop_back(v);
    testing::test_extract(v);
  }

  SECTION("small vector of strings") {
    static std::initializer_list<std::string> strings = {"first", "second", "third", "fourth", "fifth"};
    inline_vector<std::string, 10> v = strings;

    SECTION("construction") {
      WHEN("can construct from size") { testing::test_construct(inline_vector<std::string, 10>(4), 4U); }
      WHEN("can construct from size and value") {
        testing::test_construct(inline_vector<std::string, 10>(4, *strings.begin()), 4U, *strings.begin());
      }
    }

    testing::test_allocators<testing::throwing::no>(v, no_allocator{});
    testing::test_resize<false>(v, "42");
    testing::test_clear(v);
    testing::test_assign(v, strings);
    testing::test_append<false>(v, strings);
    testing::test_emplace_back(v, "42");
    testing::test_erase_after(v);
    testing::test_erase(v);
    testing::test_insert(v, strings);
    testing::test_pop_back(v);
    testing::test_extract(v);

    // check that long strings are also properly returned
    emplace_back(v, "some very long string");
    CHECK(pop_back_extract(v) == "some very long string");

    SECTION("assigning itself has no effect") {
      v = v;
      CHECK_THAT(v, Equals(strings));

      v = std::move(v);
      CHECK_THAT(v, Equals(strings));
    }
  }
}

}  // namespace detail::containers

struct Unformattable {
  int i;
};

TEST_CASE("Utilities tests", "[utilities][detail]") {
  SECTION("maybe_format_arg always return formattable value") {
    WHEN("argument is already formattable") {
      CHECK(FLAT_HASH_FORMAT_NS format("{}", detail::maybe_format_arg(42)) == "42");
    }

    WHEN("argument is not formattable") {
      CHECK(FLAT_HASH_FORMAT_NS format("{}", detail::maybe_format_arg(Unformattable{42})) == "{?}");
    }
  }
}

FLAT_HASH_NAMESPACE_END
