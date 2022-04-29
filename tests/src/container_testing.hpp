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

#include <memory_resource>

#include <catch2/generators/catch_generators_range.hpp>
#include <flat_hash/detail/containers.hpp>

#include "testing.hpp"

FLAT_HASH_NAMESPACE_BEGIN

#define FLAT_HASH_TESTING_ORDERING_TEST(name, _X_TEST_MACRO)                                                     \
  SECTION(name " - ordering_policy::preserved"){_X_TEST_MACRO(::flat_hash::ordering_policy::preserved)} SECTION( \
      name " - ordering_policy::relaxed") {                                                                      \
    _X_TEST_MACRO(::flat_hash::ordering_policy::relaxed)                                                         \
  }

namespace testing {

enum struct throwing {
  yes = false,
  no = true,
};

template <throwing nothrow = throwing::yes, std::ranges::range R, class Allocator = no_allocator>
void test_allocators(R const& container, Allocator const& expected_allocator = {}) {
  SECTION("allocators") {
    STATIC_REQUIRE(detail::containers::maybe_allocator<Allocator>);

    if constexpr (std::same_as<Allocator, no_allocator>) {
      STATIC_REQUIRE_FALSE(detail::containers::is_allocator<Allocator>);
      STATIC_REQUIRE(std::same_as<detail::containers::allocator_t<R>, void>);
      STATIC_REQUIRE_FALSE(detail::containers::allocator_aware<R>);
      STATIC_REQUIRE_FALSE(detail::containers::nothrow_gettable_allocator<R>);

      CHECK(detail::containers::maybe_get_allocator(container) == expected_allocator);
    } else {
      STATIC_REQUIRE(detail::containers::is_allocator<Allocator>);
      STATIC_REQUIRE(detail::containers::allocator_aware<R>);
      STATIC_REQUIRE(std::same_as<detail::containers::allocator_t<R>, Allocator>);
      STATIC_REQUIRE(detail::containers::gettable_allocator<R>);
      STATIC_REQUIRE(detail::containers::nothrow_gettable_allocator<R> == (nothrow == throwing::no));

      CHECK(detail::containers::get_allocator(container) == expected_allocator);
      CHECK(detail::containers::maybe_get_allocator(container) == expected_allocator);
    }

    STATIC_REQUIRE(std::same_as<Allocator, optional_allocator<R>>);
  }
}

template <std::ranges::range R, class... Args>
auto test_construct(R const& expected, Args&&... args) -> R {
  STATIC_REQUIRE(detail::containers::constructible_container<R>);
  auto container = detail::containers::make_container<R>(std::forward<Args>(args)...);
  CHECK_THAT(container, Equals(expected));
  return container;
}

template <bool PastCapacity = true, std::ranges::range R>
void test_resize(R container, std::ranges::range_value_t<R> const& value) {
  SECTION("resizing") {
    STATIC_REQUIRE(detail::containers::resizable<R>);
    aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
    std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
    std::vector vector = pmr::to_vector(container, &resource);
    auto size = vector.size();
    auto capacity = detail::containers::capacity(container);

    constexpr auto inner_test = [](auto& original, auto& expected, auto&&... args) {
      detail::containers::resize(original, args...);
      expected.resize(args...);
      CHECK_THAT(original, Equals(expected));
    };

    if constexpr (PastCapacity) {
      SECTION("past the capacity") {
        auto new_size = capacity + 2;
        WHEN("default value resizing") { inner_test(container, vector, new_size); }
        WHEN("value resizing") { inner_test(container, vector, new_size, value); }
      }
    }

    CHECK(capacity > 1);

    SECTION("within the capacity") {
      WHEN("increasing the size") {
        auto new_size = std::max(size + 1, capacity - 1);
        AND_WHEN("default value resizing") { inner_test(container, vector, new_size); }
        AND_WHEN("value resizing") { inner_test(container, vector, new_size, value); }
      }

      if (size == 0) {
        detail::containers::resize(container, 2, value);
        vector.resize(2, value);
      }
      WHEN("reducing the size") { inner_test(container, vector, size - 1); }
    }
  }
}

template <class R>
void test_reserve(R container) {
  SECTION("reserve") {
    STATIC_REQUIRE(detail::containers::reservable<R>);

    testing::aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
    std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
    std::vector vector = pmr::to_vector(container, &resource);

    auto capacity = detail::containers::capacity(container);
    SECTION("reserving more increases the capacity but not the size") {
      auto new_size = capacity + 2;
      detail::containers::reserve(container, new_size);
      CHECK(detail::containers::capacity(container) == new_size);
      CHECK_THAT(container, Equals(vector));
    }

    SECTION("reserving less does not change size or capacity") {
      detail::containers::reserve(container, 0);
      CHECK(detail::containers::capacity(container) == capacity);
      CHECK_THAT(container, Equals(vector));
    }
  }
}

template <class R>
void test_clear(R container) {
  SECTION("clear") {
    STATIC_REQUIRE(detail::containers::clearable<R>);

    REQUIRE(std::ranges::size(container) != 0);
    detail::containers::clear(container);
    REQUIRE(std::ranges::size(container) == 0);
  }
}

template <class R>
void test_assign(R container, std::initializer_list<std::ranges::range_value_t<R>> values) {
  using ilist = std::initializer_list<std::ranges::range_value_t<R>>;
  SECTION("assign") {
    STATIC_REQUIRE(detail::containers::assignable_to<ilist, R>);

    WHEN("assigning a range") {
      using diff_t = std::ranges::range_difference_t<R>;
      diff_t ssize = std::ranges::ssize(values);
      auto offset = GENERATE_COPY(range(diff_t{0}, ssize));

      auto subvalues = std::ranges::subrange(values.begin() + offset, values.end());
      detail::containers::assign(container, subvalues);
      CHECK_THAT(container, Equals(subvalues));
    }

    WHEN("assigning an empty range") {
      detail::containers::assign(container, ilist{});
      CHECK(std::ranges::size(container) == 0);
    }
  }
}

template <bool PastCapacity = true, class R>
void test_append(R container, std::initializer_list<std::ranges::range_value_t<R>> values) {
  using ilist = std::initializer_list<std::ranges::range_value_t<R>>;
  SECTION("append") {
    aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
    std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
    std::vector vector = pmr::to_vector(container, &resource);

    STATIC_REQUIRE(detail::containers::appendable_to<ilist, R>);

    WHEN("appending a range") {
      using diff_t = std::ranges::range_difference_t<R>;
      diff_t ssize = std::ranges::ssize(values);
      auto offset = GENERATE_COPY(range(diff_t{0}, ssize));

      auto subvalues = std::ranges::subrange(values.begin() + offset, values.end());
      detail::containers::append(container, subvalues);
      vector.insert(vector.cend(), subvalues.begin(), subvalues.end());
      CHECK_THAT(container, Equals(vector));
    }

    if constexpr (PastCapacity) {
      WHEN("appending past capacity") {
        auto capacity = detail::containers::capacity(container);
        while (vector.size() <= capacity) {
          detail::containers::append(container, values);
          vector.insert(vector.cend(), values.begin(), values.end());
          CHECK_THAT(container, Equals(vector));
        }
      }
    }

    WHEN("appending an empty range") {
      detail::containers::append(container, ilist{});
      CHECK_THAT(container, Equals(vector));
    }
  }
}

template <class R, class... Args>
void test_emplace_back(R container, Args&&... args) {
  SECTION("emplace back") {
    STATIC_REQUIRE(detail::containers::back_emplaceable<R, Args...>);

    aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
    std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
    std::vector vector = pmr::to_vector(container, &resource);

    auto n = std::max(static_cast<std::size_t>(detail::containers::capacity(container) - std::ranges::size(container)),
                      std::size_t{1});
    for (auto i [[maybe_unused]] : std::views::iota(std::size_t{0}, n)) {
      vector.emplace_back(args...);
      CHECK(detail::containers::emplace_back(container, args...) == vector.back());
      CHECK_THAT(container, Equals(vector));
    }
  }
}

template <ordering_policy ordering, class R>
void _test_erase_section(R container) {
  aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
  std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
  std::vector vector = pmr::to_vector(container, &resource);

  using diff_t = std::ranges::range_difference_t<R>;
  diff_t ssize = static_cast<diff_t>(vector.size());

  WHEN("erasing single value") {
    STATIC_REQUIRE(detail::containers::erasable<R>);

    auto offset = GENERATE_COPY(range(diff_t{0}, ssize));

    vector.erase(vector.cbegin() + offset);
    auto erased_it = detail::containers::policy_erase<ordering>(container, std::ranges::cbegin(container) + offset);
    CHECK(erased_it == std::ranges::begin(container) + offset);
    if constexpr (ordering != ordering_policy::preserved) {
      // order not guaranteed, sort in place to make orders identical
      std::ranges::sort(container);
      std::ranges::sort(vector);
    }

    CHECK_THAT(container, Equals(vector));
  }

  WHEN("erasing range") {
    auto start = GENERATE_COPY(range(diff_t{0}, ssize));
    auto end = GENERATE_REF(range(start + 1, ssize + 1));

    vector.erase(vector.cbegin() + start, vector.cbegin() + end);
    auto erased_it = detail::containers::policy_erase<ordering>(container, std::ranges::cbegin(container) + start,
                                                                std::ranges::cbegin(container) + end);
    CHECK(erased_it == std::ranges::begin(container) + start);
    if constexpr (ordering != ordering_policy::preserved) {
      // order not guaranteed, sort in place to make orders identical
      std::ranges::sort(container);
      std::ranges::sort(vector);
    }

    CHECK_THAT(container, Equals(vector));
  }
}

template <class R>
void test_erase(R container) {
#define FLAT_HASH__ORDERING_SECTION(ordering) _test_erase_section<ordering>(container);
  FLAT_HASH_TESTING_ORDERING_TEST("erase", FLAT_HASH__ORDERING_SECTION);
#undef FLAT_HASH__ORDERING_SECTION
}

template <class R>
void test_erase_after(R container) {
  using diff_t = std::ranges::range_difference_t<R>;

  SECTION("erase after") {
    STATIC_REQUIRE(detail::containers::erasable<R>);

    aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
    std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
    std::vector vector = pmr::to_vector(container, &resource);

    diff_t ssize = static_cast<diff_t>(vector.size());
    auto offset = GENERATE_COPY(range(diff_t{0}, ssize));

    {
      auto it = vector.erase(vector.cbegin() + offset, vector.cend());
      CHECK(it == vector.end());
    }
    auto it = detail::containers::erase_after(container, std::ranges::cbegin(container) + offset);

    CHECK(it == std::ranges::end(container));
    CHECK_THAT(container, Equals(vector));
  }
}

template <ordering_policy ordering, class R>
void _test_insert_section(R container, std::initializer_list<std::ranges::range_value_t<R>> vals) {
  using diff_t = std::ranges::range_difference_t<R>;

  aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
  std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
  std::vector vector = pmr::to_vector(container, &resource);

  diff_t ssize = static_cast<diff_t>(vector.size());

  WHEN("inserting single value") {
    STATIC_REQUIRE(detail::containers::insertible_to<std::ranges::range_value_t<R> const&, R>);

    auto offset = GENERATE_COPY(range(diff_t{0}, ssize + 1));
    auto&& value = GENERATE_COPY(values(vals));

    vector.insert(vector.cbegin() + offset, value);
    CHECK(*detail::containers::policy_insert<ordering>(container, std::ranges::cbegin(container) + offset, value) ==
          value);
    if constexpr (ordering != ordering_policy::preserved) {
      // order not guaranteed, sort in place to make orders identical
      std::ranges::sort(container);
      std::ranges::sort(vector);
    }

    CHECK_THAT(container, Equals(vector));
  }

  WHEN("inserting range") {
    STATIC_REQUIRE(detail::containers::insertible_to<std::initializer_list<std::ranges::range_value_t<R>>, R>);

    auto offset = GENERATE_COPY(range(diff_t{0}, ssize + 1));

    vector.insert(vector.cbegin() + offset, vals.begin(), vals.end());
    CHECK(*detail::containers::policy_insert<ordering>(container, std::ranges::cbegin(container) + offset, vals) ==
          *vals.begin());
    if constexpr (ordering != ordering_policy::preserved) {
      // order not guaranteed, sort in place to make orders identical
      std::ranges::sort(container);
      std::ranges::sort(vector);
    }

    CHECK_THAT(container, Equals(vector));
  }
}

template <class R>
void test_insert(R container, std::initializer_list<std::ranges::range_value_t<R>> vals) {
#define FLAT_HASH__ORDERING_SECTION(ordering) _test_insert_section<ordering>(container, vals);
  FLAT_HASH_TESTING_ORDERING_TEST("insert", FLAT_HASH__ORDERING_SECTION);
#undef FLAT_HASH__ORDERING_SECTION
}

template <class R>
void test_pop_back(R container) {
  SECTION("pop back") {
    STATIC_REQUIRE(detail::containers::back_popable<R>);

    aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
    std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
    std::vector vector = pmr::to_vector(container, &resource);

    while (!vector.empty()) {
      detail::containers::pop_back(container);
      vector.pop_back();
      CHECK_THAT(container, Equals(vector));
    }
  }
}

template <class R>
void test_pop_front(R container) {
  SECTION("pop front") {
    STATIC_REQUIRE(detail::containers::front_popable<R>);

    aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
    std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
    std::vector vector = pmr::to_vector(container, &resource);

    for (auto i : std::views::iota(std::ptrdiff_t{0}, std::ranges::ssize(vector))) {
      detail::containers::pop_front(container);
      CHECK_THAT(container, Equals(std::ranges::subrange(vector.begin() + i + 1, vector.end())));
    }
  }
}

template <ordering_policy ordering, class R>
void _test_extract_section(R container) {
  using diff_t = std::ranges::range_difference_t<R>;
  STATIC_REQUIRE(detail::containers::extractable<R>);

  aligned_buffer_t<std::ranges::range_value_t<R>, 16> storage;
  std::pmr::monotonic_buffer_resource resource(&storage, sizeof(storage));
  std::vector vector = pmr::to_vector(container, &resource);

  diff_t ssize = static_cast<diff_t>(vector.size());

  auto offset = GENERATE_COPY(range(diff_t{0}, ssize));

  auto value = *(std::ranges::begin(container) + offset);

  CHECK_THAT(container, Equals(vector));
  CHECK(value == detail::containers::extract<ordering>(container, std::ranges::cbegin(container) + offset));
  vector.erase(vector.cbegin() + offset);
  if constexpr (ordering != ordering_policy::preserved) {
    std::ranges::sort(vector);
    std::ranges::sort(container);
  }

  CHECK_THAT(container, Equals(vector));
}

template <class R>
void test_extract(R container) {
#define FLAT_HASH__ORDERING_SECTION(ordering) _test_extract_section<ordering>(container);
  FLAT_HASH_TESTING_ORDERING_TEST("extract", FLAT_HASH__ORDERING_SECTION);
#undef FLAT_HASH__ORDERING_SECTION
}

}  // namespace testing

FLAT_HASH_NAMESPACE_END
