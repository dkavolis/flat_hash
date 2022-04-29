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

#include "../macros.hpp"
#include "capacity.hpp"
#include "container_traits.hpp"
#include "get_allocator.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {

template <class Container>
concept clearable = traits_clearable<Container> || traits_back_popable<Container> || traits_front_popable<Container> ||
                    traits_erasable<Container> || traits_range_erasable<Container> ||
                    (appendable_from_capacity<Container> &&
                     std::is_destructible_v<std::ranges::range_value_t<Container>>);

enum struct clear_algorithm {
  clear,
  pop_back,
  pop_front,
  erase_single,
  erase_range,
  append_from_capacity,
};

template <clearable Container>
[[nodiscard]] consteval auto select_clear_algorithm() noexcept -> clear_algorithm {
  if constexpr (traits_clearable<Container>) {
    return clear_algorithm::clear;
  } else if constexpr (appendable_from_capacity<Container>) {
    return clear_algorithm::append_from_capacity;
  } else if constexpr (traits_range_erasable<Container>) {
    return clear_algorithm::erase_range;
  } else if constexpr (traits_back_popable<Container>) {
    return clear_algorithm::pop_back;
  } else if constexpr (traits_front_popable<Container>) {
    return clear_algorithm::pop_front;
  } else if constexpr (traits_erasable<Container>) {
    return clear_algorithm::erase_single;
  } else {
    FLAT_HASH_UNREACHABLE();
  }
}

template <class Container>
inline constexpr bool nothrow_clearable = clearable<Container> && []() {
  switch (select_clear_algorithm<Container>()) {
    case clear_algorithm::clear: return traits_nothrow_clearable<Container>;
    case clear_algorithm::append_from_capacity:
      return nothrow_appendable_from_capacity<Container> &&
             std::is_nothrow_destructible_v<std::ranges::range_value_t<Container>>;
    default: return false;
  }
}();

template <clearable Container>
constexpr void clear(Container& container) noexcept(nothrow_clearable<Container>) {
  constexpr auto algo = select_clear_algorithm<Container>();

  if (std::ranges::empty(container)) { return; }

  if constexpr (algo == clear_algorithm::clear) {
    container_traits<Container>::clear(container);
  } else if constexpr (algo == clear_algorithm::pop_back || algo == clear_algorithm::pop_front ||
                       algo == clear_algorithm::erase_single) {
    while (!std::ranges::empty(container)) {
      if constexpr (algo == clear_algorithm::pop_back) {
        container_traits<Container>::pop_back(container);
      } else if constexpr (algo == clear_algorithm::pop_front) {
        container_traits<Container>::pop_front(container);
      } else {
        container_traits<Container>::erase(container, std::ranges::prev(std::ranges::cend(container)));
      }
    }
  } else if constexpr (algo == clear_algorithm::erase_range) {
    container_traits<Container>::erase(container, std::ranges::cbegin(container), std::ranges::cend(container));
  } else if constexpr (algo == clear_algorithm::append_from_capacity) {
    auto count = std::ranges::ssize(container);
    auto allocator = maybe_get_allocator(container);
    destroy_range(allocator, container);
    append_from_capacity(container, -count);
  } else {
    FLAT_HASH_UNREACHABLE();
  }
}

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
