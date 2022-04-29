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

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {

// pop_back/pop_front should be constant time so don't use erase

template <class Container>
concept back_popable = traits_back_popable<Container> || appendable_from_capacity<Container>;

template <back_popable Container>
constexpr auto pop_back(Container& container) -> decltype(auto) {
  if constexpr (traits_back_popable<Container>) {
    return container_traits<Container>::pop_back(container);
  } else {
    auto allocator = maybe_get_allocator(container);
    destroy(allocator, back(container));
    container_traits<Container>::append_from_capacity(container, -1);
  }
}

template <back_popable Container>
[[nodiscard]] constexpr auto pop_back_extract(Container& container) -> std::ranges::range_value_t<Container> {
  using value_t = std::ranges::range_value_t<Container>;
  using return_t = decltype(pop_back(container));

  if constexpr (std::convertible_to<return_t, value_t>) {
    return pop_back(container);
  } else {
    value_t value = std::move(back(container));
    pop_back(container);
    return value;
  }
}

template <class Container>
concept front_popable = traits_front_popable<Container>;

template <front_popable Container>
constexpr auto pop_front(Container& container) -> decltype(auto) {
  return container_traits<Container>::pop_front(container);
}

template <front_popable Container>
[[nodiscard]] constexpr auto pop_front_extract(Container& container) -> std::ranges::range_value_t<Container> {
  using value_t = std::ranges::range_value_t<Container>;
  using return_t = decltype(pop_front(container));

  if constexpr (std::convertible_to<return_t, value_t>) {
    return pop_front(container);
  } else {
    value_t value = std::move(front(container));
    pop_front(container);
    return value;
  }
}

template <class Container>
concept popable = back_popable<Container> || front_popable<Container> || traits_erasable<Container> ||
                  traits_range_erasable<Container>;

enum struct pop_end {
  front,
  back,
};

template <popable Container>
inline constexpr pop_end popped_from = []() {
  if constexpr (back_popable<Container>) {
    return pop_end::back;
  } else if constexpr (front_popable<Container>) {
    return pop_end::front;
  } else if constexpr (traits_erasable<Container>) {
    return pop_end::back;
  } else if constexpr (traits_range_erasable<Container>) {
    return pop_end::back;
  } else {
    FLAT_HASH_UNREACHABLE();
  }
}();

template <popable Container>
constexpr auto pop(Container& container) -> decltype(auto) {
  if constexpr (back_popable<Container>) {
    return pop_back(container);
  } else if constexpr (front_popable<Container>) {
    return pop_front(container);
  } else if constexpr (traits_erasable<Container>) {
    return container_traits<Container>::erase(container, std::ranges::prev(std::ranges::cend(container)));
  } else if constexpr (traits_range_erasable<Container>) {
    auto cend = std::ranges::cend(container);
    return container_traits<Container>::erase(container, std::ranges::prev(cend), cend);
  } else {
    FLAT_HASH_UNREACHABLE();
  }
}

template <popable Container>
[[nodiscard]] constexpr auto pop_extract(Container& container) -> std::ranges::range_value_t<Container> {
  using value_t = std::ranges::range_value_t<Container>;
  using return_t = decltype(pop(container));

  constexpr auto select = [](auto& cnt) -> decltype(auto) {
    if constexpr (popped_from<Container> == pop_end::front) {
      return front(cnt);
    } else {
      return back(cnt);
    }
  };

  if constexpr (std::convertible_to<return_t, value_t>) {
    return pop(container);
  } else {
    value_t value = std::move(select(container));
    pop(container);
    return value;
  }
}

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
