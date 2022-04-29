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
#include "append.hpp"
#include "container_traits.hpp"
#include "erase.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {

template <std::unsigned_integral UI, std::invocable T>
[[nodiscard]] constexpr auto generate_n(UI n, T&& function) noexcept {
  return std::views::iota(UI{0}, n) | std::views::transform(std::forward<T>(function));
}

template <class Container>
concept resizable = traits_resizable<Container> || traits_resizable_fill<Container> ||
                    (erasable<Container> && appendable_to<Container, Container>);

template <resizable Container>
constexpr void resize(Container& container, std::ranges::range_size_t<Container> size,
                      std::ranges::range_value_t<Container> const& value) {
  if constexpr (traits_resizable_fill<Container>) {
    container_traits<Container>::resize(container, size, value);
  } else if constexpr (traits_resizable<Container>) {
    auto offset = std::ranges::ssize(container);
    container_traits<Container>::resize(container, size);
    std::ranges::fill(std::ranges::begin(container) + offset, std::ranges::end(container), value);
  } else {
    auto old_size = std::ranges::size(container);
    if (size <= old_size) {
      erase_after(container,
                  std::ranges::begin(container) + static_cast<std::ranges::range_difference_t<Container>>(old_size));
    } else {
      append(container, generate_n(size - old_size, [&value](auto) { return value; }));
    }
  }
}

template <resizable Container>
  requires(!traits_resizable_fill<Container> || std::is_default_constructible_v<std::ranges::range_value_t<Container>>)
constexpr void resize(Container& container, std::ranges::range_size_t<Container> size) {
  if constexpr (traits_resizable<Container>) {
    container_traits<Container>::resize(container, size);
  } else {
    if (!std::ranges::empty(container)) {
      container_traits<Container>::resize(container, size, front(container));
    } else {
      container_traits<Container>::resize(container, size, make_constructor<std::ranges::range_value_t<Container>>());
    }
  }
}
}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
