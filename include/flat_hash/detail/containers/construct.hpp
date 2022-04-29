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

#include "../bits.hpp"
#include "../macros.hpp"
#include "container_traits.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {
template <class R>
concept sized_constructible =
    requires {
      requires traits_constructible<R, std::ranges::range_size_t<R>> ||
                   traits_constructible<R, std::ranges::range_size_t<R>, std::ranges::range_value_t<R>> ||
                   traits_constructible<R, std::ranges::range_size_t<R>, allocator_t<R>> ||
                   traits_constructible<R, std::ranges::range_size_t<R>, std::ranges::range_value_t<R>, allocator_t<R>>;
    };

template <class R>
concept constructible_container = std::is_default_constructible_v<R> || sized_constructible<R>;

template <std::ranges::sized_range Container>
  requires constructible_container<Container>
[[nodiscard]] constexpr auto make_container(std::ranges::range_size_t<Container> size [[maybe_unused]],
                                            optional_allocator<Container> allocator
                                            [[maybe_unused]] = optional_allocator<Container>()) -> Container {
  using size_type = std::ranges::range_size_t<Container>;
  using value_type = std::ranges::range_value_t<Container>;
  using allocator_type = optional_allocator<Container>;

  if constexpr (traits_constructible<Container, size_type, allocator_type>) {
    return container_traits<Container>::construct(size, allocator);
  } else if constexpr (traits_constructible<Container, size_type>) {
    return container_traits<Container>::construct(size);
  } else if constexpr (traits_constructible<Container, size_type, value_type, allocator_type>) {
    return container_traits<Container>::construct(size, construct<value_type>(), allocator);
  } else if constexpr (traits_constructible<Container, size_type, value_type>) {
    return container_traits<Container>::construct(size, construct<value_type>());
  } else {
    return construct<Container>();
  }
}

template <std::ranges::sized_range Container>
  requires constructible_container<Container>
[[nodiscard]] constexpr auto make_container(std::ranges::range_size_t<Container> size,
                                            std::ranges::range_value_t<Container> const& value,
                                            optional_allocator<Container> allocator
                                            [[maybe_unused]] = optional_allocator<Container>()) -> Container {
  using size_type = std::ranges::range_size_t<Container>;
  using value_type = std::ranges::range_value_t<Container>;
  using allocator_type = optional_allocator<Container>;

  if constexpr (traits_constructible<Container, size_type, value_type, allocator_type>) {
    return container_traits<Container>::construct(size, value, allocator);
  } else if constexpr (traits_constructible<Container, size_type, value_type>) {
    return container_traits<Container>::construct(size, value);
  } else if constexpr (mutable_range<Container>) {
    Container r = make_container<Container>(size, allocator);
    std::ranges::fill(r, value);
    return r;
  } else {
    return make_container<Container>(size, allocator);
  }
}

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
