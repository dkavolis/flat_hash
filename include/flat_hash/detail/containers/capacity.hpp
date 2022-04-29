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
#include "container_traits.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {
template <std::ranges::sized_range Container>
[[nodiscard]] constexpr auto capacity(Container const& container) noexcept(!traits_capacity<Container> ||
                                                                           traits_nothrow_capacity<Container>)
    -> std::ranges::range_size_t<Container> {
  if constexpr (traits_capacity<Container>) {
    return container_traits<Container>::capacity(container);
  } else {
    return std::ranges::size(container);
  }
}

template <class R>
concept appendable_from_capacity = std::ranges::sized_range<R> && requires(R& r, std::ranges::range_difference_t<R> n) {
                                                                    container_traits<R>::append_from_capacity(r, n);
                                                                  };
template <class R>
concept nothrow_appendable_from_capacity =
    appendable_from_capacity<R> && requires(R& r, std::ranges::range_difference_t<R> n) {
                                     { container_traits<R>::append_from_capacity(r, n) } noexcept;
                                   };

template <appendable_from_capacity R>
constexpr auto append_from_capacity(R& container, std::ranges::range_difference_t<R> n) noexcept(
    nothrow_appendable_from_capacity<R>) {
  container_traits<R>::append_from_capacity(container, n);
}

template <std::ranges::range R>
[[nodiscard]] constexpr auto appendable_begin(R& container) {
  if constexpr (std::ranges::contiguous_range<R> && appendable_from_capacity<R>) {
    return std::ranges::data(container);
  } else {
    return std::ranges::begin(container);
  }
}
}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
