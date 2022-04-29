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

template <class Container>
concept reservable = traits_reservable<Container>;

template <std::ranges::sized_range R>
  requires reservable<R>
constexpr void reserve(R& container, std::ranges::range_size_t<R> size) {
  container_traits<R>::reserve(container, size);
}

[[nodiscard]] constexpr auto growth_size(std::size_t current_capacity, std::size_t current_size,
                                         std::size_t extra_elements) noexcept -> std::size_t {
  return std::max(current_size + extra_elements, current_capacity * 2);
}

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
