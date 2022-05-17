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
#include "stl_container_traits.hpp"

FLAT_HASH_NAMESPACE_BEGIN

/**
 * @brief Customization traits struct for custom containers, inherit from stl_container_traits<T> when customizing
 * if your containers implements at least some of std::vector compatible API.
 *
 * @tparam T
 */
template <class T>
struct container_traits {};

template <std::ranges::random_access_range R>
struct container_traits<R> : stl_container_traits<R> {};

namespace detail::containers {
// concepts for container traits functionality

// signature: size_type[, value_type const&][, allocator_type const&]
template <class R, class... Args>
concept traits_constructible = requires(Args... args) {
                                 {
                                   container_traits<R>::construct(std::forward<Args>(args)...)
                                   } -> std::convertible_to<R>;
                               };
template <class R, class... Args>
concept traits_nothrow_constructible = requires(Args... args) {
                                         {
                                           container_traits<R>::construct(std::forward<Args>(args)...)
                                           } noexcept -> std::convertible_to<R>;
                                       };

template <class R>
concept traits_resizable = requires(R& r) { container_traits<R>::resize(r, 1); };

template <class R>
concept traits_resizable_fill =
    requires(R& r, std::ranges::range_value_t<R> const& value) { container_traits<R>::resize(r, 1, value); };

template <class R>
concept traits_reservable = requires(R& r) { container_traits<R>::reserve(r, 1); };

template <class R>
concept traits_capacity = requires(R const& r) {
                            { container_traits<R>::capacity(r) } -> std::convertible_to<std::ranges::range_size_t<R>>;
                          };
template <class R>
concept traits_nothrow_capacity = requires(R const& r) {
                                    {
                                      container_traits<R>::capacity(r)
                                      } noexcept -> std::convertible_to<std::ranges::range_size_t<R>>;
                                  };

template <class R>
concept traits_get_allocator = requires(R const& r) {
                                 {
                                   container_traits<R>::get_allocator(r)
                                   } noexcept -> std::convertible_to<allocator_t<R>>;
                               };

template <class R>
concept traits_clearable = requires(R& r) { container_traits<R>::clear(r); };
template <class R>
concept traits_nothrow_clearable = requires(R& r) {
                                     { container_traits<R>::clear(r) } noexcept;
                                   };

template <class R, class... Args>
concept traits_assignable =
    requires(R& r, Args... args) { container_traits<R>::assign(r, std::forward<Args>(args)...); };

template <class R, class... Args>
concept traits_back_emplaceable =
    requires(R& r, Args... args) { container_traits<R>::emplace_back(r, std::forward<Args>(args)...); };

template <class R>
concept traits_erasable = requires(R& r, std::ranges::iterator_t<R const> pos) { container_traits<R>::erase(r, pos); };

template <class R>
concept traits_range_erasable =
    requires(R& r, std::ranges::iterator_t<R const> first, std::ranges::iterator_t<R const> last) {
      container_traits<R>::erase(r, first, last);
    };

template <class R, class... Args>
concept traits_insertible = requires(R& r, std::ranges::iterator_t<R const> pos, Args... args) {
                              container_traits<R>::insert(r, pos, std::forward<Args>(args)...);
                            };

template <class R>
concept traits_back_popable = requires(R& r) { container_traits<R>::pop_back(r); };

template <class R>
concept traits_front_popable = requires(R& r) { container_traits<R>::pop_front(r); };

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
