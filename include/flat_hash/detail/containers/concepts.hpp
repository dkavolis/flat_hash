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

#include <utility>

#include "../macros.hpp"
#include "allocators.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {

template <class T>
concept mutable_range = std::ranges::range<T> && std::ranges::output_range<T, std::ranges::range_value_t<T>>;

template <class T, class... Args>
concept brace_constructible = requires(Args... args) { T{std::forward<Args>(args)...}; };

template <class T, class... Args>
concept nothrow_brace_constructible = requires(Args... args) {
                                        { T{std::forward<Args>(args)...} } noexcept;
                                      };

template <class T, class Arg>
concept subscriptable = requires(T& container, Arg arg) { container[arg]; };

template <class T>
concept mutable_ = !
std::is_const_v<T>;

namespace containers {

// common container methods (based on stl containers)

template <class T, class... Args>
concept member_resize = requires(T& container, Args... args) { container.resize(std::forward<Args>(args)...); };

template <class T, class... Args>
concept member_reserve = requires(T& container, Args... args) { container.reserve(std::forward<Args>(args)...); };

template <class T, class... Args>
concept member_assign = requires(T& container, Args... args) { container.assign(std::forward<Args>(args)...); };

template <class T>
concept member_pop_back = requires(T& r) { r.pop_back(); };
template <class T>
concept member_nothrow_pop_back = requires(T& r) {
                                    { r.pop_back() } noexcept;
                                  };

template <class T>
concept member_pop_front = requires(T& r) { r.pop_front(); };
template <class T>
concept member_nothrow_pop_front = requires(T& r) {
                                     { r.pop_front() } noexcept;
                                   };

template <class T>
concept member_clear = requires(T& r) { r.clear(); };
template <class T>
concept member_nothrow_clear = requires(T& r) {
                                 { r.clear() } noexcept;
                               };

template <class T>
concept member_capacity = std::ranges::sized_range<T> && requires(T& r) {
                                                           {
                                                             r.capacity()
                                                             } -> std::convertible_to<std::ranges::range_size_t<T>>;
                                                         };
template <class T>
concept member_nothrow_capacity =
    std::ranges::sized_range<T> && requires(T& r) {
                                     { r.capacity() } noexcept -> std::convertible_to<std::ranges::range_size_t<T>>;
                                   };

template <class T, class... Args>
concept member_push_back = requires(T& container, Args... args) { container.push_back(std::forward<Args>(args)...); };

template <class T, class... Args>
concept member_emplace_back =
    requires(T& container, Args... args) { container.emplace_back(std::forward<Args>(args)...); };

template <class T, class... Args>
concept member_add = requires(T& container, Args... args) { container.add(std::forward<Args>(args)...); };

template <class T, class... Args>
concept member_insert = requires(T& container, std::ranges::iterator_t<T const> pos, Args... args) {
                          container.insert(pos, std::forward<Args>(args)...);
                        };

template <class T, class... Args>
concept member_emplace = requires(T& container, std::ranges::iterator_t<T const> pos, Args... args) {
                           container.emplace(pos, std::forward<Args>(args)...);
                         };

template <class T, class... Args>
concept member_remove = requires(T& container, Args... args) { container.remove(std::forward<Args>(args)...); };

template <class T, class... Args>
concept member_erase = requires(T& container, Args... args) { container.erase(std::forward<Args>(args)...); };

template <class T>
concept member_append_from_capacity =
    requires(T& container, std::ranges::range_difference_t<T> n) { container.append_from_capacity(n); };
template <class T>
concept member_nothrow_append_from_capacity = requires(T& container, std::ranges::range_difference_t<T> n) {
                                                { container.append_from_capacity(n) } noexcept;
                                              };

}  // namespace containers
}  // namespace detail

FLAT_HASH_NAMESPACE_END
