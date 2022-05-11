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

#include <concepts>
#include <memory>

#include "../macros.hpp"

FLAT_HASH_NAMESPACE_BEGIN

struct no_allocator {
  constexpr friend auto operator<=>(no_allocator, no_allocator) noexcept = default;
};

namespace detail::containers {
// https://en.cppreference.com/w/cpp/named_req/Allocator
template <class T>
concept is_allocator =
    requires {
      typename T::value_type;
      typename std::allocator_traits<T>;
    } &&
    requires(T& alloc, typename std::allocator_traits<T>::pointer p, typename std::allocator_traits<T>::size_type n) {
      { alloc.allocate(n) } -> std::same_as<typename std::allocator_traits<T>::pointer>;
      alloc.deallocate(p, n);
    } && std::equality_comparable<T> && std::is_nothrow_copy_constructible_v<T> &&
    std::is_nothrow_move_constructible_v<T>;

template <class T>
concept member_allocator_type = requires { is_allocator<typename T::allocator_type>; };

template <class T>
concept member_get_allocator = requires(T const& r) {
                                 { r.get_allocator() } -> is_allocator;
                               };
template <class T>
concept member_nothrow_get_allocator = member_get_allocator<T> && requires(T const& r) {
                                                                    { r.get_allocator() } noexcept;
                                                                  };

// trait for inferring allocator type, either from T::allocator_type or T::get_allocator()

template <class T>
struct allocator_type {
  using type = void;
};
template <member_allocator_type T>
struct allocator_type<T> {
  using type = typename T::allocator_type;
};
template <member_get_allocator T>
  requires(!member_allocator_type<T>)
struct allocator_type<T> {
  using type = decltype(std::declval<T const&>().get_allocator());
};

template <class T>
using allocator_t = typename allocator_type<T>::type;
template <class T>
concept allocator_aware = (!std::same_as<void, allocator_t<T>>);

template <class... C>
struct maybe_enable_allocator_type {};
template <allocator_aware C>
struct maybe_enable_allocator_type<C> {
  using allocator_type = allocator_t<C>;
};
template <class C, class... T>
struct maybe_enable_allocator_type<C, T...> : maybe_enable_allocator_type<T...> {};
template <allocator_aware C, class... T>
struct maybe_enable_allocator_type<C, T...> {
  using allocator_type = allocator_t<C>;
};

// optional allocator type for common constructor of (non) allocator aware containers
template <class Container>
struct maybe_allocator_for {
  using type = no_allocator;
};
template <allocator_aware Container>
struct maybe_allocator_for<Container> {
  using type = allocator_t<Container>;
};

template <class T>
concept maybe_allocator = std::same_as<no_allocator, T> || is_allocator<T>;

}  // namespace detail::containers

template <class T>
using optional_allocator = typename detail::containers::maybe_allocator_for<T>::type;

FLAT_HASH_NAMESPACE_END
