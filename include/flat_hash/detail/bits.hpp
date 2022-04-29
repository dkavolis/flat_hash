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
#include <ranges>
#include <span>
#include <type_traits>

#include "config.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {

template <class T, class...>
using head_t = T;

template <class T>
struct add_const_if_ref {
  // value type so const does nothing
  using type = T;
};
template <class T>
struct add_const_if_ref<T&> {
  using type = T const&;
};
template <class T>
struct add_const_if_ref<T*> {
  using type = T const*;
};

template <class T>
using add_const_if_ref_t = typename add_const_if_ref<T>::type;

template <class T>
concept transparent_functor = requires { typename T::is_transparent; };

template <class T, class Value>
concept hash_for = std::semiregular<T> && requires(T const& hasher, Value v) {
                                            { hasher(v) } -> std::unsigned_integral;
                                          };

template <class T, class Value, class Other = Value>
concept equality_comparator = std::semiregular<T> && requires(T const& comparator, Value a, Other b) {
                                                       { comparator(a, b) } -> std::convertible_to<bool>;
                                                     };

template <class From, class To>
concept void_or_convertible_to = std::convertible_to<From, To> || std::same_as<From, void>;

template <class T1, class T2 = T1>
concept swappable = std::is_swappable_with_v<T1&, T2&>;

template <class T1, class T2 = T1>
concept nothrow_swappable = std::is_nothrow_swappable_with_v<T1&, T2&>;

template <class T, class Key, class Hash, class KeyEq>
concept valid_key = (std::same_as<T, Key> || (transparent_functor<Hash> && transparent_functor<KeyEq> &&
                                              hash_for<Hash, T> && equality_comparator<KeyEq, T, Key>));

template <class T>
struct maybe_empty {
  T value;

  constexpr maybe_empty() noexcept(std::is_nothrow_constructible_v<T>) = default;
  constexpr maybe_empty(T v) noexcept(std::is_nothrow_move_constructible_v<T>) : value(std::move(v)) {}

  constexpr auto get() noexcept -> T& { return value; }
  constexpr auto get() const noexcept -> T const& { return value; }

  constexpr friend void swap(maybe_empty& lhs, maybe_empty& rhs) noexcept(nothrow_swappable<T>)
    requires swappable<T>
  {
    std::ranges::swap(lhs.value, rhs.value);
  }
};

// MSVC doesn't propagate emptyness for no_unique_address even if a single member is empty and marked with
// no_unique_address so use inheritance instead and move constructing an empty member with no_unique_address may
// overwrite valid memory...
template <class T>
  requires(std::is_empty_v<T>)
struct maybe_empty<T> : public T {
  constexpr maybe_empty() noexcept(std::is_nothrow_default_constructible_v<T>) = default;
  constexpr maybe_empty(T /*unused*/) noexcept(std::is_nothrow_default_constructible_v<T>){};

  constexpr auto get() noexcept -> T& { return *this; }
  constexpr auto get() const noexcept -> T const& { return *this; }

  constexpr friend void swap(maybe_empty /*unused*/, maybe_empty /*unused*/) noexcept {
    // nothing to swap
  }
};

template <class T>
concept pointer = std::is_pointer_v<T>;

template <class T>
concept smart_pointer = requires(T& ptr) {
                          typename T::element_type;
                          { ptr.get() } -> pointer;
                        };

template <class T, class V>
concept smart_pointer_to = smart_pointer<T> && requires(T& ptr) {
                                                 { ptr.get() } -> std::convertible_to<V>;
                                               };

template <class T>
struct is_static_sized : std::false_type {
  constexpr static std::size_t size = std::dynamic_extent;
};
template <class T, std::size_t N>
struct is_static_sized<std::array<T, N>> : std::true_type {
  constexpr static std::size_t size = N;
};
template <class T, std::size_t N>
  requires(N != std::dynamic_extent)
struct is_static_sized<std::span<T, N>> : std::true_type {
  constexpr static std::size_t size = N;
};
template <class T, std::size_t N>
struct is_static_sized<T[N]> : std::true_type {
  constexpr static std::size_t size = N;
};

template <class T>
concept static_sized = is_static_sized<T>::value && requires {
                                                      { is_static_sized<T>::size } -> std::convertible_to<std::size_t>;
                                                    } && (is_static_sized<T>::size > 0);

template <class T>
constexpr static std::size_t static_size_v = is_static_sized<T>::size;

template <class C>
concept index_range = std::ranges::random_access_range<C> && std::unsigned_integral<std::ranges::range_value_t<C>> &&
                      // ensure static size containers are a power of two
                      (!static_sized<C> || std::has_single_bit(static_size_v<C>));

template <class Derived, template <class...> class T>
struct is_base_of_template {
 private:
  template <class... Args>
  static auto derives(T<Args...> const&) -> std::true_type;
  static auto derives(...) -> std::false_type;

 public:
  constexpr static bool value = decltype(derives(std::declval<Derived>()))::value;
};

}  // namespace detail

FLAT_HASH_NAMESPACE_END

#if __has_include(<gsl/span>)
#  include <gsl/span>

FLAT_HASH_NAMESPACE_BEGIN

template <class T, std::size_t N>
  requires(N != gsl::dynamic_extent)
struct is_static_sized<gsl::span<T, N>> : std::true_type {
  constexpr static std::size_t size = N;
};

FLAT_HASH_NAMESPACE_END

#endif
