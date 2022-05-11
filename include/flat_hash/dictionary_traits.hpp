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

#include "set_traits.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {
template <class T>
using options_value_t = std::ranges::range_value_t<typename T::value_container>;

template <class T>
concept mutable_dictionary_traits = mutable_set_traits<T> && mutable_range<typename T::value_container>;
}  // namespace detail

template <class T>
concept dictionary_traits = (
    set_traits<T> && requires { requires std::ranges::random_access_range<typename T::value_container>; } &&
    requires(T & options, detail::options_key_t<T> const& key) {
      { options.on_missing_key(key) } -> detail::void_or_convertible_to<detail::options_value_t<T>>;
    });

template <class T, class Key, class Value>
concept dictionary_traits_for = (dictionary_traits<T> && set_traits_for<T, Key> &&
                                 requires(T & options, Key const& key) {
                                   { options.on_missing_key(key) } -> detail::void_or_convertible_to<Value>;
                                 });

template <class Key, class Value>
struct dynamic_dictionary_traits : dynamic_set_traits<Key> {
  using value_container = std::vector<Value>;

  constexpr static void on_missing_key(Key const& /* key */) noexcept {}
};

template <class Key, class Value, std::size_t Extent = std::dynamic_extent>
struct dictionary_view_traits : set_view_traits<Key, Extent> {
  using value_container = std::span<Value, Extent>;  // values can be mutable, instead specify from the template

  constexpr static void on_missing_key(Key const& /* key */) noexcept {}
};

template <class Key, class Value, std::size_t N>
struct fixed_dictionary_traits : fixed_set_traits<Key, N> {
  using value_container = std::array<Value, N>;

  constexpr static void on_missing_key(Key const& /* key */) noexcept {}
};

template <class Key, class Value, std::size_t N>
struct inline_dictionary_traits : inline_set_traits<Key, N> {
  using value_container = detail::inline_vector<Value, N>;

  constexpr static void on_missing_key(Key const& /* key */) noexcept {}
};

namespace pmr {
template <class Key, class Value>
struct dynamic_dictionary_traits : dynamic_set_traits<Key> {
  using value_container = std::pmr::vector<Value>;

  constexpr static void on_missing_key(Key const& /* key */) noexcept {}
};
}  // namespace pmr

FLAT_HASH_NAMESPACE_END
