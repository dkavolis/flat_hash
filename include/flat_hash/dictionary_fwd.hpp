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

#include "detail/config.hpp"
#include "dictionary_traits.hpp"

FLAT_HASH_NAMESPACE_BEGIN

template <class Key, class Value, dictionary_traits_for<Key, Value> Traits = dynamic_dictionary_traits<Key, Value>>
class dictionary;

template <std::random_access_iterator KeyIter, std::random_access_iterator ValueIter>
class dictionary_iterator;

template <class Key, class Value, std::size_t Extent = std::dynamic_extent>
using dictionary_view = dictionary<Key, Value, dictionary_view_traits<Key, Value, Extent>>;

template <class Key, class Value, std::size_t N>
using fixed_dictionary = dictionary<Key, Value, fixed_dictionary_traits<Key, Value, N>>;

template <class Key, class Value, std::size_t N>
using inline_dictionary = dictionary<Key, Value, inline_dictionary_traits<Key, Value, N>>;

namespace pmr {
template <class K, class Value>
using dictionary = ::flat_hash::dictionary<K, Value, dynamic_dictionary_traits<K, Value>>;
}  // namespace pmr

/**
 * @brief Check whether a type derives from set
 *
 * @tparam T
 */
template <class T>
concept is_dictionary = detail::is_base_of_template<T, dictionary>::value;

/**
 * @brief Check whether a type derives from set_iterator
 *
 * @tparam T
 */
template <class T>
concept is_dictionary_iterator = detail::is_base_of_template<T, dictionary_iterator>::value;

/**
 * @brief Traits for ranges of unique (key, value) pairs to allow optimizing range insertions into dictionary
 *
 * @tparam T
 */
template <class T>
inline constexpr bool is_unique_map = false;
;
template <class Key, class T, class Traits>
inline constexpr bool is_unique_map<dictionary<Key, T, Traits>> = true;

template <class T>
concept unique_map = std::ranges::range<T> &&
                     (is_dictionary<std::remove_cvref_t<T>> || is_unique_map<std::remove_cvref_t<T>>);

FLAT_HASH_NAMESPACE_END

template <class Key, class Value, class Traits>
inline constexpr bool std::ranges::enable_borrowed_range<flat_hash::dictionary<Key, Value, Traits>> =
    // acts in a similar way to zipped range, std::ranges::zip_view uses conjunction so use it here as well
    std::ranges::enable_borrowed_range<typename Traits::key_container> &&
    std::ranges::enable_borrowed_range<typename Traits::value_container>;
