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
#include "set_traits.hpp"

FLAT_HASH_NAMESPACE_BEGIN

/**
 * @brief Customizable set, by default uses std::vector as underlying containers
 *
 * @tparam Key set value type
 * @tparam Traits set traits
 */
template <class Key, set_traits_for<Key> Traits = dynamic_set_traits<Key>>
class set;

/**
 * @brief Iterator wrapper that always returns const references, mainly useful for std::span<T> and the like for which
 * const_iterator returns mutable references
 *
 * @tparam Iter
 */
template <std::random_access_iterator Iter>
class set_iterator;

/**
 * @brief Immutable set view based on std::span, convertible from any compatible set with all contiguous containers
 *
 * @tparam Key set value type
 * @tparam Extent set size
 */
template <class Key, std::size_t Extent = std::dynamic_extent>
using set_view = set<Key, set_view_traits<Key, Extent>>;

/**
 * @brief Statically sized set based on std::array
 *
 * @tparam Key set value type
 * @tparam N set size
 */
template <class Key, std::size_t N>
using fixed_set = set<Key, fixed_set_traits<Key, N>>;

/**
 * @brief Small dynamic set that never allocates
 *
 * @tparam Key set value type
 * @tparam N maximum set size
 */
template <class Key, std::size_t N>
using inline_set = set<Key, inline_set_traits<Key, N>>;

namespace pmr {
/**
 * @brief Set using std::vector containers with polymorphic allocators
 *
 * @tparam K set value type
 */
template <class K>
using set = ::flat_hash::set<K, dynamic_set_traits<K>>;
}  // namespace pmr

/**
 * @brief Check whether a type derives from set
 *
 * @tparam T
 */
template <class T>
concept is_set = detail::is_base_of_template<T, set>::value;

/**
 * @brief Check whether a type derives from set_iterator
 *
 * @tparam T
 */
template <class T>
concept is_set_iterator = detail::is_base_of_template<T, set_iterator>::value;

/**
 * @brief Traits for ranges of unique values to allow optimizing range insertions into set
 *
 * @tparam T
 */
template <class T>
struct is_unique_range : std::false_type {};
template <class T, class Traits>
struct is_unique_range<set<T, Traits>> : std::true_type {};

// TODO: set slices that are unique ranges

template <class T>
inline constexpr bool is_unique_range_v = is_unique_range<T>::value;

template <class T>
concept unique_range = std::ranges::range<T> &&
                       (is_set<std::remove_cvref_t<T>> || is_unique_range_v<std::remove_cvref_t<T>>);

FLAT_HASH_NAMESPACE_END
