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

#include <functional>
#include <vector>

#include "detail/bits.hpp"
#include "detail/config.hpp"
#include "detail/containers/container_traits.hpp"
#include "detail/containers/resize.hpp"
#include "detail/equal_to.hpp"
#include "detail/inline_vector.hpp"
#include "detail/transparent_hash.hpp"
#include "probing_policy.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {
template <class T>
using options_index_t = std::ranges::range_value_t<typename T::index_container>;

template <class T>
using options_key_t = std::ranges::range_value_t<typename T::key_container>;

template <class T>
concept mutable_set_traits = mutable_range<typename T::key_container> && mutable_range<typename T::index_container>;
}  // namespace detail

/**
 * @brief Check whether a type is valid traits for set
 *
 * @tparam T
 */
template <class T>
concept set_traits = std::is_default_constructible_v<T> &&
                     requires {
                       typename T::key_container;
                       typename T::index_container;
                       typename T::hasher;
                       typename T::key_equal;
                       typename T::probing_policy;
                       requires(
                           // either cannot add/remove elements from the set of require mutation policy
                           !(detail::containers::resizable<typename T::key_container>) ||
                           requires {
                             { T::ordering } -> std::common_reference_with<ordering_policy>;
                           });
                     } && std::ranges::random_access_range<typename T::key_container> &&
                     detail::index_range<typename T::index_container> &&
                     detail::hash_for<typename T::hasher, std::ranges::range_value_t<typename T::key_container>> &&
                     detail::equality_comparator<typename T::key_equal, detail::options_key_t<T>> &&
                     probing::probing_policy<typename T::probing_policy, typename T::index_container> &&
                     (!detail::mutable_set_traits<T> ||
                      // only mutable sets need T::on_duplicate_key
                      requires(T & options, detail::options_key_t<T> const& key) { options.on_duplicate_key(key); });

/**
 * @brief Check whether a type is valid traits for set with a specified key type
 *
 * @tparam T
 * @tparam Key key type
 */
template <class T, class Key>
concept set_traits_for =
    set_traits<T> &&
    (!detail::mutable_set_traits<T> || requires(T & options, Key const& key) { options.on_duplicate_key(key); }) &&
    detail::hash_for<typename T::hasher, Key> && detail::equality_comparator<typename T::key_equal, Key>;

/**
 * @brief Check whether two different set traits types can result in identical lookups.
 *
 * @tparam T
 * @tparam V
 */
template <class T, class V>
concept same_lookup_as = set_traits<T> && set_traits<V> && std::same_as<typename T::hasher, typename V::hasher> &&
                         std::same_as<typename T::key_equal, typename V::key_equal> &&
                         std::same_as<typename T::probing_policy, typename V::probing_policy>;

template <class T>
  requires requires { typename std::remove_cvref_t<T>::traits_type; }
using traits_t = std::remove_cvref_t<T>::traits_type;

/**
 * @brief Default index type for use in hash tables. Using 32 bit unsigned integrals to fit more buckets into cache.
 *
 */
using default_index_type = std::uint32_t;

/**
 * @brief Compute default hash table size for n elements assuming 75% load factor.
 *
 * @param n number of elements
 * @return std::size_t
 */
[[nodiscard]] constexpr auto default_table_size_for(std::size_t n) noexcept -> std::size_t {
  // using max load factor of 75% here
  return std::bit_ceil((4 * n + 2) / 3);
}

/**
 * @brief Default set traits using std::vector as containers
 *
 * @tparam Key
 */
template <class Key>
struct dynamic_set_traits {
  using key_container = std::vector<Key>;
  using index_container = std::vector<default_index_type>;
  using hasher = detail::hash<Key>;
  using key_equal = detail::equal_to;
  using probing_policy = probing::quadratic;

  constexpr static ordering_policy ordering = ordering_policy::preserved;

  static void on_duplicate_key(Key const& key [[maybe_unused]]) noexcept {}
};

/**
 * @brief Set view traits using std::span as containers, compatible with other predefined traits
 *
 * @tparam Key
 * @tparam Extent
 */
template <class Key, std::size_t Extent = std::dynamic_extent>
struct set_view_traits : dynamic_set_traits<Key> {
  using key_container = std::span<Key const, Extent>;
  using index_container = std::span<default_index_type const>;
};

/**
 * @brief Statically sized set traits using std::array as container
 *
 * @tparam Key
 * @tparam N size
 */
template <class Key, std::size_t N>
struct fixed_set_traits : dynamic_set_traits<Key> {
  using key_container = std::array<Key, N>;
  using index_container = std::array<default_index_type, default_table_size_for(N)>;
};

/**
 * @brief Small stack allocated set traits using a custom small vector as value container
 *
 * @tparam Key
 * @tparam N capacity
 */
template <class Key, std::size_t N>
struct inline_set_traits : fixed_set_traits<Key, N> {
  using key_container = detail::inline_vector<Key, N>;
};

namespace pmr {
/**
 * @brief std::vector based set traits using polymorphic allocators
 *
 * @tparam Key
 */
template <class Key>
struct dynamic_set_traits : flat_hash::dynamic_set_traits<Key> {
  using key_container = std::pmr::vector<Key>;
  using index_container = std::pmr::vector<default_index_type>;
};
}  // namespace pmr

namespace detail {
template <set_traits T>
using hash_container_base_t = hash_container_base<typename T::index_container, typename T::probing_policy,
                                                  typename T::hasher, typename T::key_equal>;

/**
 * @brief Whether the key type can be used for lookup
 *
 * @tparam K key type
 * @tparam Set set/dict type
 */
template <class K, class Set>
concept hashed_lookup_key = valid_key<K, typename Set::key_type, typename Set::hasher, typename Set::key_equal>;

}  // namespace detail

FLAT_HASH_NAMESPACE_END
