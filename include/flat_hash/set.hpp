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

#include <ranges>
#include <span>
#include <type_traits>
#include <utility>

#include "detail/bits.hpp"
#include "detail/containers.hpp"
#include "detail/hash_table.hpp"
#include "set_fwd.hpp"
#include "set_traits.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {

/**
 * @brief Whether the values stored by the set can be mutated
 *
 * @tparam Set set type
 */
template <class Set>
concept mutable_set = is_set<Set> &&
                      (mutable_range<typename Set::key_container> && mutable_range<typename Set::index_container>);

/**
 * @brief Whether the key type can be used for lookup
 *
 * @tparam K key type
 * @tparam Set set type
 */
template <class K, class Set>
concept set_lookup_key =
    is_set<Set> && valid_key<K, typename Set::value_type, typename Set::hasher, typename Set::key_equal>;

/**
 * @brief Whether the key type can be used for adding to the set
 *
 * @tparam K key type
 * @tparam Set set type
 */
template <class K, class Set>
concept set_addable_key = is_set<Set> && (set_lookup_key<K, Set> && mutable_set<Set> &&
                                          containers::back_emplaceable<typename Set::key_container>);

/**
 * @brief Whether the key type can be used for removal from the set
 *
 * @tparam K key type
 * @tparam Set set type
 */
template <class K, class Set>
concept set_removable_key = is_set<Set> && (set_lookup_key<K, Set> && mutable_set<Set> &&
                                            containers::erasable<typename Set::key_container>);

/**
 * @brief Whether the set can be cleared
 *
 * @tparam Set set type
 */
template <class Set>
concept clearable_set = is_set<Set> &&
                        (mutable_range<typename Set::key_container> && mutable_range<typename Set::index_container> &&
                         containers::clearable<typename Set::key_container>);

/**
 * @brief Whether the set supports reserve
 *
 * @tparam Set set type
 */
template <class Set>
concept reservable_set = is_set<Set> && containers::reservable<typename Set::key_container>;

/**
 * @brief Whether the set can be default constructed. False for statically sized containers (i.e.
 * std::array and std::span<..., N> since they cannot be empty on default construction)
 *
 * @tparam Set
 */
template <class Set>
concept default_constructible_set = (is_set<Set> && !static_sized<typename Set::key_container>);

/**
 * @brief Helper function for computing the lowest number of buckets for specified load_factor
 *
 * @tparam U
 * @tparam UI
 * @param n number of elements
 * @param load_factor desired maximum load factor
 * @return U lowest number of buckets for holding n elements with at most load_factor
 */
template <std::integral U = default_index_type, std::unsigned_integral UI>
[[nodiscard]] constexpr auto at_least(UI n, double load_factor) noexcept -> U {
  return static_cast<U>(std::ceil(static_cast<double>(n) / load_factor));
}

template <class Set, template <class> class Trait>
concept set_apply = Trait<typename Set::traits_type>::value && Trait<typename Set::hasher>::value &&
                    Trait<typename Set::key_equal>::value && Trait<typename Set::hash_table>::value &&
                    Trait<typename Set::key_container>::value;
}  // namespace detail

template <std::random_access_iterator Iter>
class set_iterator : public iterator_facade<set_iterator<Iter>, std::contiguous_iterator<Iter>> {
  template <std::random_access_iterator>
  friend class set_iterator;

 public:
  using value_type = std::iter_value_t<Iter>;
  using reference = detail::add_const_if_ref_t<std::iter_reference_t<Iter>>;
  using difference_type = std::iter_difference_t<Iter>;

  constexpr set_iterator() noexcept(std::is_nothrow_default_constructible_v<Iter>) = default;
  constexpr set_iterator(Iter iter) noexcept(std::is_nothrow_move_constructible_v<Iter>) : iter_(std::move(iter)) {}
  template <class U>
    requires(std::constructible_from<Iter, U> && !std::same_as<set_iterator, std::remove_cvref_t<U>>)
  constexpr explicit(!std::convertible_to<U, Iter>)
      set_iterator(U&& iter) noexcept(std::is_nothrow_constructible_v<Iter, U>)
      : iter_(std::forward<U>(iter)) {}

  template <class U>
    requires(std::constructible_from<Iter, U> && !std::same_as<Iter, U>)
  constexpr set_iterator(set_iterator<U> const& other) noexcept(std::is_nothrow_constructible_v<Iter, U>)
      : iter_(other.iter_) {}

  [[nodiscard]] constexpr auto dereference() const noexcept(nothrow_dereference<Iter>) -> reference { return *iter_; }
  constexpr void increment() noexcept(nothrow_increment<Iter>) { ++iter_; }
  constexpr void decrement() noexcept(nothrow_decrement<Iter>) { --iter_; }
  constexpr void advance(difference_type n) noexcept(nothrow_advance<Iter>) { iter_ += n; }

  template <class S>
  // is_set_iterator is needed to avoid recursive concepts checks
    requires(!is_set_iterator<S> && std::sentinel_for<S, Iter>)
  [[nodiscard]] constexpr auto equals(S const& sentinel) const noexcept(nothrow_equals<Iter, S>) -> bool {
    return iter_ == sentinel;
  }
  template <std::sentinel_for<Iter> S>
  [[nodiscard]] constexpr auto equals(set_iterator<S> const& sentinel) const noexcept(nothrow_equals<Iter, S>) -> bool {
    return equals(sentinel.iter_);
  }

  template <class S>
    requires(!is_set_iterator<S> && std::sized_sentinel_for<S, Iter>)
  [[nodiscard]] constexpr auto distance_to(S const& sentinel) const noexcept(nothrow_distance_to<Iter, S>)
      -> difference_type {
    return sentinel - iter_;
  }
  template <std::sized_sentinel_for<Iter> S>
  [[nodiscard]] constexpr auto distance_to(set_iterator<S> const& sentinel) const noexcept(nothrow_distance_to<Iter, S>)
      -> difference_type {
    return distance_to(sentinel.iter_);
  }

 private:
  Iter iter_;

  template <class K, set_traits_for<K> T>
  friend class set;

  [[nodiscard]] constexpr auto base() const noexcept -> Iter { return iter_; }
};

/**
 * @brief Set initialization parameters, an aggregate to allow constructing with designated initializers
 *
 * @tparam Traits set traits
 */
template <set_traits Traits>
struct set_init {
  using hasher = Traits::hasher;
  using key_equal = Traits::key_equal;
  using probing_policy = Traits::probing_policy;
  using maybe_key_allocator = optional_allocator<typename Traits::key_container>;
  using maybe_index_allocator = optional_allocator<typename Traits::index_container>;

  Traits traits{};
  hasher hash_function{};
  key_equal key_eq{};
  probing_policy policy{};

  // keeping possible allocators as non-optional members for simplicity
  maybe_key_allocator key_allocator{};
  maybe_index_allocator index_allocator{};
};

template <class Key, set_traits_for<Key> Traits>
class set : public detail::containers::maybe_enable_allocator_type<typename Traits::key_container> {
  template <class K, set_traits_for<K> T>
  friend class set;

 public:
  using traits_type = Traits;
  using options_type = set_init<traits_type>;
  using index_container = Traits::index_container;
  using key_container = Traits::key_container;
  using probing_policy = Traits::probing_policy;
  using hash_table = detail::hash_table<index_container, probing_policy>;
  using index_type = hash_table::value_type;
  constexpr static ordering_policy ordering = Traits::ordering;

  using key_type = Key;
  using value_type = Key;
  using size_type = std::uint64_t;
  using difference_type = std::int64_t;
  using hasher = Traits::hasher;
  using key_equal = Traits::key_equal;
  using hash_type = decltype(hasher{}(std::declval<key_type const&>()));
  // allocator_type...
  // keys are not mutable
  using iterator = set_iterator<std::ranges::iterator_t<key_container const>>;
  using const_iterator = iterator;
  using reference = iterator::reference;
  using const_reference = reference;

  /**
   * @brief Default constructor, will not allocate unless one of the types from Traits allocates in their default
   * constructor
   */
  constexpr set()
    requires(detail::default_constructible_set<set>)
  {
    FLAT_HASH_ASSERT(empty(), "set is not empty: {}", detail::maybe_format_arg(*this));
  }

  /**
   * @brief Construct an empty set with no buckets allocated
   *
   * @param init set initialization options
   */
  constexpr explicit set(set_init<Traits> init)
    requires(detail::default_constructible_set<set>)
  : traits_(std::move(init.traits)),
    hash_(init.hash_function),
    key_eq_(init.key_eq),
    hash_table_(0, std::move(init.policy), init.index_allocator),
    keys_(detail::containers::make_container<key_container>(0, init.key_allocator)) {
    FLAT_HASH_ASSERT(empty(), "set is not empty: {}", detail::maybe_format_arg(*this));
  }

  /**
   * @brief Construct a set with a given number of buckets and traits, static size index containers may ignore the
   * bucket_count. If possible, will reserve storage for keys.
   *
   * @param bucket_count number of buckets
   * @param init set initialization options
   */
  constexpr explicit set(size_type bucket_count, set_init<Traits> init = set_init<Traits>())
    requires(std::constructible_from<hash_table, size_type> && !detail::static_sized<key_container>)
  : traits_(std::move(init.traits)),
    hash_(init.hash_function),
    key_eq_(init.key_eq),
    hash_table_(bucket_count, std::move(init.policy), init.index_allocator),
    keys_(detail::containers::make_container<key_container>(0, init.key_allocator)) {
    if constexpr (detail::containers::reservable<key_container>) {
      detail::containers::reserve(
          keys_, static_cast<size_type>(max_load_factor() * static_cast<double>(this->bucket_count())));
    }
    FLAT_HASH_ASSERT(empty(), "set is not empty: {}", detail::maybe_format_arg(*this));
  }

  /**
   * @brief Construct a set from an initializer list, requires the set to be mutable.
   *
   * @param ilist values, may contain duplicates
   * @param init set initialization options
   */
  constexpr set(std::initializer_list<Key> ilist, set_init<Traits> init = set_init<Traits>())
    requires(detail::mutable_range<index_container>)
  : traits_(std::move(init.traits)),
    hash_(init.hash_function),
    key_eq_(init.key_eq),
    hash_table_(detail::at_least<size_type>(ilist.size(), init.policy.max_load_factor()), std::move(init.policy),
                init.index_allocator),
    keys_(detail::containers::make_container<key_container>(0, init.key_allocator)) {
    if constexpr (detail::containers::back_emplaceable<key_container, Key>) {
      merge(ilist);
    } else {
      assign(ilist);
    }
  }

  /**
   * @brief Construct a set from a range of keys, requires the set to be mutable.
   *
   * @tparam R input range with values convertible to Key
   *
   * @param r values, may contain duplicates
   * @param init set initialization options
   */
  template <std::ranges::input_range R>
    requires(std::constructible_from<Key, std::ranges::range_value_t<R>> && detail::mutable_range<index_container> &&
             !std::same_as<set, std::remove_cvref_t<R>>)
  constexpr explicit set(R&& r, set_init<Traits> init = set_init<Traits>())
      : traits_(std::move(init.traits)),
        hash_(init.hash_function),
        key_eq_(init.key_eq),
        hash_table_(detail::at_least<size_type>(detail::containers::size_hint_or(r, hash_table::default_size / 2),
                                                init.policy.max_load_factor()),
                    std::move(init.policy), init.index_allocator),
        keys_(detail::containers::make_container<key_container>(0, init.key_allocator)) {
    if constexpr (detail::containers::back_emplaceable<key_container, std::ranges::range_value_t<R>>) {
      merge(std::forward<R>(r));
    } else {
      assign(std::forward<R>(r));
    }
  }

  template <std::input_iterator It>
    requires(std::constructible_from<Key, std::iter_value_t<It>> && detail::mutable_range<index_container>)
  constexpr explicit set(It first, It last, set_init<Traits> init = set_init<Traits>())
      : set(std::ranges::subrange(first, last), std::move(init)) {}

  /**
   * @brief Converting constructor from a compatible set reference, implicitly convertible if containers are also
   * implicitly convertible.
   *
   * @tparam S compatible set type
   *
   * @param other other set
   * @param traits traits to use for this set
   */
  template <class T>
    requires(same_lookup_as<Traits, T> && !std::same_as<T, Traits> &&
             std::constructible_from<key_container, typename T::key_container&> &&
             std::constructible_from<hash_table, typename set<Key, T>::hash_table&> &&
             std::constructible_from<hasher, typename T::hasher&> &&
             std::constructible_from<key_equal, typename T::key_equal&>)
  constexpr explicit(!(std::convertible_to<typename T::key_container&, key_container> &&
                       std::convertible_to<typename set<Key, T>::hash_table&, hash_table>))
      set(set<Key, T>& other, Traits t = Traits()) noexcept(
          (std::is_nothrow_move_constructible_v<Traits> &&
           std::is_nothrow_constructible_v<key_container, typename T::key_container&> &&
           std::is_nothrow_constructible_v<hash_table, typename set<Key, T>::hash_table&> &&
           std::is_nothrow_constructible_v<hasher, typename T::hasher&> &&
           std::is_nothrow_constructible_v<key_equal, typename T::key_equal&>))
      : traits_(std::move(t)),
        hash_(other.hash_),
        key_eq_(other.key_eq_),
        hash_table_(other.hash_table_),
        keys_(other.keys_) {}

  /**
   * @copydoc set::set(set<Key,T>&, Traits)
   */
  template <class T>
    requires(same_lookup_as<Traits, T> && !std::same_as<T, Traits> &&
             std::constructible_from<key_container, typename T::key_container const&> &&
             std::constructible_from<hash_table, typename set<Key, T>::hash_table const&> &&
             std::constructible_from<hasher, typename T::hasher const&> &&
             std::constructible_from<key_equal, typename T::key_equal const&>)
  constexpr explicit(!(std::convertible_to<typename T::key_container const&, key_container> &&
                       std::convertible_to<typename set<Key, T>::hash_table const&, hash_table>))
      set(set<Key, T> const& other, Traits t = Traits()) noexcept(
          (std::is_nothrow_move_constructible_v<Traits> &&
           std::is_nothrow_constructible_v<key_container, typename T::key_container const&> &&
           std::is_nothrow_constructible_v<hash_table, typename set<Key, T>::hash_table const&> &&
           std::is_nothrow_constructible_v<hasher, typename T::hasher const&> &&
           std::is_nothrow_constructible_v<key_equal, typename T::key_equal const&>))
      : traits_(std::move(t)),
        hash_(other.hash_),
        key_eq_(other.key_eq_),
        hash_table_(other.hash_table_),
        keys_(other.keys_) {}

  /**
   * @brief Default copy constructor
   *
   */
  constexpr set(set const& other) noexcept(detail::set_apply<set, std::is_nothrow_copy_constructible>) = default;

  /**
   * @brief Default move constructor
   *
   */
  constexpr set(set&& other) noexcept(detail::set_apply<set, std::is_nothrow_move_constructible>) = default;

  /**
   * @brief Default copy assignment operator
   *
   * @return set&
   */
  constexpr auto operator=(set const&) noexcept(detail::set_apply<set, std::is_nothrow_copy_assignable>)
      -> set& = default;

  /**
   * @brief Default move assignment operator
   *
   * @return set&
   */
  constexpr auto operator=(set&&) noexcept(detail::set_apply<set, std::is_nothrow_move_assignable>) -> set& = default;

  /**
   * @brief Default destructor
   *
   */
  constexpr ~set() noexcept(detail::set_apply<set, std::is_nothrow_destructible>) = default;

  /**
   * @brief Assign values to set
   *
   * @param values values to assign, may contain duplicates
   *
   * @return reference to this
   */
  constexpr auto operator=(std::initializer_list<value_type> values) -> set&
    requires((detail::clearable_set<set> || !detail::containers::back_emplaceable<key_container, value_type const&>) &&
             detail::mutable_set<set>)
  {
    if constexpr (detail::containers::back_emplaceable<key_container, value_type const&>) {
      clear();
      merge(values);
    } else {
      if (!empty()) { hash_table_.clear(); }
      assign(values);
    }
    return *this;
  }

  /**
   * @brief Get allocator used by the key_container.
   *
   */
  [[nodiscard]] constexpr auto get_allocator() const
      noexcept(detail::containers::nothrow_gettable_allocator<key_container>)
          -> detail::containers::allocator_t<key_container>
    requires(detail::containers::gettable_allocator<key_container>)
  {
    return detail::containers::get_allocator(keys_);
  }

  /**
   * @brief Get allocator used by the hash_table and index_container.
   *
   */
  [[nodiscard]] constexpr auto get_hash_table_allocator() const
      noexcept(detail::containers::nothrow_gettable_allocator<index_container>)
          -> detail::containers::allocator_t<hash_table>
    requires(detail::containers::gettable_allocator<index_container>)
  {
    return hash_table_.get_allocator();
  }

  // Iterators, keys are immutable

  /**
   * @brief Iterator the beginning of the stored keys.
   *
   * @return const_iterator
   */
  [[nodiscard]] constexpr auto cbegin() const noexcept -> const_iterator { return std::ranges::cbegin(keys_); }

  /**
   * @brief Iterator the end of the stored keys.
   *
   * @return const_iterator
   */
  [[nodiscard]] constexpr auto cend() const noexcept -> const_iterator { return std::ranges::cend(keys_); }

  /**
   * @copydoc set::cbegin()
   */
  [[nodiscard]] constexpr auto begin() const noexcept -> const_iterator { return cbegin(); }

  /**
   * @copydoc set::cend()
   */
  [[nodiscard]] constexpr auto end() const noexcept -> const_iterator { return cend(); }

  // Capacity

  /**
   * @brief Check if there are no keys stored
   *
   * @return true if no keys are stored
   * @return false otherwise
   */
  [[nodiscard]] constexpr auto empty() const noexcept -> bool { return std::ranges::empty(keys_); }

  /**
   * @brief Number of keys stored
   *
   * @return size_type
   */
  [[nodiscard]] constexpr auto size() const noexcept -> size_type { return std::ranges::size(keys_); }

  /**
   * @copydoc set::size()
   */
  [[nodiscard]] constexpr auto ssize() const noexcept -> difference_type { return std::ranges::ssize(keys_); }

  /**
   * @brief Capacity of the key_container, may fall back to container size.
   *
   * @return size_type
   */
  [[nodiscard]] constexpr auto capacity() const noexcept -> size_type { return detail::containers::capacity(keys_); }

  /**
   * @brief Maximum number of keys that can be stored. Depends on the index type size and probing_policy used assuming
   * unlimited memory.
   *
   * @return size_type
   */
  [[nodiscard]] constexpr auto max_size() const noexcept -> size_type {
    auto [reserved_values, reserved_bits] = hash_table_.reserved();
    constexpr auto indexing_bytes = static_cast<std::uint8_t>(std::min(sizeof(hash_type), sizeof(index_type)));

    return (size_type{1} << (indexing_bytes * CHAR_BIT - reserved_bits)) - reserved_values;
  }

  // Modifiers

  /**
   * @brief Clear the key_container and reset all buckets to empty.
   *
   */
  constexpr void clear() noexcept
    requires detail::clearable_set<set>
  {
    // hash_table may not use dynamic container but that doesn't matter
    if (empty()) { return; }
    detail::containers::clear(keys_);
    hash_table_.clear();
  }

  /**
   * @brief Try inserting a key value at the specified position
   *
   * @param pos position to insert the value at
   * @param key value to insert
   * @return std::pair<iterator, bool> iterator to the inserted value or the element preventing insertion, true if
   * inserted and false otherwise
   */
  constexpr auto try_insert(const_iterator pos, key_type&& key) -> std::pair<iterator, bool> {
    return try_insert_impl(pos, key, std::true_type{});
  }

  /**
   * @copydoc set::try_insert(const_iterator, key_type&&)
   */
  constexpr auto try_insert(const_iterator pos, key_type const& key) -> std::pair<iterator, bool> {
    return try_insert_impl(pos, key, std::false_type{});
  }

  /**
   * @brief Try inserting a value at the specified position. Heterogeneous lookup overload.
   *
   * @copydetail set::try_insert(const_iterator, key_type&&)
   */
  template <detail::set_addable_key<set> K>
  constexpr auto try_insert(const_iterator pos, K&& key) -> std::pair<iterator, bool> {
    return try_insert_impl(pos, key, std::bool_constant<!std::is_lvalue_reference_v<K>>{});
  }

  /**
   * @brief Insert a new value. The behaviour on duplicate values depends on the supplied traits
   * Traits::on_duplicate_value(key_type const&)
   *
   * @param pos position to insert value at
   * @param key value to insert
   * @return iterator Iterator to the inserted value or the element preventing insertion
   */
  constexpr auto insert(const_iterator pos, key_type&& key) -> iterator {
    return insert_impl(pos, key, std::true_type{});
  }

  /**
   * @copydoc set::insert(const_iterator, key_type&&)
   */
  constexpr auto insert(const_iterator pos, key_type const& key) -> iterator {
    return insert_impl(pos, key, std::false_type{});
  }

  /**
   * @copybrief set::insert(const_iterator, key_type&&)
   * Heterogeneous lookup overload.
   *
   * @copydetail set::insert(const_iterator, key_type&&)
   */
  template <detail::set_addable_key<set> K>
  constexpr auto insert(const_iterator pos, K&& key) -> iterator {
    return insert_impl(pos, key, std::bool_constant<!std::is_lvalue_reference_v<K>>{});
  }

  /**
   * @brief Try inserting new value to the end of the set.
   *
   * @param key value to insert
   * @return std::pair<iterator, bool> iterator to the inserted value or the element preventing insertion, true if
   * inserted and false otherwise
   */
  constexpr auto try_insert(key_type&& key) -> std::pair<iterator, bool> { return try_insert(cend(), std::move(key)); }

  /**
   * @copydoc set::try_insert(key_type&& key)
   */
  constexpr auto try_insert(key_type const& key) -> std::pair<iterator, bool> { return try_insert(cend(), key); }

  /**
   * @copybrief set::try_insert(key_type&&)
   * Heterogeneous lookup overload.
   *
   * @copydetail set::try_insert(key_type&&)
   */
  template <detail::set_addable_key<set> K>
  constexpr auto try_insert(K&& key) -> std::pair<iterator, bool> {
    return try_insert(cend(), std::forward<K>(key));
  }

  /**
   * @copydoc set::try_insert(key_type&&)
   */
  constexpr auto insert(key_type&& key) -> std::pair<iterator, bool> { return try_insert(cend(), std::move(key)); }

  /**
   * @copydoc set::try_insert(key_type const&)
   */
  constexpr auto insert(key_type const& key) -> std::pair<iterator, bool> { return try_insert(cend(), key); }

  /**
   * @copydoc set::try_insert(K&&)
   */
  template <detail::set_addable_key<set> K>
  constexpr auto insert(K&& key) -> std::pair<iterator, bool> {
    return try_insert(cend(), std::forward<K>(key));
  }

  /**
   * @brief Insert a range of values at the end of the key_container. Duplicates are ignored and values are consumed in
   * the order of iteration.
   *
   * @tparam It
   * @tparam S
   * @param first iterator the beginning of the range
   * @param last to range end
   */
  template <std::input_iterator It, std::sentinel_for<It> S>
    requires(detail::set_addable_key<std::iter_value_t<It>, set>)
  constexpr void insert(It first, S last) {
    insert(std::ranges::subrange(first, last));
  }

  /**
   * @copybrief set::insert(It, S)
   *
   * @tparam R
   * @param range range of values
   */
  template <std::ranges::input_range R>
    requires(detail::set_addable_key<std::ranges::range_value_t<R>, set>)
  constexpr void insert(R&& range) {
    merge(std::forward<R>(range));
  }

  /**
   * @brief Insert value at specified position. Unlike std::set and std::unordered_set, the first argument is not a hint
   * but a position to insert the element at.
   *
   * @tparam Args
   * @param pos position to insert value at
   * @param args key_type constructor arguments
   *
   * @return std::pair<iterator, bool> iterator to the inserted element or the element preventing insertion, true if a
   * new value was inserted and false otherwise
   */
  template <class... Args>
    requires(detail::containers::constructible_from<value_type, Args...> && detail::set_addable_key<value_type, set>)
  constexpr auto try_emplace_hint(const_iterator pos, Args&&... args) -> std::pair<iterator, bool> {
    // TODO: emplace back and remove on throw
    return try_insert(pos, detail::containers::construct<value_type>(std::forward<Args>(args)...));
  }

  /**
   * @brief Insert value at the end of key_container. Unlike std::set and std::unordered_set, the first argument is not
   * a hint but a position to insert the element at.
   *
   * @copydetail set::try_emplace_hint(const_iterator, Args&&)
   */
  template <class... Args>
    requires(detail::containers::constructible_from<value_type, Args...> && detail::set_addable_key<value_type, set>)
  constexpr auto try_emplace(Args&&... args) -> std::pair<iterator, bool> {
    return try_emplace_hint(cend(), std::forward<Args>(args)...);
  }

  /**
   * @copybrief set::try_emplace_hint(const_iterator, Args&&)
   *
   * @tparam Args
   * @param pos position to insert value at
   * @param args key_type constructor arguments
   * @return iterator to the inserted element or the element preventing insertion
   */
  template <class... Args>
    requires(detail::containers::constructible_from<value_type, Args...> && detail::set_addable_key<value_type, set>)
  constexpr auto emplace_hint(const_iterator pos, Args&&... args) -> iterator {
    return try_emplace_hint(pos, std::forward<Args>(args)...).first;
  }

  /**
   * @brief set::try_emplace(Args&&)
   *
   * @copydetail set::emplace_hint(const_iterator, Args&&)
   */
  template <class... Args>
    requires(detail::containers::constructible_from<value_type, Args...> && detail::set_addable_key<value_type, set>)
  constexpr auto emplace(Args&&... args) -> std::pair<iterator, bool> {
    return try_emplace_hint(cend(), std::forward<Args>(args)...);
  }

  /**
   * @brief Remove value from the set
   *
   * @param key value to remove
   * @return 1 if a value matching key was removed
   * @return 0 otherwise
   */
  constexpr auto erase(key_type const& key) -> size_type
    requires detail::set_removable_key<value_type, set>
  {
    return extract(key) ? 1 : 0;
  }

  /**
   * @copybrief set::erase(key_type const&)
   * Heterogeneous overload.
   *
   * @copydetail set::erase(key_type const&)
   */
  template <detail::set_removable_key<set> K>
  constexpr auto erase(K const& key) -> size_type {
    return extract(key) ? 1 : 0;
  }

  /**
   * @brief Erase element at the specified position
   *
   * @param pos position to remove
   * @return iterator to the element after pos
   */
  constexpr auto erase(const_iterator pos) -> iterator
    requires detail::set_removable_key<value_type, set>
  {
    auto offset = pos - cbegin();
    extract(pos);
    return begin() + offset;
  }

  /**
   * @brief Erase elements in range [first, last)
   *
   * @param first beginning of the range
   * @param last end of the range
   * @return iterator to the first element past removed ones
   */
  constexpr auto erase(const_iterator first, const_iterator last) -> iterator
    requires detail::set_removable_key<value_type, set>
  {
    auto offset = first - cbegin();
    if (last != first) {
      if (last - first == 1) {
        erase(*first);
      } else {
        // TODO: for low numbers of elements to erase erase them one by one?
        auto beg = std::ranges::cbegin(keys_);
        detail::containers::policy_erase<ordering>(keys_, beg + offset, beg + (last - cbegin()));
        rehash(0);
      }
    }

    return begin() + offset;
  }

  /**
   * @brief Swap two sets.
   *
   */
  constexpr void swap(set& rhs) noexcept((detail::nothrow_swappable<Traits> && detail::nothrow_swappable<hasher> &&
                                          detail::nothrow_swappable<key_equal> &&
                                          detail::nothrow_swappable<hash_table> &&
                                          detail::nothrow_swappable<key_container>))
    requires(detail::swappable<Traits> && detail::swappable<hasher> && detail::swappable<key_equal> &&
             detail::swappable<hash_table> && detail::swappable<key_container>)
  {
    if (this == &rhs) [[unlikely]] { return; }
    std::ranges::swap(traits_, rhs.traits_);
    std::ranges::swap(hash_, rhs.hash_);
    std::ranges::swap(key_eq_, rhs.key_eq_);
    std::ranges::swap(hash_table_, rhs.hash_table_);
    std::ranges::swap(keys_, rhs.keys_);
  }

  /**
   * @brief Extract the specified value. Unlike standard containers, the return type is not a node handle.
   *
   * @tparam K
   * @param key value to extract
   * @return std::optional<value_type> the extracted value or nothing if it was not found
   */
  template <detail::set_removable_key<set> K>
  constexpr auto extract(K const& key) -> std::optional<value_type> {
    auto bucket_iterator = find_bucket(key);
    return extract_impl(bucket_iterator);
  }

  /**
   * @brief Extract the element at the specified position
   *
   * @param pos iterator to the element to extract
   * @return value_type extracted element
   */
  constexpr auto extract(const_iterator pos) -> value_type
    requires detail::set_removable_key<value_type, set>
  {
    FLAT_HASH_ASSERT(pos != cend());
    auto bucket_iterator = find_bucket(pos);

    // if pos is valid it will always have value
    return extract_impl(bucket_iterator).value();
  }

  /**
   * @brief Add elements from the range to the end of key_container. Ranges for which
   * std::ranges::borrowed_range<R>::value == false will have their values consumed (moved from). Unlike
   * std::unordered_set, merge does not splice two sets, use set::splice in that case.
   *
   * @tparam R
   * @param source range of values, unique_range containers may be faster.
   */
  template <std::ranges::input_range R>
    requires(detail::set_addable_key<std::ranges::range_value_t<R>, set>)
  constexpr void merge(R&& source) {
    try_reserve_for(source);

    if constexpr (unique_range<R>) {
      merge_unique(std::forward<R>(source));
    } else {
      merge_any(std::forward<R>(source));
    }
  }

  /**
   * @brief Combine two sets by moving values from other into this. other is left in a valid state without the
   * transferred values.
   *
   * @tparam K
   * @tparam T
   * @param other compatible set to transfer values from
   */
  template <detail::set_addable_key<set> K, class T>
  constexpr void splice(set<K, T>& other) {
    try_reserve_for(other);
    // private method exposes mutable key values
    other.erase_if([this](K& key) { return try_insert_impl(cend(), key, std::true_type{}).second; }, true);
  }

  /**
   * @brief Combine two sets by moving values from other into this. other is left in an unspecified state without the
   * transferred values, valid state can be reacquired by calling other.rehash(0).
   *
   * @copydetail set::splice(set<K,T>&)
   */
  template <detail::set_addable_key<set> K, class T>
  constexpr void splice(set<K, T>&& other) {
    try_reserve_for(other);
    // consuming, so no need to update the hash_table
    other.erase_if([this](K& key) { return try_insert_impl(cend(), key, std::true_type{}).second; }, false);
  }

  // Lookup
  /**
   * @brief Check if key is held by this set
   *
   * @param key key value
   * @return true if key is held
   * @return false otherwise
   */
  [[nodiscard]] constexpr auto contains(key_type const& key) const noexcept -> bool { return find(key) != end(); }

  /**
   * @copybrief set::contains(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail set::contains(key_type const&)
   */
  template <detail::set_lookup_key<set> K>
  [[nodiscard]] constexpr auto contains(K const& key) const noexcept -> bool {
    return find(key) != end();
  }

  /**
   * @brief Number of values matching key
   *
   * @param key key value
   * @return 1 if found
   * @return 0 otherwise
   */
  [[nodiscard]] constexpr auto count(key_type const& key) const noexcept -> size_type { return contains(key) ? 1 : 0; }

  /**
   * @copybrief set::count(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail set::count(key_type const&)
   */
  template <detail::set_lookup_key<set> K>
  [[nodiscard]] constexpr auto count(K const& key) const noexcept -> size_type {
    return contains(key) ? 1 : 0;
  }

  /**
   * @brief Find the iterator to the key
   *
   * @param key key value
   * @return const_iterator iterator to the element with key value or end otherwise
   */
  [[nodiscard]] constexpr auto find(key_type const& key) const noexcept -> const_iterator {
    auto bucket = find_bucket(key);
    if (bucket != hash_table_.end()) { return begin() + static_cast<difference_type>(*bucket); }
    return end();
  }

  /**
   * @copybrief set::find(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail set::find(key_type const&)
   */
  template <detail::set_lookup_key<set> K>
  [[nodiscard]] constexpr auto find(K const& key) const noexcept -> const_iterator {
    auto bucket = find_bucket(key);
    if (bucket != hash_table_.end()) { return begin() + static_cast<difference_type>(*bucket); }
    return end();
  }

  /**
   * @brief Find the range of buckets holding key. Values are unique so the range is always of size 1 or 0 depending if
   * the key is held.
   *
   * @tparam K
   * @param key key value
   * @return std::pair<const_iterator, const_iterator> [first, last) range containing key if found, {end, end} otherwise
   */
  [[nodiscard]] constexpr auto equal_range(key_type const& key) const noexcept
      -> std::pair<const_iterator, const_iterator> {
    auto iter = find(key);
    if (iter == end()) { return {end(), end()}; }
    return {iter, iter + 1};
  }

  /**
   * @copybrief set::equal_range(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail set::equal_range(key_type const&)
   */
  template <detail::set_lookup_key<set> K>
  [[nodiscard]] constexpr auto equal_range(K const& key) const noexcept -> std::pair<const_iterator, const_iterator> {
    auto iter = find(key);
    if (iter == end()) { return {end(), end()}; }
    return {iter, iter + 1};
  }

  // Bucket interface
  /**
   * @brief Number of buckets used
   *
   * @return size_type
   */
  [[nodiscard]] constexpr auto bucket_count() const noexcept -> size_type { return hash_table_.bucket_count(); }

  /**
   * @brief Maximum number of buckets dependant on the hash type, system limits are ignored
   *
   * @return size_type
   */
  [[nodiscard]] constexpr auto max_bucket_count() const noexcept -> size_type {
    return static_cast<size_type>(std::numeric_limits<hash_type>::max());
  }

  /**
   * @brief Number of elements the bucket points to
   *
   * @param bucket index to the bucket
   * @return size_type 1 if bucket points to a key value, 0 otherwise
   */
  [[nodiscard]] constexpr auto bucket_size(size_type bucket) const noexcept -> size_type {
    FLAT_HASH_ASSERT(bucket < bucket_count());
    return (hash_table_.begin() + static_cast<hash_table::difference_type>(bucket)).holds_value() ? 1 : 0;
  }

  /**
   * @brief Find bucket index holding key
   *
   * @param key key value
   * @return size_type index to the hash table bucket
   */
  [[nodiscard]] constexpr auto bucket(key_type const& key) const noexcept -> size_type {
    return static_cast<size_type>(find_bucket(key) - hash_table_.begin());
  }

  /**
   * @copybrief set::bucket(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail set::bucket(key_type const&)
   */
  template <detail::set_lookup_key<set> K>
  [[nodiscard]] constexpr auto bucket(K const& key) const noexcept -> size_type {
    return static_cast<size_type>(find_bucket(key) - hash_table_.begin());
  }

  // Hash policy
  /**
   * @brief Current load factor
   *
   * @return double
   */
  [[nodiscard]] constexpr auto load_factor() const noexcept -> double { return calc_load_factor(size()); }

  /**
   * @brief Maximum load factor
   *
   * @return double
   */
  [[nodiscard]] constexpr auto max_load_factor() const noexcept -> double {
    return static_cast<double>(hash_table_.probing_policy().max_load_factor());
  }

  /**
   * @brief Set the number of buckets and rehash the table. count = 0 will force rehash the table without changing the
   * number of buckets.
   *
   * @param count new number of buckets
   */
  constexpr void rehash(size_type count)
    requires detail::mutable_range<index_container>
  {
    if (count != 0) {
      FLAT_HASH_ASSERT(static_cast<size_type>(static_cast<double>(count) * max_load_factor()) > size(),
                       "Too few buckets to hold {:d} keys", size());
      hash_table_.resize_at_least(count);
    } else {
      hash_table_.clear();
    }
    index_type j = 0;
    for (value_type const& key : keys_) {
      // unless the invariant has been broken all keys will be different, therefore predicate will always return false
      // hash_table handles always_false in a more optimal way
      insert_key_into_table(key, j++);
    }
  }

  /**
   * @brief Reserve space for keys and buckets, may require a rehash
   *
   * @param n number of keys to reserve for
   */
  constexpr void reserve(size_type n)
    requires detail::reservable_set<set>
  {
    detail::containers::reserve(keys_, n);
    if (n <= size()) return;

    bool changed = hash_table_.resize_at_least(detail::at_least<index_type>(capacity(), max_load_factor()));
    if constexpr (detail::mutable_range<index_container>) {
      if (changed) { rehash(0); }
    } else {
      FLAT_HASH_ASSERT(!changed, "Cannot resize immutable hash table");
    }
  }

  // Observers
  /**
   * @brief Hasher accessor
   *
   * @return hasher
   */
  [[nodiscard]] constexpr auto hash_function() const noexcept(std::is_nothrow_copy_constructible_v<hasher>) -> hasher {
    return hash_.get();
  }

  /**
   * @brief Key equality comparison accessor
   *
   * @return key_equal
   */
  [[nodiscard]] constexpr auto key_eq() const noexcept(std::is_nothrow_copy_constructible_v<key_equal>) -> key_equal {
    return key_eq_.get();
  }

  /**
   * @brief Traits accessor
   *
   * @return Traits&
   */
  constexpr auto traits() noexcept -> Traits& { return traits_.get(); }

  /**
   * @brief Traits accessor
   *
   * @return Traits const&
   */
  [[nodiscard]] constexpr auto traits() const noexcept -> Traits const& { return traits_.get(); }

  /**
   * @brief Probing policy accessor
   *
   * @return probing_policy&
   */
  constexpr auto probing() noexcept -> probing_policy& { return hash_table_.probing_policy(); }

  /**
   * @brief Probing policy accessor
   *
   * @return probing_policy const&
   */
  [[nodiscard]] constexpr auto probing() const noexcept -> probing_policy const& {
    return hash_table_.probing_policy();
  }

  /**
   * @brief Accessor to data for contiguous containers
   *
   */
  [[nodiscard]] constexpr auto data() const noexcept -> key_type const*
    requires std::ranges::contiguous_range<key_container>
  {
    return std::ranges::data(keys_);
  }

  /**
   * @brief Implicit conversion to std::span for contiguous containers
   *
   * @return std::span<Key const>
   */
  [[nodiscard]] constexpr operator std::span<Key const>() const noexcept
    requires std::ranges::contiguous_range<key_container>
  {
    return std::span<Key const>(data(), size());
  }

  /**
   * @brief Implicit conversion to fixed exten std::span for contiguous and statically sized containers
   *
   * @return std::span<Key const, detail::static_size_v<key_container>>
   */
  [[nodiscard]] constexpr operator std::span<Key const, detail::static_size_v<key_container>>() const noexcept
    requires(std::ranges::contiguous_range<key_container> && detail::static_sized<key_container>)
  {
    return std::span<Key const, detail::static_size_v<key_container>>(data(), size());
  }

  // Non-member functions
  /**
   * @brief Set equality comparison
   *
   * @tparam K
   * @tparam Opts
   * @param lhs
   * @param rhs
   * @return true if lhs and rhs contain all same values
   * @return false otherwise
   */
  template <class K, class Opts>
    requires(detail::set_lookup_key<K, set> || detail::set_lookup_key<Key, set<K, Opts>>)
  [[nodiscard]] constexpr friend auto operator==(set const& lhs, set<K, Opts> const& rhs) noexcept -> bool {
    return lhs.size() == rhs.size() && lhs.is_subset_of(rhs);
  }

  /**
   * @brief Set comparison
   *
   * @tparam K
   * @tparam Opts
   * @param lhs
   * @param rhs
   * @return true if lhs is a proper subset of rhs
   * @return false otherwise
   */
  template <class K, class Opts>
    requires(detail::set_lookup_key<K, set> || detail::set_lookup_key<Key, set<K, Opts>>)
  [[nodiscard]] constexpr friend auto operator<(set const& lhs, set<K, Opts> const& rhs) noexcept -> bool {
    return lhs.size() < rhs.size() && lhs.is_subset_of(rhs);
  }

  /**
   * @brief Set comparison
   *
   * @tparam K
   * @tparam Opts
   * @param lhs
   * @param rhs
   * @return true if lhs is a subset of rhs
   * @return false otherwise
   */
  template <class K, class Opts>
    requires(detail::set_lookup_key<K, set> || detail::set_lookup_key<Key, set<K, Opts>>)
  [[nodiscard]] constexpr friend auto operator<=(set const& lhs, set<K, Opts> const& rhs) noexcept -> bool {
    return lhs.size() <= rhs.size() && lhs.is_subset_of(rhs);
  }

  /**
   * @brief Set comparison
   *
   * @tparam K
   * @tparam Opts
   * @param lhs
   * @param rhs
   * @return true if rhs is a proper subset of lhs
   * @return false otherwise
   */
  template <class K, class Opts>
    requires(detail::set_lookup_key<K, set> || detail::set_lookup_key<Key, set<K, Opts>>)
  [[nodiscard]] constexpr friend auto operator>(set const& lhs, set<K, Opts> const& rhs) noexcept -> bool {
    return lhs.size() > rhs.size() && rhs.is_subset_of(lhs);
  }

  /**
   * @brief Set comparison
   *
   * @tparam K
   * @tparam Opts
   * @param lhs
   * @param rhs
   * @return true if rhs is a subset of lhs
   * @return false otherwise
   */
  template <class K, class Opts>
    requires(detail::set_lookup_key<K, set> || detail::set_lookup_key<Key, set<K, Opts>>)
  [[nodiscard]] constexpr friend auto operator>=(set const& lhs, set<K, Opts> const& rhs) noexcept -> bool {
    return lhs.size() >= rhs.size() && rhs.is_subset_of(lhs);
  }

  /**
   * @brief Swap two sets.
   *
   * @param lhs
   * @param rhs
   */
  constexpr friend void swap(set& lhs, set& rhs) noexcept(noexcept(lhs.swap(rhs))) { lhs.swap(rhs); }

  /**
   * @brief Erase values matching a predicate
   *
   * @param self set
   * @param predicate predicate
   * @return size_type number of elements erased
   */
  constexpr friend auto erase_if(set& self, std::predicate<value_type const&> auto&& predicate) -> size_type
      requires(detail::containers::erasable<key_container>&& detail::mutable_range<index_container>) {
        return self.erase_if(predicate);
      }

  /**
   * @brief Accessor for the hash table
   *
   * @return hash_table const&
   */
  [[nodiscard]] constexpr auto table() const noexcept -> hash_table const& {
    return hash_table_;
  }

 protected:
  template <detail::set_lookup_key<set> K>
  [[nodiscard]] constexpr auto equal_predicate(K const& key) const noexcept -> std::predicate<index_type> auto{
    return [this, &key](index_type index) noexcept(
               noexcept(key_eq_.get()(key, detail::containers::at(keys_, index)))) -> bool {
      return key_eq_.get()(key, detail::containers::at(keys_, index));
    };
  }

  template <detail::set_lookup_key<set> K, std::predicate<index_type> Pred>
  [[nodiscard]] constexpr auto find_bucket(K const& key, Pred predicate) const noexcept -> hash_table::const_iterator {
    return hash_table_.find(hash_.get()(key), predicate);
  }

  template <detail::set_lookup_key<set> K>
  [[nodiscard]] constexpr auto find_bucket(K const& key) const noexcept -> hash_table::const_iterator {
    return find_bucket(key, equal_predicate(key));
  }

  [[nodiscard]] constexpr auto find_bucket(const_iterator pos) const noexcept -> hash_table::const_iterator {
    auto offset = static_cast<size_type>(pos - cbegin());
    // position is known so only need to look for the bucket with matching index, avoids fetching the stored keys
    return find_bucket(*pos, [offset](index_type index) noexcept { return index == offset; });
  }

 private:
  // apparently having empty members at the end will overwrite other members...
  FLAT_HASH_NO_UNIQUE_ADDRESS detail::maybe_empty<Traits> traits_{};
  FLAT_HASH_NO_UNIQUE_ADDRESS detail::maybe_empty<hasher> hash_{};
  FLAT_HASH_NO_UNIQUE_ADDRESS detail::maybe_empty<key_equal> key_eq_{};

  hash_table hash_table_;
  key_container keys_;

  constexpr void insert_key_into_table(value_type const& key, index_type index) noexcept {
    auto [it, value] = hash_table_.find_insertion_bucket(hash_.get()(key), detail::always_false, index);
    hash_table_.insert(it, *value);
  }

  constexpr auto ensure_load_factor(size_type n, bool force_rehash = false, bool strict = true) -> bool {
    if constexpr (detail::containers::resizable<index_container>) {
      bool resized = false;
      if (calc_load_factor(n) > max_load_factor()) [[unlikely]] {
        resized = hash_table_.resize_at_least(detail::at_least<size_type>(n, max_load_factor()));
      }
      if (resized || force_rehash) {
        rehash(0);
        return true;
      }
    } else {
      if (strict) {
        FLAT_HASH_ASSERT(calc_load_factor(n) < max_load_factor(),
                         "Cannot ensure load factor with non-resizable hash table");
      }
    }
    return false;
  }

  [[nodiscard]] constexpr auto calc_load_factor(size_type n) const noexcept -> double {
    return static_cast<double>(n) / static_cast<double>(bucket_count());
  }

  /**
   * @brief Directly assign values to key_container if the container is not back_emplaceable, mostly useful for fixed
   * size containers. Asserts on too few or too many unique values assigned.
   *
   * @param values
   */
  template <std::ranges::input_range R>
    requires((std::ranges::borrowed_range<R> && std::is_assignable_v<key_type&, std::ranges::range_reference_t<R>>) ||
             (!std::ranges::borrowed_range<R> && std::is_assignable_v<key_type&, std::ranges::range_value_t<R>>))
  constexpr void assign(R&& values) {
    if constexpr (detail::containers::resizable<key_container>) { detail::containers::resize(keys_, values.size()); }
    [[maybe_unused]] size_type n = std::ranges::size(keys_);

    index_type added = 0;
    auto out = std::ranges::begin(keys_);
    for (auto&& v : values) {
      auto [probe, bucket_value] = hash_table_.find_insertion_bucket(hash_.get()(v), equal_predicate(v), added);
      if (!bucket_value) [[unlikely]] { continue; }
      if constexpr (!detail::containers::resizable<key_container>) {
        // check that we are still within the container size
        if (added == n) [[unlikely]] {  // LCOV_EXCL_LINE
          FLAT_HASH_ASSERT(false, "Tried to add more unique values than the container could hold ({:d}): {}", n,
                           detail::maybe_format_arg(values));
          break;
        }
      }

      if constexpr (std::ranges::borrowed_range<R>) {
        *out = v;
      } else {
        *out = std::move(v);
      }
      // update table after assignment in case it throws
      hash_table_.insert(probe, *bucket_value);
      ++out;
      ++added;
    }

    if constexpr (detail::containers::resizable<key_container>) {
      detail::containers::resize(keys_, n + added);
    } else {
      FLAT_HASH_ASSERT(added == n, "Not enough unique values ({:d}), expected {:d}: {}", added, n,
                       detail::maybe_format_arg(values));
    }
  }

  template <detail::set_addable_key<set> K, bool Move>
  constexpr auto try_insert_impl(const_iterator pos, K& key, std::bool_constant<Move>) -> std::pair<iterator, bool> {
    // forwarding is controlled by Move so that this method never consumes the key argument if it is already present in
    // the set

    auto offset = pos - cbegin();
    FLAT_HASH_ASSERT(offset <= std::ranges::ssize(keys_), "Iterator past the end");
    ensure_load_factor(size() + 1);

    auto [table_iterator, bucket_value] =
        hash_table_.find_insertion_bucket(hash_.get()(key), equal_predicate(key), static_cast<index_type>(offset));

    auto forwarded_key = [&key]() noexcept -> decltype(auto) {
      if constexpr (Move && !std::is_const_v<K>) {
        return std::move(key);
      } else {
        return key;
      }
    };

    iterator out{};
    if (bucket_value) {
      if (pos == cend()) {
        detail::containers::emplace_back(keys_, forwarded_key());
        hash_table_.insert(table_iterator, *bucket_value);
        out = std::ranges::begin(keys_) + ssize() - 1;
      } else {
        if constexpr (ordering == ordering_policy::preserved && detail::containers::insertible_to<K, key_container>) {
          out = detail::containers::policy_insert<ordering>(keys_, pos.base(), forwarded_key());

          // shift the indices and then insert the value
          hash_table_.mutate_if([i = static_cast<size_type>(offset)](index_type index) noexcept { return index >= i; },
                                [](index_type index) noexcept { return index + 1; });
          hash_table_.insert(table_iterator, *bucket_value);
        } else if constexpr (ordering == ordering_policy::relaxed) {
          auto swap_iter = find_bucket(pos);
          // may throw so do it first before updating hash table
          out = detail::containers::policy_insert<ordering>(keys_, pos.base(), forwarded_key());

          // first modify the swapped bucket because insert may invalidate it, i.e. robin_hood probing
          hash_table_.overwrite(swap_iter, static_cast<index_type>(ssize() - 1));
          hash_table_.insert(table_iterator, *bucket_value);
        } else {
          detail::containers::emplace_back(keys_, forwarded_key());
          hash_table_.insert(table_iterator, *bucket_value);
          out = std::ranges::begin(keys_) + ssize() - 1;
        }
      }
    }

    return {out, bucket_value.has_value()};
  }

  template <detail::set_addable_key<set> K, bool Move>
  constexpr auto insert_impl(const_iterator pos, K& key, std::bool_constant<Move> move) -> iterator {
    auto [iter, added] = try_insert_impl(pos, key, move);

    if (!added) { traits_.on_duplicate_key(*iter); }
    return iter;
  }

  constexpr auto extract_impl(hash_table::const_iterator bucket_iterator) -> std::optional<value_type> {
    if (bucket_iterator == hash_table_.end()) {
      // not found
      return std::nullopt;
    }

    auto index = *bucket_iterator;
    auto first = std::ranges::begin(keys_);
    auto iter = first + static_cast<difference_type>(index);
    std::optional<value_type> value = std::nullopt;
    if constexpr (ordering == ordering_policy::preserved) {
      value = detail::containers::extract<ordering>(keys_, iter);
      hash_table_.clear_bucket(bucket_iterator);

      if (index < size()) {
        hash_table_.mutate_if([index](index_type bucket_value) noexcept { return bucket_value > index; },
                              [](index_type bucket_value) noexcept { return --bucket_value; });
      }
    } else if constexpr (ordering == ordering_policy::relaxed) {
      auto last = first + std::ranges::ssize(keys_) - 1;
      auto last_table_iter = hash_table_.find(hash_.get()(*last), equal_predicate(*last));
      value = detail::containers::extract<ordering>(keys_, iter);
      hash_table_.swap_buckets(bucket_iterator, last_table_iter);
      hash_table_.clear_bucket(bucket_iterator);
    }

    return value;
  }

  template <class K, class Opts>
    requires(detail::set_lookup_key<K, set> || detail::set_lookup_key<Key, set<K, Opts>>)
  [[nodiscard]] constexpr auto is_subset_of(set<K, Opts> const& rhs) const noexcept -> bool {
    if constexpr (detail::set_lookup_key<Key, set<K, Opts>>) {
      return std::ranges::all_of(keys_, [&rhs](key_type const& key) -> bool { return rhs.contains(key); });
    } else {
      // slow path
      size_type expected = size();
      size_type missing = rhs.size() - expected;
      size_type found = 0;
      size_type count = 0;
      for (auto const& key : rhs.keys_) {
        found += static_cast<size_type>(contains(key));
        ++count;

        if (found == expected) [[unlikely]] { return true; }
        if (count - found >= missing) [[unlikely]] { return false; }
      }

      return false;
    }
  }

  template <std::ranges::input_range R>
    requires(detail::set_addable_key<std::ranges::range_value_t<R>, set>)
  constexpr void merge_unique(R&& source) {
    auto const old_size = size();

    auto&& values = [&source]() -> decltype(auto) {
      if constexpr (is_set<R>) {
        // set_iterator always returns const references, so the values wouldn't be consumed even if it was passed as an
        // rvalue, use the original container directly instead
        return source.keys_;
      } else {
        return source;
      }
    }();

#ifdef __cpp_exceptions
    auto const old_ssize = static_cast<difference_type>(old_size);

    try {
#endif
      for (auto&& value : values) {
        // guaranteed to contain unique values, only add values that are not already in this set
        if (!contains(value)) {
          if constexpr (std::ranges::borrowed_range<R>) {
            detail::containers::emplace_back(keys_, value);
          } else {
            detail::containers::emplace_back(keys_, std::move(value));
          }
        }
      }

      ensure_load_factor(size());
#ifdef __cpp_exceptions
    } catch (...) {
      // erasing from the end is much less likely to throw if it does
      detail::containers::erase_after(keys_, std::ranges::cbegin(keys_) + old_ssize);
      throw;
    }
#endif

    // no need to rehash previous keys as values were simply placed into key container
    auto const s = static_cast<index_type>(size());
    for (auto i = static_cast<index_type>(old_size); i < s; ++i) {
      insert_key_into_table(detail::containers::at(keys_, i), i);
    }
  }

  template <std::ranges::input_range R>
    requires(detail::set_addable_key<std::ranges::range_value_t<R>, set>)
  constexpr void merge_any(R&& source) {
    for (auto&& value : source) {
      // not guaranteed to contain unique values, have to insert and update hash table every time
      if constexpr (std::ranges::borrowed_range<R>) {
        try_insert(value);
      } else {
        try_insert(std::move(value));
      }
    }
  }

  template <std::ranges::range R>
  constexpr void try_reserve_for(R const& range) {
    if constexpr (std::ranges::sized_range<R>) {
      size_type old_size = size();
      if constexpr (detail::reservable_set<set>) {
        reserve(old_size + std::ranges::size(range));
      } else {
        // don't need to be strict about hash table size if the range can have duplicates
        ensure_load_factor(old_size + std::ranges::size(range), /* rehash = */ false, /* strict = */ unique_range<R>);
      }
    }
  }

  constexpr auto erase_if(std::predicate<value_type&> auto&& predicate, bool update_hashes = true) -> size_type
    requires detail::containers::erasable<key_container> && detail::mutable_range<index_container>
  {
    size_type erased = detail::containers::policy_erase_if<ordering>(keys_, predicate);
    if (update_hashes && erased != 0) { rehash(0); }
    return erased;
  }
};

// deduction guides
template <set_traits T>
set(set_init<T>) -> set<std::ranges::range_value_t<typename T::key_container>, T>;

template <set_traits T>
set(std::uint64_t, set_init<T>) -> set<std::ranges::range_value_t<typename T::key_container>, T>;

template <class K, set_traits_for<K> T = dynamic_set_traits<K>>
set(std::initializer_list<K>, set_init<T> = {}) -> set<K, T>;

template <std::ranges::input_range R,
          set_traits_for<std::ranges::range_value_t<R>> T = dynamic_set_traits<std::ranges::range_value_t<R>>>
  requires(detail::mutable_range<typename T::index_container>)
set(R&&, set_init<T> = {}) -> set<std::ranges::range_value_t<R>, T>;

template <std::input_iterator It, set_traits_for<std::iter_value_t<It>> T = dynamic_set_traits<std::iter_value_t<It>>>
  requires(detail::mutable_range<typename T::index_container>)
set(It, It, set_init<T> = {}) -> set<std::iter_value_t<It>, T>;

FLAT_HASH_NAMESPACE_END

template <class Key, class Traits, class Char>
  requires(flat_hash::detail::formattable<Key, Char>)
struct FLAT_HASH_FORMAT_NS formatter<flat_hash::set<Key, Traits>, Char> {
  FLAT_HASH_FORMAT_NS formatter<Key, Char> subformatter{};
  flat_hash::detail::range_format_options options{};

  // fmt basic_format_parse_context also has ErrorHandler template argument
  template <class... Args>
  constexpr auto parse(FLAT_HASH_FORMAT_NS basic_format_parse_context<Args...>& ctx) {
    auto [it, opts] = flat_hash::detail::parse_range_format(ctx);
    options = opts;

    ctx.advance_to(it);
    return subformatter.parse(ctx);
  }

  template <class OutputIt>
  auto format(flat_hash::set<Key, Traits> const& set, FLAT_HASH_FORMAT_NS basic_format_context<OutputIt, Char>& context)
      -> OutputIt {
    auto strings = flat_hash::detail::get_set_separators(options);
    return flat_hash::detail::format_range(set, context, strings,
                                           [this](auto& value, auto& ctx) { return subformatter.format(value, ctx); });
  }
};

#if defined(FLAT_HASH_USE_FMTLIB)
#  include <fmt/ranges.h>

// set is also a range which makes formatter specialization ambiguous, disable range check since we provide a
// formatter already
template <class Key, class Traits, class Char>
  requires(flat_hash::detail::formattable<Key, Char>)
struct fmt::is_range<flat_hash::set<Key, Traits>, Char> : std::false_type {};
#endif
