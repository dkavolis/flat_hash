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

#include "containers/utility.hpp"
#include "hash_table.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {

template <template <class> class Trait, class... T>
concept conjunction = (... && Trait<T>::value);

template <class Base, template <class> class Trait>
concept hash_container_base_applied =
    conjunction<Trait, typename Base::hash_table_type, typename Base::hasher, typename Base::key_equal>;

template <class Base>
concept swappable_hash_container_base = swappable<typename Base::hash_table_type> && swappable<typename Base::hasher> &&
                                        swappable<typename Base::key_equal>;

template <class Base>
concept nothrow_swappable_hash_container_base =
    nothrow_swappable<typename Base::hash_table_type> && nothrow_swappable<typename Base::hasher> &&
    nothrow_swappable<typename Base::key_equal>;

/**
 * @brief Helper function for computing the lowest number of buckets for specified load_factor
 *
 * @tparam U
 * @tparam UI
 * @param n number of elements
 * @param load_factor desired maximum load factor
 * @return U lowest number of buckets for holding n elements with at most load_factor
 */
template <std::integral U, std::unsigned_integral UI>
[[nodiscard]] constexpr auto at_least(UI n, double load_factor) noexcept -> U {
  return static_cast<U>(std::ceil(static_cast<double>(n) / load_factor));
}

// TODO: replace templated functors with function_ref for fewer template instantiations?
// TODO: benchmark runtime ordering_policy
// both will probably have not much difference because there is a lot going on already with lookup and insertions but
// will improve compile times

template <index_range Container, probing::probing_policy<Container> Policy, std::semiregular Hash,
          std::semiregular KeyEq>
class hash_container_base : public containers::maybe_enable_allocator_type<Container> {
  template <index_range C, probing::probing_policy<C>, std::semiregular, std::semiregular>
  friend class hash_container_base;

 public:
  using index_container = Container;
  using probing_policy = Policy;
  using index_type = std::ranges::range_value_t<Container>;
  using hash_table_type = hash_table<Container, Policy>;
  using probing_iterator = hash_table_type::probing_iterator;
  using probing_info = hash_table_type::probing_info;

  template <class Key>
  using hash_type = decltype(std::declval<Hash const&>()(std::declval<Key const&>()));

  using value_type = index_type;
  using size_type = std::ranges::range_size_t<Container>;
  using difference_type = std::ranges::range_difference_t<Container>;
  using hasher = Hash;
  using key_equal = KeyEq;
  using iterator = hash_table_type::iterator;
  using const_iterator = hash_table_type::const_iterator;

  constexpr hash_container_base()
    requires std::is_default_constructible_v<hash_table_type>
  = default;
  constexpr hash_container_base(size_type bucket_count, probing_policy const& policy,
                                optional_allocator<index_container> const& allocator, hasher const& hash,
                                key_equal const& key_eq)
      : table_(bucket_count, policy, allocator), hash_(hash), key_eq_(key_eq) {}
  constexpr hash_container_base(index_container indices, probing_policy const& policy, hasher const& hash,
                                key_equal const& key_eq)
      : table_(std::move(indices), policy), hash_(hash), key_eq_(key_eq) {}

  template <class C>
    requires std::constructible_from<Container, C&>
  constexpr explicit(!std::convertible_to<C&, Container>)
      hash_container_base(hash_container_base<C, Policy, Hash, KeyEq>& other)
      : table_(other.table_), hash_(other.hash_), key_eq_(other.key_eq_) {}

  template <class C>
    requires std::constructible_from<Container, C const&>
  constexpr explicit(!std::convertible_to<C const&, Container>)
      hash_container_base(hash_container_base<C, Policy, Hash, KeyEq> const& other)
      : table_(other.table_), hash_(other.hash_), key_eq_(other.key_eq_) {}

  constexpr hash_container_base(hash_container_base const&) noexcept(
      hash_container_base_applied<hash_container_base, std::is_nothrow_copy_constructible>) = default;
  constexpr hash_container_base(hash_container_base&&) noexcept(
      hash_container_base_applied<hash_container_base, std::is_nothrow_move_constructible>) = default;

  constexpr auto operator=(hash_container_base const&) noexcept(
      hash_container_base_applied<hash_container_base, std::is_nothrow_copy_assignable>)
      -> hash_container_base& = default;
  constexpr auto operator=(hash_container_base&&) noexcept(
      hash_container_base_applied<hash_container_base, std::is_nothrow_move_assignable>)
      -> hash_container_base& = default;

  constexpr ~hash_container_base() noexcept(
      hash_container_base_applied<hash_container_base, std::is_nothrow_destructible>) = default;

  [[nodiscard]] constexpr auto get_allocator() const noexcept(containers::nothrow_gettable_allocator<index_container>)
      -> containers::allocator_t<hash_table_type>
    requires containers::gettable_allocator<index_container>
  {
    return table_.get_allocator();
  }

  [[nodiscard]] constexpr auto begin() const noexcept -> const_iterator { return table_.cbegin(); }
  [[nodiscard]] constexpr auto end() const noexcept -> const_iterator { return table_.end(); }

  [[nodiscard]] constexpr auto bucket_count() const noexcept -> size_type { return table_.bucket_count(); }

  template <class Key>
    requires hash_for<hasher, Key>
  [[nodiscard]] constexpr auto max_bucket_count() const noexcept -> size_type {
    return static_cast<size_type>(std::numeric_limits<hash_type<Key>>::max());
  }

  template <class Key>
    requires hash_for<hasher, Key>
  [[nodiscard]] constexpr auto max_keys() const noexcept -> size_type {
    auto [reserved_values, reserved_bits] = table_.reserved();
    constexpr auto indexing_bytes = static_cast<std::uint8_t>(std::min(sizeof(hash_type<Key>), sizeof(index_type)));

    return (size_type{1} << (indexing_bytes * CHAR_BIT - reserved_bits)) - reserved_values;
  }
  [[nodiscard]] constexpr auto max_load_factor() const noexcept -> float {
    return static_cast<float>(table_.probing_policy().max_load_factor());
  }
  [[nodiscard]] constexpr auto supported_keys_capacity() const noexcept -> size_type {
    return static_cast<size_type>(max_load_factor() * static_cast<float>(bucket_count()));
  }

  constexpr void clear() noexcept
    requires mutable_range<index_container>
  {
    table_.clear();
  }

  constexpr void swap(hash_container_base& other) noexcept(nothrow_swappable_hash_container_base<hash_container_base>)
    requires swappable_hash_container_base<hash_container_base>
  {
    if (this == &other) [[unlikely]] { return; }
    std::ranges::swap(table_, other.table_);
    std::ranges::swap(hash_, other.hash_);
    std::ranges::swap(key_eq_, other.key_eq_);
  }

  template <class K>
    requires hash_for<hasher, K>
  [[nodiscard]] constexpr auto hash(K const& value) const noexcept(noexcept(this->hash_.get()(value))) {
    return hash_.get()(value);
  }

  template <class K>
    requires hash_for<hasher, K>
  [[nodiscard]] constexpr auto find_at(K const& key, index_type index) const -> const_iterator {
    return table_.find(hash(key), [index](index_type i) noexcept { return index == i; });
  }

  template <std::ranges::random_access_range Keys, class K = std::ranges::range_value_t<Keys>>
    requires(valid_key<K, std::ranges::range_value_t<Keys>, hasher, key_equal>)
  [[nodiscard]] constexpr auto find_in(K const& key, Keys const& keys) const -> const_iterator {
    return table_.find(hash(key), [&key, &keys, this](index_type index) -> bool {
      return key_eq_.get()(key, containers::at(keys, index));
    });
  }

  [[nodiscard]] constexpr auto load_factor(size_type n) const noexcept -> float {
    return static_cast<float>(n) / static_cast<float>(bucket_count());
  }

  template <std::ranges::random_access_range Keys>
    requires hash_for<hasher, std::ranges::range_value_t<Keys>> && mutable_range<index_container>
  constexpr void rehash(size_type n, Keys const& keys) {
    if (n != 0) {
      FLAT_HASH_ASSERT(static_cast<size_type>(static_cast<float>(n) * max_load_factor()) > std::ranges::size(keys),
                       "Too few buckets to hold {:d} keys", std::ranges::size(keys));
      if (!table_.resize_at_least(n)) { table_.clear(); }
    } else {
      table_.clear();
    }
    fill_hashes(keys);
  }

  constexpr void insert_new_key(std::uint64_t hash, index_type index) {
    auto [it, probe_state] = table_.find_insertion_bucket(hash, always_false);
    table_.insert(it, *probe_state, index);
  }

  template <std::ranges::input_range Keys>
    requires hash_for<hasher, std::ranges::range_value_t<Keys>>
  constexpr void reserve_for(Keys const& keys) {
    bool resized = table_.resize_at_least(at_least<index_type>(containers::capacity(keys), max_load_factor()));
    if constexpr (mutable_range<index_container>) {
      if (resized) { fill_hashes(keys); }
    } else {
      FLAT_HASH_ASSERT(!resized, "Cannot resize immutable hash table");
    }
  }

  template <std::ranges::random_access_range Keys>
    requires hash_for<hasher, std::ranges::range_value_t<Keys>>
  constexpr auto ensure_load_factor(Keys const& keys, size_type extra, bool strict = true) -> bool {
    size_type n = std::ranges::size(keys) + extra;
    if constexpr (containers::resizable<index_container>) {
      bool resized = false;
      if (load_factor(n) > max_load_factor()) [[unlikely]] {
        resized = table_.resize_at_least(at_least<size_type>(n, max_load_factor()));
      }
      if (resized) {
        fill_hashes(keys);
        return true;
      }
    } else {
      if (strict) {
        FLAT_HASH_ASSERT(load_factor(n) < max_load_factor(),
                         "Cannot ensure load factor with non-resizable hash table: {:.2f} < {:.2f}", load_factor(n),
                         max_load_factor());
      }
    }
    return false;
  }

  template <ordering_policy Ordering, class Key, std::ranges::random_access_range Keys,
            std::invocable<index_type> OnInsert>
    requires(valid_key<Key, std::ranges::range_value_t<Keys>, hasher, key_equal>)
  constexpr auto try_insert_at(Key const& key, Keys& keys, std::ranges::iterator_t<Keys const&> pos, OnInsert on_insert)
      -> std::pair<index_type, bool> {
    auto [table_pos, info] = table_.find_insertion_bucket(
        hash(key), [&key, &keys, this](index_type index) { return key_eq_.get()(key, containers::at(keys, index)); });
    auto offset = static_cast<index_type>(pos - std::ranges::cbegin(keys));
    auto size = static_cast<index_type>(std::ranges::size(keys));
    const_iterator swap_iter;
    if (info) {
      ensure_load_factor(keys, 1);
      if constexpr (Ordering == ordering_policy::relaxed) {
        if (pos != std::ranges::cend(keys)) { swap_iter = find_at(containers::at(keys, offset), offset); }
      }
    }

    return try_insert<Ordering>(table_pos, swap_iter, info, offset, size, on_insert);
  }

  template <ordering_policy Ordering, std::ranges::random_access_range Keys, std::invocable<index_type> OnErase,
            class Key = std::ranges::range_value_t<Keys>>
    requires(valid_key<Key, std::ranges::range_value_t<Keys>, hasher, key_equal>)
  constexpr auto try_erase(Keys& keys, Key const& key, OnErase on_erase) -> bool {
    auto iter = find_in(key, keys);
    if (iter == end()) { return false; }
    auto keys_count = static_cast<index_type>(std::ranges::size(keys));
    const_iterator last;
    if constexpr (Ordering == ordering_policy::relaxed) {
      if (iter.decoded() + 1 != keys_count) { last = find_at(containers::back(keys), keys_count - 1); }
    }
    return erase_at<Ordering>(iter, last, keys_count, on_erase);
  }

  template <ordering_policy Ordering, std::ranges::random_access_range Keys, std::invocable<index_type> OnErase>
    requires hash_for<hasher, std::ranges::range_value_t<Keys>>
  constexpr auto try_erase(Keys& keys, std::ranges::iterator_t<Keys const&> pos, OnErase on_erase) -> bool {
    auto offset = pos - std::ranges::cbegin(keys);
    auto i = static_cast<index_type>(offset);
    auto iter = find_at(*pos, i);
    auto keys_count = static_cast<index_type>(std::ranges::size(keys));
    const_iterator last;
    if constexpr (Ordering == ordering_policy::relaxed) {
      if (pos != std::ranges::cend(keys)) { last = find_at(containers::back(keys), keys_count - 1); }
    }
    return erase_at<Ordering>(iter, last, keys_count, on_erase);
  }

  [[nodiscard]] constexpr auto hash_function() const noexcept(std::is_nothrow_copy_constructible_v<hasher>) -> hasher {
    return hash_.get();
  }
  [[nodiscard]] constexpr auto key_eq() const noexcept(std::is_nothrow_copy_constructible_v<key_equal>) -> key_equal {
    return key_eq_.get();
  }
  constexpr auto probing() noexcept -> probing_policy& { return table_.probing_policy(); }
  [[nodiscard]] constexpr auto probing() const noexcept -> probing_policy const& { return table_.probing_policy(); }

  constexpr friend void swap(hash_container_base& lhs, hash_container_base& rhs) noexcept(
      nothrow_swappable_hash_container_base<hash_container_base>)
    requires swappable_hash_container_base<hash_container_base>
  {
    lhs.swap(rhs);
  }

  [[nodiscard]] constexpr auto table() const noexcept -> hash_table_type const& { return table_; }

 private:
  hash_table<Container, Policy> table_{};
  FLAT_HASH_NO_UNIQUE_ADDRESS maybe_empty<Hash> hash_{};
  FLAT_HASH_NO_UNIQUE_ADDRESS maybe_empty<KeyEq> key_eq_{};

  template <ordering_policy Ordering, std::invocable<index_type> OnInsert>
  constexpr auto try_insert(const_iterator pos, const_iterator at_swap [[maybe_unused]],
                            std::optional<probing_info> info, index_type index, index_type keys_count,
                            OnInsert on_insert) -> std::pair<index_type, bool> {
    if (info) {
      // may throw so do it before modifying hash table
      if constexpr (std::convertible_to<std::invoke_result_t<OnInsert, index_type>, bool>) {
        if (!std::invoke(on_insert, index)) [[unlikely]] { return {keys_count, false}; }
      } else {
        std::invoke(on_insert, index);
      }

      if (index != keys_count) {
        // inserting not at the end, need to sync the bucket that will have their elements
        if constexpr (Ordering == ordering_policy::preserved) {
          // shift the indices and then insert the value
          table_.mutate_if([index](index_type i) noexcept { return i >= index; },
                           [](index_type i) noexcept { return i + 1; });
        } else if constexpr (Ordering == ordering_policy::relaxed) {
          // first modify the swapped bucket because insert may invalidate it, i.e. robin_hood probing
          table_.overwrite(at_swap, keys_count);
        }
      }
      table_.insert(pos, *info, index);
    } else {
      // not inserting, return the found index
      index = pos.decoded();
    }

    return {index, info.has_value()};
  }

  template <ordering_policy Ordering, std::invocable<index_type> OnErase>
  constexpr auto erase_at(const_iterator pos, const_iterator last [[maybe_unused]], index_type keys_count,
                          OnErase on_erase) -> bool {
    auto index = pos.decoded();

    std::invoke(on_erase, index);

    if (index != keys_count - 1) {
      if constexpr (Ordering == ordering_policy::preserved) {
        table_.mutate_if([index](index_type bucket_value) noexcept { return bucket_value > index; },
                         [](index_type bucket_value) noexcept { return --bucket_value; });
      } else if constexpr (Ordering == ordering_policy::relaxed) {
        table_.swap_buckets(pos, last);
      }
    }

    table_.clear_bucket(pos);
    return true;
  }

  template <std::ranges::input_range Keys>
  constexpr void fill_hashes(Keys const& keys) {
    index_type j = 0;
    for (auto const& key : keys) {
      // unless the invariant has been broken all keys will be different, therefore predicate will always return false
      // hash_table handles always_false in a more optimal way
      insert_new_key(hash(key), j++);
    }
  }
};
}  // namespace detail

FLAT_HASH_NAMESPACE_END
