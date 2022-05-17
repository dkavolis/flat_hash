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

#include "detail/hash_container_base.hpp"
#include "dictionary_fwd.hpp"
#include "dictionary_traits.hpp"
#include "set.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {
template <class Dict>
concept mutable_dictionary =
    mutable_range<typename Dict::key_container> && mutable_range<typename Dict::index_container> &&
    mutable_range<typename Dict::value_container>;

template <class K, class V, class Dict>
concept dict_insertible = hashed_lookup_key<K, Dict> && mutable_dictionary<Dict> &&
                          containers::back_emplaceable<typename Dict::key_container> &&
                          containers::back_emplaceable<typename Dict::value_container> &&
                          containers::constructible_from<typename Dict::key_type, K&&> &&
                          containers::constructible_from<typename Dict::mapped_type, V&&>;

template <class K, class V, class Dict>
concept dict_assignable =
    hashed_lookup_key<K, Dict> && mutable_dictionary<Dict> && std::is_assignable_v<typename Dict::key_type&, V&&> &&
    std::is_assignable_v<typename Dict::mapped_type&, V&&>;

template <class K, class V, class Dict>
concept dict_insertible_or_assignable = dict_insertible<K, V, Dict> && dict_assignable<K, V, Dict>;

template <class P, class Dict>
concept dict_pair_insertible = pair_like<P> && dict_insertible<tuple_element_t<0, P>, tuple_element_t<1, P>, Dict>;

template <class P, class Dict>
concept dict_pair_assignable = pair_like<P> && dict_assignable<tuple_element_t<0, P>, tuple_element_t<1, P>, Dict>;

template <class P, class Dict>
concept dict_pair_insertible_or_assignable =
    pair_like<P> && dict_insertible_or_assignable<tuple_element_t<0, P>, tuple_element_t<1, P>, Dict>;

template <class K, class Dict>
concept dict_erasable_key =
    hashed_lookup_key<K, Dict> && mutable_dictionary<Dict> && containers::erasable<typename Dict::key_container> &&
    containers::erasable<typename Dict::value_container>;

template <class Dict>
concept clearable_dict = mutable_dictionary<Dict> && containers::clearable<typename Dict::key_container> &&
                         containers::clearable<typename Dict::value_container>;

template <class Dict>
concept reservable_dict =
    containers::reservable<typename Dict::key_container> && containers::reservable<typename Dict::value_container>;

template <class Dict>
concept default_constructible_dict =
    !(static_sized<typename Dict::key_container> || static_sized<typename Dict::value_container>);

template <class Dict, template <class> class Trait>
concept dict_apply = conjunction<Trait, typename Dict::base, typename Dict::key_container,
                                 typename Dict::value_container, typename Dict::traits_type>;

template <class Dict>
concept swappable_dict = swappable<typename Dict::traits_type> && swappable<typename Dict::base> &&
                         swappable<typename Dict::key_container> && swappable<typename Dict::value_container>;

template <class Dict>
concept nothrow_swappable_dict =
    nothrow_swappable<typename Dict::traits_type> && nothrow_swappable<typename Dict::base> &&
    nothrow_swappable<typename Dict::key_container> && nothrow_swappable<typename Dict::value_container>;

template <class Key, class Dict>
concept dict_subscriptable =
    hashed_lookup_key<Key, Dict> && std::constructible_from<typename Dict::key_type, Key&&> &&
    requires {
      typename Dict::traits_type;
      requires dictionary_traits<typename Dict::traits_type>;
      requires requires(typename Dict::traits_type & traits, typename Dict::key_type const& key) {
                 requires requires {
                            { traits.on_missing_key(key) } -> std::convertible_to<typename Dict::mapped_type>;
                          } ||
                              std::constructible_from<typename Dict::mapped_type, decltype(traits.on_missing_key(key))>;
               } || std::is_default_constructible_v<typename Dict::mapped_type>;
    };

template <class T1, class T2>
[[nodiscard]] constexpr auto forward_as_pair(T1&& first, T2&& second) noexcept -> std::pair<T1&&, T2&&> {
  return {std::forward<T1>(first), std::forward<T2>(second)};
}

template <class Out, class T>
concept const_writable = requires(Out&& o, T&& t) { const_cast<const Out&&>(o) = std::forward<T>(t); };

// std::pair is not a valid proxy for indirectly writable iterators
template <class T1, class T2>
struct dict_reference : public std::pair<T1, T2> {
  using base = std::pair<T1, T2>;

  // https://github.com/ericniebler/stl2/issues/642#issuecomment-839856562
  template <class U1, class U2>
    requires(const_writable<T1, U1> && const_writable<T2, U2>)
  constexpr auto operator=(std::pair<U1, U2>&& other) const -> dict_reference<T1, T2> const& {
    base::first = std::forward<U1>(other.first);
    base::second = std::forward<U2>(other.second);
    return *this;
  }

  using base::base;
  using base::operator=;
};

// not a general purpose zipped iterator, assumes that both iterators always point to the same relative element in equal
// size ranges, this allows comparisons based on only one of the iterators
template <std::random_access_iterator KI, std::random_access_iterator VI>
class dictionary_iterator_base {
  template <std::random_access_iterator, std::random_access_iterator>
  friend class dictionary_iterator_base;

 public:
  using key_type = std::iter_value_t<KI>;
  using key_reference = std::iter_reference_t<KI>;
  using mapped_type = std::iter_value_t<VI>;
  using mapped_reference = std::iter_reference_t<VI>;

  using value_type = std::pair<key_type, mapped_type>;
  using reference = dict_reference<key_reference, mapped_reference>;
  using difference_type = std::common_type_t<std::iter_difference_t<KI>, std::iter_difference_t<VI>>;

  constexpr dictionary_iterator_base() noexcept = default;
  constexpr dictionary_iterator_base(KI key_iterator, VI value_iterator) noexcept
      : key_iterator_(key_iterator), value_iterator_(value_iterator) {}
  template <class K, class V>
    requires std::constructible_from<KI, K> && std::constructible_from<VI, V>
  constexpr dictionary_iterator_base(dictionary_iterator_base<K, V> other) noexcept
      : key_iterator_(other.key_iterator_), value_iterator_(other.value_iterator_) {}

  constexpr dictionary_iterator_base(dictionary_iterator_base const&) noexcept = default;
  constexpr dictionary_iterator_base(dictionary_iterator_base&&) noexcept = default;
  constexpr auto operator=(dictionary_iterator_base const&) noexcept -> dictionary_iterator_base& = default;
  constexpr auto operator=(dictionary_iterator_base&&) noexcept -> dictionary_iterator_base& = default;
  constexpr ~dictionary_iterator_base() noexcept = default;

  [[nodiscard]] constexpr auto key() const noexcept(nothrow_dereference<KI>) -> key_reference { return *key_iter(); }
  [[nodiscard]] constexpr auto value() const noexcept(nothrow_dereference<VI>) -> mapped_reference {
    return *value_iter();
  }

  [[nodiscard]] constexpr auto dereference() const noexcept(nothrow_dereference<KI>&& nothrow_dereference<VI>)
      -> reference {
    return {key(), value()};
  }
  constexpr void increment() noexcept(nothrow_increment<KI>&& nothrow_increment<VI>) {
    ++key_iterator_;
    ++value_iterator_;
  }
  constexpr void decrement() noexcept(nothrow_decrement<KI>&& nothrow_decrement<VI>) {
    --key_iterator_;
    --value_iterator_;
  }
  constexpr void advance(difference_type n) noexcept(nothrow_advance<KI>&& nothrow_advance<VI>) {
    key_iterator_ += n;
    value_iterator_ += n;
  }

  template <std::sentinel_for<KI> K, std::sentinel_for<VI> V>
  [[nodiscard]] constexpr auto equals(dictionary_iterator_base<K, V> const& sentinel) const
      noexcept(nothrow_equals<KI, K> || nothrow_equals<VI, V>) -> bool {
    if constexpr (nothrow_equals<KI, K> || !nothrow_equals<VI, V>) {
      return key_iterator_ == sentinel.key_iterator_;
    } else {
      return value_iterator_ == sentinel.value_iterator_;
    }
  }

  template <std::sentinel_for<KI> K, std::sentinel_for<VI> V>
  [[nodiscard]] constexpr auto distance_to(dictionary_iterator_base<K, V> const& sentinel) const
      noexcept(nothrow_distance_to<KI, K> || nothrow_distance_to<VI, V>) -> difference_type {
    if constexpr (nothrow_distance_to<KI, K> || nothrow_distance_to<VI, V>) {
      return sentinel.key_iterator_ - key_iterator_;
    } else {
      return sentinel.value_iterator_ - value_iterator_;
    }
  }

 protected:
  [[nodiscard]] constexpr auto key_iter() const noexcept -> KI { return key_iterator_; }
  [[nodiscard]] constexpr auto value_iter() const noexcept -> VI { return value_iterator_; }

 private:
  KI key_iterator_;
  VI value_iterator_;
};

template <std::random_access_iterator KeyIter, std::random_access_iterator ValueIter>
class mutable_dictionary_iterator : public dictionary_iterator_base<KeyIter, ValueIter>,
                                    public iterator_facade<mutable_dictionary_iterator<KeyIter, ValueIter>> {
 public:
  using base = dictionary_iterator_base<KeyIter, ValueIter>;
  using key_type = base::key_type;
  using mapped_type = base::mapped_type;

  using base::advance;
  using base::base;
  using base::decrement;
  using base::distance_to;
  using base::equals;
  using base::increment;
  using base::key;
  using base::key_iter;
  using base::value;
  using base::value_iter;

  // need to specialize iter_move so that the referenced values can be moved, otherwise the pair assignment operator
  // will only forward based on the pair template types
  [[nodiscard]] friend constexpr auto iter_move(mutable_dictionary_iterator const& iter) noexcept(
      nothrow_dereference<KeyIter>&& nothrow_dereference<ValueIter>) -> decltype(auto) {
    return forward_as_pair(std::ranges::iter_move(iter.key_iter()), std::ranges::iter_move(iter.value_iter()));
  }

  // same for iter_swap
  friend constexpr void
  iter_swap(mutable_dictionary_iterator const& lhs, mutable_dictionary_iterator const& rhs) noexcept(noexcept(
      std::ranges::iter_swap(lhs.key_iter(), rhs.key_iter())) && noexcept(std::ranges::iter_swap(lhs.value_iter(),
                                                                                                 rhs.value_iter()))) {
    std::ranges::iter_swap(lhs.key_iter(), rhs.key_iter());
    std::ranges::iter_swap(lhs.value_iter(), rhs.value_iter());
  }
};

template <std::ranges::random_access_range K, std::ranges::random_access_range V>
using mutable_dictionary_iter_t = mutable_dictionary_iterator<std::ranges::iterator_t<K>, std::ranges::iterator_t<V>>;

// range imitating a single container of (key, value) pairs from 2 separate ones
template <std::ranges::random_access_range K, std::ranges::random_access_range V>
class mutable_dictionary_range {
 public:
  using key_type = std::ranges::range_value_t<K>;
  using mapped_type = std::ranges::range_value_t<V>;
  using value_type = std::pair<key_type, mapped_type>;
  using iterator = mutable_dictionary_iter_t<K, V>;
  using const_iterator = iterator;
  using size_type = std::common_type_t<std::ranges::range_size_t<K>, std::ranges::range_size_t<V>>;
  using difference_type = std::common_type_t<std::ranges::range_difference_t<K>, std::ranges::range_difference_t<V>>;

  constexpr mutable_dictionary_range(K& keys, V& values) noexcept
      : keys_(std::addressof(keys)), values_(std::addressof(values)) {}

  [[nodiscard]] constexpr auto begin() const noexcept -> const_iterator {
    return const_iterator(std::ranges::begin(*keys_), std::ranges::begin(*values_));
  }
  [[nodiscard]] constexpr auto end() const noexcept -> const_iterator {
    return const_iterator(std::ranges::end(*keys_), std::ranges::end(*values_));
  }

  template <class P>
    requires(!pair_like<P> && std::constructible_from<value_type>)
  constexpr void emplace_back(P&& value) {
    emplace_back(value_type(value));
  }

  template <class P>
    requires pair_like<P> && requires {
                               requires containers::back_emplaceable<K, tuple_element_t<0, P>>;
                               requires containers::back_emplaceable<V, tuple_element_t<1, P>>;
                             }
  constexpr void emplace_back(P&& value) {
    emplace_back_impl(std::forward<P>(value));
  }

  // enable erase algorithms
  constexpr void erase(const_iterator first, const_iterator last)
    requires containers::erasable<K> && containers::erasable<V>
  {
    containers::erase(*keys_, first.key_iter(), last.key_iter());
    containers::erase(*values_, first.value_iter(), last.value_iter());
  }

  constexpr void clear()
    requires containers::clearable<K> && containers::clearable<V>
  {
    containers::clear(*keys_);
    containers::clear(*values_);
  }

  constexpr void resize(size_type n)
    requires containers::resizable<K> && containers::resizable<V>
  {
    containers::resize(*keys_, static_cast<std::ranges::range_size_t<K>>(n));
    containers::resize(*values_, static_cast<std::ranges::range_size_t<V>>(n));
  }

 private:
  K* keys_;
  V* values_;

  template <class P>
  constexpr void emplace_back_impl(P&& value) {
    containers::emplace_back(*keys_, std::get<0>(std::forward<P>(value)));
    FLAT_HASH_TRY { containers::emplace_back(*values_, std::get<1>(std::forward<P>(value))); }
    FLAT_HASH_CATCH(...) {
      containers::pop_back(*keys_);
      FLAT_HASH_THROW();
    }
  }
};

}  // namespace detail

/**
 * @brief Dictionary iterator of (key, value) pairs. Assumes it is constructed with equivalent key and value iterators,
 * i.e. pointing to the same position in their respective containers of equal sizes
 *
 * @tparam KeyIter
 * @tparam ValueIter
 */
template <std::random_access_iterator KeyIter, std::random_access_iterator ValueIter>
class dictionary_iterator : public detail::dictionary_iterator_base<KeyIter, ValueIter>,
                            public iterator_facade<dictionary_iterator<KeyIter, ValueIter>> {
 public:
  using base = detail::dictionary_iterator_base<KeyIter, ValueIter>;
  using key_type = base::key_type;  // no reason to add const as unlike std::unordered_set keys are not stored in nodes
  using key_reference = detail::add_const_if_ref_t<typename base::key_reference>;
  using mapped_type = base::mapped_type;
  using mapped_reference = base::mapped_reference;

  using value_type = std::pair<key_type, mapped_type>;
  using reference = std::pair<key_reference, mapped_reference>;
  using difference_type = base::difference_type;

  using base::base;

  [[nodiscard]] constexpr auto key() const noexcept(nothrow_dereference<KeyIter>) -> key_reference {
    return *key_iter();
  }

  [[nodiscard]] constexpr auto dereference() const
      noexcept(nothrow_dereference<KeyIter>&& nothrow_dereference<ValueIter>) -> reference {
    return {key(), value()};
  }

  using base::advance;
  using base::decrement;
  using base::distance_to;
  using base::equals;
  using base::increment;
  using base::value;

 private:
  template <class K, class V, dictionary_traits_for<K, V>>
  friend class dictionary;

  using base::key_iter;
  using base::value_iter;

  // exposed keys are immutable

  [[nodiscard]] friend constexpr auto iter_move(dictionary_iterator const& iter) noexcept(
      nothrow_dereference<KeyIter>&& nothrow_dereference<ValueIter>) -> decltype(auto) {
    return detail::forward_as_pair(iter.key(), std::ranges::iter_move(iter.value_iter()));
  }

  friend constexpr void iter_swap(dictionary_iterator const& lhs, dictionary_iterator const& rhs) noexcept(
      noexcept(std::ranges::iter_swap(lhs.value_iter(), rhs.value_iter()))) {
    std::ranges::iter_swap(lhs.value_iter(), rhs.value_iter());
  }
};

template <dictionary_traits Traits>
struct dictionary_init {
  using hasher = Traits::hasher;
  using key_equal = Traits::key_equal;
  using probing_policy = Traits::probing_policy;
  using maybe_key_allocator = optional_allocator<typename Traits::key_container>;
  using maybe_value_allocator = optional_allocator<typename Traits::value_container>;
  using maybe_index_allocator = optional_allocator<typename Traits::index_container>;

  Traits traits{};
  hasher hash_function{};
  key_equal key_eq{};
  probing_policy policy{};

  // keeping possible allocators as non-optional members for simplicity
  maybe_key_allocator key_allocator{};
  maybe_value_allocator value_allocator{};
  maybe_index_allocator index_allocator{};
};

template <class Key, class Value, dictionary_traits_for<Key, Value> Traits>
class dictionary : private detail::hash_container_base_t<Traits>,
                   detail::containers::maybe_enable_allocator_type<typename Traits::value_container,
                                                                   typename Traits::key_container> {
  template <class K, class V, dictionary_traits_for<K, V>>
  friend class dictionary;

 public:
  using base = detail::hash_container_base_t<Traits>;
  using traits_type = Traits;
  using options_type = dictionary_init<traits_type>;
  using index_container = base::index_container;
  using key_container = Traits::key_container;
  using value_container = Traits::value_container;
  using probing_policy = base::probing_policy;
  using hash_table = base::hash_table_type;
  using index_type = base::value_type;
  constexpr static ordering_policy ordering = Traits::ordering;

  using key_type = Key;
  using mapped_type = Value;
  using size_type = std::uint64_t;
  using difference_type = std::int64_t;
  using hasher = base::hasher;
  using key_equal = base::key_equal;
  using hash_type = base::template hash_type<Key>;
  // allocator_type...
  // keys are not mutable
  using iterator =
      dictionary_iterator<std::ranges::iterator_t<key_container const>, std::ranges::iterator_t<value_container>>;
  using const_iterator =
      dictionary_iterator<std::ranges::iterator_t<key_container const>, std::ranges::iterator_t<value_container const>>;
  using value_type = iterator::value_type;
  using mutable_value_type = std::pair<key_type, mapped_type>;
  using reference = iterator::reference;
  using const_reference = const_iterator::reference;
  using keys_view = set<Key, ref_set_traits<Traits>>;
  using keys_const_view = keys_view;  // keys are immutable so keys_view is always const
  using values_view = detail::ref_view_t<value_container>;
  using values_const_view = detail::ref_view_t<value_container const>;

  constexpr static inline size_type npos = std::numeric_limits<size_type>::max();

  constexpr dictionary()
    requires detail::default_constructible_dict<dictionary>
  {
    FLAT_HASH_ASSERT(std::ranges::empty(keys_) && std::ranges::empty(values_), "Dictionary not empty:\n{}\n{}",
                     detail::maybe_format_arg(keys_), detail::maybe_format_arg(values_));
  }

  constexpr explicit dictionary(dictionary_init<Traits> init)
      : base(0, init.policy, init.index_allocator, init.hash_function, init.key_eq),
        keys_(detail::containers::make_container<key_container>(0, init.key_allocator)),
        values_(detail::containers::make_container<value_container>(0, init.value_allocator)),
        traits_(std::move(init.traits)) {
    FLAT_HASH_ASSERT(std::ranges::empty(keys_) && std::ranges::empty(values_), "Keys and values not empty:\n{}\n{}",
                     detail::maybe_format_arg(keys_), detail::maybe_format_arg(values_));
  }

  constexpr explicit dictionary(size_type bucket_count, dictionary_init<Traits> init = {})
    requires detail::default_constructible_dict<dictionary>
  : base(bucket_count, init.policy, init.index_allocator, init.hash_function, init.key_eq),
    keys_(detail::containers::make_container<key_container>(0, init.key_allocator)),
    values_(detail::containers::make_container<key_container>(0, init.value_allocator)),
    traits_(std::move(init.traits)) {
    if constexpr (detail::containers::reservable<key_container>) {
      detail::containers::reserve(keys_, base::supported_keys_capacity());
    }
    if constexpr (detail::containers::reservable<value_container>) {
      detail::containers::reserve(values_, base::supported_keys_capacity());
    }
    FLAT_HASH_ASSERT(std::ranges::empty(keys_) && std::ranges::empty(values_), "Keys and values not empty:\n{}\n{}",
                     detail::maybe_format_arg(keys_), detail::maybe_format_arg(values_));
  }

  template <std::ranges::input_range R>
    requires std::constructible_from<value_type, std::ranges::range_reference_t<R>> &&
                 detail::mutable_range<index_container>
  constexpr explicit dictionary(R&& r, size_type bucket_count = npos, dictionary_init<Traits> init = {})
      : base(bucket_count == npos
                 ? detail::at_least<size_type>(detail::containers::size_hint_or(r, hash_table::default_size / 2),
                                               init.policy.max_load_factor())
                 : bucket_count,
             init.policy, init.index_allocator, init.hash_function, init.key_eq),
        keys_(detail::containers::make_container<key_container>(0, init.key_allocator)),
        values_(detail::containers::make_container<key_container>(0, init.value_allocator)),
        traits_(std::move(init.traits)) {
    if constexpr (detail::dict_pair_insertible<std::ranges::range_reference_t<R>, dictionary>) {
      merge(std::forward<R>(r));
    } else {
      assign(std::forward<R>(r));
    }
  }

  template <std::input_iterator It>
    requires std::constructible_from<value_type, std::iter_reference_t<It>> && detail::mutable_range<index_container>
  constexpr dictionary(It first, It last, size_type bucket_count = npos, dictionary_init<Traits> init = {})
      : dictionary(std::ranges::subrange(first, last), bucket_count, init) {}

  constexpr dictionary(std::initializer_list<value_type> ilist, size_type bucket_count = npos,
                       dictionary_init<Traits> init = {})
    requires detail::mutable_range<index_container>
  : base(bucket_count == npos ? detail::at_least<size_type>(ilist.size(), init.policy.max_load_factor()) : bucket_count,
         init.policy, init.index_allocator, init.hash_function, init.key_eq),
    keys_(detail::containers::make_container<key_container>(0, init.key_allocator)),
    values_(detail::containers::make_container<value_container>(0, init.value_allocator)),
    traits_(std::move(init.traits)) {
    if constexpr (detail::dict_pair_insertible<value_type, dictionary>) {
      merge(ilist);
    } else {
      assign(ilist);
    }
  }

  template <class T>
    requires(same_lookup_as<Traits, T> && !std::same_as<T, Traits> &&
             std::constructible_from<key_container, typename T::key_container&> &&
             std::constructible_from<value_container, typename T::value_container&> &&
             std::constructible_from<base, typename dictionary<Key, Value, T>::base&>)
  constexpr explicit(!(std::convertible_to<typename T::key_container&, key_container> &&
                       std::convertible_to<typename T::value_container&, value_container> &&
                       std::convertible_to<typename dictionary<Key, Value, T>::base&, base>))
      dictionary(dictionary<Key, Value, T>& other, Traits t = Traits()) noexcept(
          (std::is_nothrow_move_constructible_v<Traits> &&
           std::is_nothrow_constructible_v<key_container, typename T::key_container&> &&
           std::is_nothrow_constructible_v<value_container, typename T::value_container&> &&
           std::is_nothrow_constructible_v<base, typename dictionary<Key, Value, T>::base&>))
      : base(other), keys_(other.keys_), values_(other.values_), traits_(std::move(t)) {}

  template <class T>
    requires(same_lookup_as<Traits, T> && !std::same_as<T, Traits> &&
             std::constructible_from<key_container, typename T::key_container const&> &&
             std::constructible_from<value_container, typename T::value_container const&> &&
             std::constructible_from<base, typename dictionary<Key, Value, T>::base const&>)
  constexpr explicit(!(std::convertible_to<typename T::key_container const&, key_container> &&
                       std::convertible_to<typename T::value_container const&, value_container> &&
                       std::convertible_to<typename dictionary<Key, Value, T>::base const&, base>))
      dictionary(dictionary<Key, Value, T> const& other, Traits t = Traits()) noexcept(
          (std::is_nothrow_move_constructible_v<Traits> &&
           std::is_nothrow_constructible_v<key_container, typename T::key_container const&> &&
           std::is_nothrow_constructible_v<value_container, typename T::value_container const&> &&
           std::is_nothrow_constructible_v<base, typename dictionary<Key, Value, T>::base const&>))
      : base(other), keys_(other.keys_), values_(other.values_), traits_(std::move(t)) {}

  /**
   * @brief Default copy constructor
   *
   */
  constexpr dictionary(dictionary const& other) noexcept(
      detail::dict_apply<dictionary, std::is_nothrow_copy_constructible>) = default;

  /**
   * @brief Default move constructor
   *
   */
  constexpr dictionary(dictionary&& other) noexcept(
      detail::dict_apply<dictionary, std::is_nothrow_move_constructible>) = default;

  /**
   * @brief Default copy assignment operator
   *
   * @return dictionary&
   */
  constexpr auto operator=(dictionary const&) noexcept(detail::dict_apply<dictionary, std::is_nothrow_copy_assignable>)
      -> dictionary& = default;

  /**
   * @brief Default move assignment operator
   *
   * @return dictionary&
   */
  constexpr auto operator=(dictionary&&) noexcept(detail::dict_apply<dictionary, std::is_nothrow_move_assignable>)
      -> dictionary& = default;

  /**
   * @brief Default destructor
   *
   */
  constexpr ~dictionary() noexcept(detail::dict_apply<dictionary, std::is_nothrow_destructible>) = default;

  [[nodiscard]] constexpr auto get_allocator() const noexcept -> detail::containers::allocator_t<value_container>
    requires(detail::containers::gettable_allocator<value_container>)
  {
    return detail::containers::get_allocator(values_);
  }

  [[nodiscard]] constexpr auto get_allocator() const noexcept -> detail::containers::allocator_t<key_container>
    requires(detail::containers::gettable_allocator<key_container> &&
             !detail::containers::gettable_allocator<value_container>)
  {
    return detail::containers::get_allocator(keys_);
  }

  /**
   * @brief Get allocator used by the key_container.
   *
   */
  [[nodiscard]] constexpr auto get_keys_allocator() const noexcept -> detail::containers::allocator_t<key_container>
    requires(detail::containers::gettable_allocator<key_container>)
  {
    return detail::containers::get_allocator(keys_);
  }

  /**
   * @brief Get allocator used by the hash_table and index_container.
   *
   */
  [[nodiscard]] constexpr auto get_hash_table_allocator() const noexcept -> detail::containers::allocator_t<base>
    requires(detail::containers::gettable_allocator<base>)
  {
    return base::get_allocator();
  }

  /**
   * @brief Iterator the beginning of the stored keys and values.
   *
   * @return const_iterator
   */
  [[nodiscard]] constexpr auto cbegin() const noexcept -> const_iterator {
    return const_iterator(std::ranges::cbegin(keys_), std::ranges::cbegin(values_));
  }

  /**
   * @brief Iterator the end of the stored keys and values.
   *
   * @return const_iterator
   */
  [[nodiscard]] constexpr auto cend() const noexcept -> const_iterator {
    FLAT_HASH_ASSERT(std::ranges::size(keys_) == std::ranges::size(values_),
                     "Keys and values are different sizes: {} and {}", std::ranges::size(keys_),
                     std::ranges::size(values_));
    return const_iterator(std::ranges::cend(keys_), std::ranges::cend(values_));
  }

  /**
   * @copydoc dictionary::cbegin()
   */
  [[nodiscard]] constexpr auto begin() const noexcept -> const_iterator { return cbegin(); }

  /**
   * @copydoc dictionary::cend()
   */
  [[nodiscard]] constexpr auto end() const noexcept -> const_iterator { return cend(); }

  /**
   * @copydoc dictionary::cbegin()
   */
  [[nodiscard]] constexpr auto begin() noexcept -> iterator {
    return iterator(std::ranges::cbegin(keys_), std::ranges::begin(values_));
  }

  /**
   * @copydoc dictionary::cend()
   */
  [[nodiscard]] constexpr auto end() noexcept -> iterator {
    FLAT_HASH_ASSERT(std::ranges::size(keys_) == std::ranges::size(values_),
                     "Keys and values are different sizes: {} and {}", std::ranges::size(keys_),
                     std::ranges::size(values_));
    return iterator(std::ranges::cend(keys_), std::ranges::end(values_));
  }

  /**
   * @brief Check if there are no keys and values stored
   *
   * @return true if no keys and values are stored
   * @return false otherwise
   */
  [[nodiscard]] constexpr auto empty() const noexcept -> bool { return std::ranges::empty(keys_); }

  /**
   * @brief Number of keys and values stored
   *
   * @return size_type
   */
  [[nodiscard]] constexpr auto size() const noexcept -> size_type { return std::ranges::size(keys_); }

  /**
   * @copydoc dictionary::size()
   */
  [[nodiscard]] constexpr auto ssize() const noexcept -> difference_type { return std::ranges::ssize(keys_); }

  /**
   * @brief Capacity of the key_container/value_container, whichever is smaller, may fall back to container size.
   *
   * @return size_type
   */
  [[nodiscard]] constexpr auto capacity() const noexcept -> size_type {
    return std::min<size_type>(detail::containers::capacity(keys_), detail::containers::capacity(values_));
  }

  /**
   * @brief Maximum number of keys that can be stored. Depends on the index type size and probing_policy used assuming
   * unlimited memory.
   *
   * @return size_type
   */
  [[nodiscard]] constexpr auto max_size() const noexcept -> size_type { return base::template max_keys<key_type>(); }

  /**
   * @brief Clear the keys and values and reset all buckets to empty.
   *
   */
  constexpr void clear() noexcept
    requires detail::clearable_dict<dictionary>
  {
    if (empty()) { return; }
    mutable_range().clear();
    base::clear();
  }

  /**
   * @brief Try inserting a (key, value) pair at the specified position
   *
   * @param pos position to insert the value at
   * @param value value to insert
   * @return std::pair<iterator, bool> iterator to the inserted value or the element preventing insertion, true if
   * inserted and false otherwise
   */
  constexpr auto try_insert(const_iterator pos, value_type&& value) -> std::pair<iterator, bool>
    requires detail::dict_pair_insertible<value_type, dictionary>
  {
    return try_insert_impl(pos, std::move(value));
  }

  /**
   * @copydoc dictionary::try_insert(const_iterator, value_type&&)
   */
  constexpr auto try_insert(const_iterator pos, value_type const& value) -> std::pair<iterator, bool>
    requires detail::dict_pair_insertible<value_type, dictionary>
  {
    return try_insert_impl(pos, value);
  }

  /**
   * @brief Try inserting a (key, value) pair at the specified position. Heterogeneous lookup overload.
   *
   * @copydetail dictionary::try_insert(const_iterator, value_type&&)
   */
  template <detail::dict_pair_insertible<dictionary> P>
  constexpr auto try_insert(const_iterator pos, P&& value) -> std::pair<iterator, bool> {
    return try_insert_impl(pos, std::forward<P>(value));
  }

  /**
   * @brief Insert a new (key, value) pair. The behaviour on duplicate values depends on the supplied traits
   * Traits::on_duplicate_key(key_type const&)
   *
   * @param pos position to insert value at
   * @param value value to insert
   * @return iterator Iterator to the inserted value or the element preventing insertion
   */
  constexpr auto insert(const_iterator pos, value_type&& value) -> iterator {
    return insert_impl(pos, std::move(value));
  }

  /**
   * @copydoc dictionary::insert(const_iterator, value_type&&)
   */
  constexpr auto insert(const_iterator pos, value_type const& value) -> iterator { return insert_impl(pos, value); }

  /**
   * @copybrief dictionary::insert(const_iterator, value_type&&)
   * Heterogeneous lookup overload.
   *
   * @copydetail dictionary::insert(const_iterator, value_type&&)
   */
  template <detail::dict_pair_insertible<dictionary> P>
  constexpr auto insert(const_iterator pos, P&& value) -> iterator {
    return insert_impl(pos, std::forward<P>(value));
  }

  /**
   * @brief Try inserting new (key, value) pair to the end of the dictionary.
   *
   * @param value value to insert
   * @return std::pair<iterator, bool> iterator to the inserted value or the element preventing insertion, true if
   * inserted and false otherwise
   */
  constexpr auto try_insert(value_type&& value) -> std::pair<iterator, bool> {
    return try_insert(cend(), std::move(value));
  }

  /**
   * @copydoc dictionary::try_insert(value_type&&)
   */
  constexpr auto try_insert(value_type const& value) -> std::pair<iterator, bool> { return try_insert(cend(), value); }

  /**
   * @copybrief dictionary::try_insert(value_type&&)
   * Heterogeneous lookup overload.
   *
   * @copydetail dictionary::try_insert(value_type&&)
   */
  template <detail::dict_pair_insertible<dictionary> P>
  constexpr auto try_insert(P&& value) -> std::pair<iterator, bool> {
    return try_insert(cend(), std::forward<P>(value));
  }

  /**
   * @copydoc dictionary::try_insert(value_type&&)
   */
  constexpr auto insert(value_type&& value) -> std::pair<iterator, bool> {
    return try_insert(cend(), std::move(value));
  }

  /**
   * @copydoc dictionary::try_insert(value_type const&)
   */
  constexpr auto insert(value_type const& value) -> std::pair<iterator, bool> { return try_insert(cend(), value); }

  /**
   * @copydoc dictionary::try_insert(K&&)
   */
  template <detail::dict_pair_insertible<dictionary> P>
  constexpr auto insert(P&& value) -> std::pair<iterator, bool> {
    return try_insert(cend(), std::forward<P>(value));
  }

  /**
   * @brief Insert a range of values at the end of the dictionary. Duplicates are ignored and values are consumed in
   * the order of iteration.
   *
   * @tparam It
   * @tparam S
   * @param first iterator the beginning of the range
   * @param last to range end
   */
  template <std::input_iterator It, std::sentinel_for<It> S>
    requires(detail::dict_pair_insertible<std::iter_value_t<It>, dictionary>)
  constexpr void insert(It first, S last) {
    insert(std::ranges::subrange(first, last));
  }

  /**
   * @copybrief dictionary::insert(It, S)
   *
   * @tparam R
   * @param range range of values
   */
  template <std::ranges::input_range R>
    requires(detail::dict_pair_insertible<std::ranges::range_value_t<R>, dictionary>)
  constexpr void insert(R&& range) {
    merge(std::forward<R>(range));
  }

  template <class M>
    requires(std::constructible_from<mapped_type, M &&> && std::is_assignable_v<mapped_type&, M &&>)
  constexpr auto insert_or_assign(key_type const& key, M&& obj) -> std::pair<iterator, bool> {
    return insert_or_assign_impl(cend(), key, std::forward<M>(obj));
  }

  template <class M>
    requires(std::constructible_from<mapped_type, M &&> && std::is_assignable_v<mapped_type&, M &&>)
  constexpr auto insert_or_assign(key_type&& key, M&& obj) -> std::pair<iterator, bool> {
    return insert_or_assign_impl(cend(), std::move(key), std::forward<M>(obj));
  }

  template <detail::hashed_lookup_key<dictionary> K, class M>
    requires(std::constructible_from<mapped_type, M &&> && std::is_assignable_v<mapped_type&, M &&>)
  constexpr auto insert_or_assign(K&& key, M&& obj) -> std::pair<iterator, bool> {
    return insert_or_assign_impl(cend(), std::forward<K>(key), std::forward<M>(obj));
  }

  template <class M>
    requires(std::constructible_from<mapped_type, M &&> && std::is_assignable_v<mapped_type&, M &&>)
  constexpr auto insert_or_assign(const_iterator pos, key_type const& key, M&& obj) -> std::pair<iterator, bool> {
    return insert_or_assign_impl(pos, key, std::forward<M>(obj));
  }

  template <class M>
    requires(std::constructible_from<mapped_type, M &&> && std::is_assignable_v<mapped_type&, M &&>)
  constexpr auto insert_or_assign(const_iterator pos, key_type&& key, M&& obj) -> std::pair<iterator, bool> {
    return insert_or_assign_impl(pos, std::move(key), std::forward<M>(obj));
  }

  template <detail::hashed_lookup_key<dictionary> K, class M>
    requires(std::constructible_from<mapped_type, M &&> && std::is_assignable_v<mapped_type&, M &&>)
  constexpr auto insert_or_assign(const_iterator pos, K&& key, M&& obj) -> std::pair<iterator, bool> {
    return insert_or_assign_impl(pos, std::forward<K>(key), std::forward<M>(obj));
  }

  /**
   * @brief Insert value at specified position. Unlike std::map and std::unordered_map, the first argument is not a hint
   * but a position to insert the element at.
   *
   * @tparam Args
   * @param pos position to insert value at
   * @param key new element key
   * @param args mapped_type constructor arguments
   *
   * @return std::pair<iterator, bool> iterator to the inserted element or the element preventing insertion, true if a
   * new value was inserted and false otherwise
   */
  template <class... Args>
    requires(detail::containers::constructible_from<mapped_type, Args...> &&
             detail::dict_insertible<key_type, mapped_type, dictionary>)
  constexpr auto try_emplace(const_iterator pos, key_type const& key, Args&&... args) -> iterator {
    auto ctor = detail::containers::make_constructor_ref<mapped_type>(std::forward<Args>(args)...);
    return try_insert(pos, detail::forward_as_pair(key, ctor)).first;
  }

  /**
   * @copydoc dictionary::try_emplace(const_iterator, key_type const&, Args&&...)
   */
  template <class... Args>
    requires(detail::containers::constructible_from<mapped_type, Args...> &&
             detail::dict_insertible<key_type, mapped_type, dictionary>)
  constexpr auto try_emplace(const_iterator pos, key_type&& key, Args&&... args) -> iterator {
    auto ctor = detail::containers::make_constructor_ref<mapped_type>(std::forward<Args>(args)...);
    return try_insert(pos, detail::forward_as_pair(std::move(key), ctor)).first;
  }

  /**
   * @copydoc dictionary::try_emplace(const_iterator, key_type const&, Args&&...)
   */
  template <detail::hashed_lookup_key<dictionary> K, class... Args>
    requires(detail::containers::constructible_from<mapped_type, Args...> &&
             detail::dict_insertible<key_type, mapped_type, dictionary>)
  constexpr auto try_emplace(const_iterator pos, K&& key, Args&&... args) -> iterator {
    auto ctor = detail::containers::make_constructor_ref<mapped_type>(std::forward<Args>(args)...);
    return try_insert(pos, detail::forward_as_pair(std::forward<K>(key), ctor)).first;
  }

  /**
   * @brief Insert value at the end.
   *
   * @tparam Args
   * @param key new element key
   * @param args mapped_type constructor arguments
   *
   * @return std::pair<iterator, bool> iterator to the inserted element or the element preventing insertion, true if a
   * new value was inserted and false otherwise
   */
  template <class... Args>
    requires(detail::containers::constructible_from<mapped_type, Args...> &&
             detail::dict_insertible<key_type, mapped_type, dictionary>)
  constexpr auto try_emplace(key_type const& key, Args&&... args) -> std::pair<iterator, bool> {
    auto ctor = detail::containers::make_constructor_ref<mapped_type>(std::forward<Args>(args)...);
    return try_insert(cend(), detail::forward_as_pair(key, ctor));
  }

  /**
   * @copydoc dictionary::try_emplace(key_type const&, Args&&...)
   */
  template <class... Args>
    requires(detail::containers::constructible_from<mapped_type, Args...> &&
             detail::dict_insertible<key_type, mapped_type, dictionary>)
  constexpr auto try_emplace(key_type&& key, Args&&... args) -> std::pair<iterator, bool> {
    auto ctor = detail::containers::make_constructor_ref<mapped_type>(std::forward<Args>(args)...);
    return try_insert(cend(), detail::forward_as_pair(std::move(key), ctor));
  }

  /**
   * @copydoc dictionary::try_emplace(key_type const&, Args&&...)
   */
  template <detail::hashed_lookup_key<dictionary> K, class... Args>
    requires(detail::containers::constructible_from<mapped_type, Args...> &&
             detail::dict_insertible<key_type, mapped_type, dictionary>)
  constexpr auto try_emplace(K&& key, Args&&... args) -> std::pair<iterator, bool> {
    auto ctor = detail::containers::make_constructor_ref<mapped_type>(std::forward<Args>(args)...);
    return try_insert(cend(), detail::forward_as_pair(std::forward<K>(key), ctor));
  }

  /**
   * @copybrief dictionary::try_emplace_hint(const_iterator, Args&&)
   *
   * @tparam Args
   * @param pos position to insert value at
   * @param args value_type constructor arguments
   * @return iterator to the inserted element or the element preventing insertion
   */
  template <class... Args>
    requires(detail::containers::constructible_from<value_type, Args...> &&
             detail::dict_pair_insertible<value_type, dictionary>)
  constexpr auto emplace_hint(const_iterator pos, Args&&... args) -> iterator {
    auto value = detail::containers::construct<mutable_value_type>(std::forward<Args>(args)...);
    return try_insert(pos, std::move(value)).first;
  }

  /**
   * @brief dictionary::try_emplace(Args&&)
   *
   * @copydetail dictionary::emplace_hint(const_iterator, Args&&)
   */
  template <class... Args>
    requires(detail::containers::constructible_from<value_type, Args...> &&
             detail::dict_pair_insertible<value_type, dictionary>)
  constexpr auto emplace(Args&&... args) -> std::pair<iterator, bool> {
    auto value = detail::containers::construct<mutable_value_type>(std::forward<Args>(args)...);
    return try_insert(cend(), std::move(value));
  }

  /**
   * @brief Remove value from the dictionary
   *
   * @param key value to remove
   * @return 1 if a value matching key was removed
   * @return 0 otherwise
   */
  constexpr auto erase(key_type const& key) -> size_type
    requires detail::dict_erasable_key<key_type, dictionary>
  {
    return extract(key) ? 1 : 0;
  }

  /**
   * @copybrief dictionary::erase(key_type const&)
   * Heterogeneous overload.
   *
   * @copydetail dictionary::erase(key_type const&)
   */
  template <detail::dict_erasable_key<dictionary> K>
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
    requires detail::dict_erasable_key<key_type, dictionary>
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
    requires detail::dict_erasable_key<key_type, dictionary>
  {
    auto offset = first - cbegin();
    if (last != first) {
      if (last - first == 1) {
        erase(first);
      } else {
        // TODO: for low numbers of elements to erase erase them one by one?
        auto range = mutable_range();
        auto beg = range.begin();
        detail::containers::policy_erase<ordering>(range, beg + offset, beg + (last - cbegin()));
        rehash(0);
      }
    }

    return begin() + offset;
  }

  /**
   * @brief Swap two sets.
   *
   */
  constexpr void swap(dictionary& rhs) noexcept(detail::nothrow_swappable_dict<dictionary>)
    requires detail::swappable_dict<dictionary>
  {
    if (this == &rhs) [[unlikely]] { return; }
    base::swap(rhs);
    std::ranges::swap(keys_, rhs.keys_);
    std::ranges::swap(values_, rhs.values_);
    std::ranges::swap(traits_, rhs.traits_);
  }

  /**
   * @brief Extract the specified value. Unlike standard containers, the return type is not a node handle.
   *
   * @tparam K
   * @param key value to extract
   * @return std::optional<value_type> the extracted value or nothing if it was not found
   */
  template <detail::dict_erasable_key<dictionary> K>
  constexpr auto extract(K const& key) -> std::optional<value_type> {
    return extract_impl(key);
  }

  /**
   * @brief Extract the element at the specified position
   *
   * @param pos iterator to the element to extract
   * @return value_type extracted element
   */
  constexpr auto extract(const_iterator pos) -> value_type
    requires detail::dict_erasable_key<key_type, dictionary>
  {
    FLAT_HASH_ASSERT(pos != cend());
    return extract_impl(pos);
  }

  /**
   * @brief Add elements from the range to the end of dictionary. Ranges for which
   * std::ranges::borrowed_range<R>::value == false will have their values consumed (moved from). Unlike
   * std::unordered_set, merge does not splice two sets, use dictionary::splice in that case.
   *
   * @tparam R
   * @param source range of values, unique_range containers may be faster.
   */
  template <std::ranges::input_range R>
    requires(detail::dict_pair_insertible<std::ranges::range_value_t<R>, dictionary>)
  constexpr void merge(R&& source) {
    try_reserve_for(source);

    if constexpr (unique_map<R>) {
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
   * @param other compatible dictionary to transfer values from
   */
  template <class K, class V, class T>
    requires detail::dict_insertible<K, V, dictionary>
  constexpr void splice(dictionary<K, V, T>& other) {
    try_reserve_for(other);
    // private method exposes mutable key values
    other.erase_if(
        [this](auto&& key_value) {
          auto&& [key, value] = key_value;
          return try_insert_impl(cend(), detail::forward_as_pair(std::move(key), std::move(value))).second;
        },
        true);
  }

  /**
   * @brief Combine two sets by moving values from other into this. other is left in an unspecified state without the
   * transferred values, valid state can be reacquired by calling other.rehash(0).
   *
   * @copydetail dictionary::splice(dictionary<K,V,T>&)
   */
  template <class K, class V, class T>
    requires detail::dict_insertible<K, V, dictionary>
  constexpr void splice(dictionary<K, V, T>&& other) {
    try_reserve_for(other);
    // consuming, so no need to update the hash_table
    other.erase_if(
        [this](auto&& key_value) {
          auto&& [key, value] = key_value;
          return try_insert_impl(cend(), detail::forward_as_pair(std::move(key), std::move(value))).second;
        },
        false);
  }

  // Lookup
  constexpr auto at(key_type const& key) -> mapped_type& { return dictionary::at_impl(*this, key).value(); }

  [[nodiscard]] constexpr auto at(key_type const& key) const -> mapped_type const& {
    return dictionary::at_impl(*this, key).value();
  }

  template <detail::hashed_lookup_key<dictionary> K>
  constexpr auto at(K const& key) -> mapped_type& {
    return dictionary::at_impl(*this, key).value();
  }

  template <detail::hashed_lookup_key<dictionary> K>
  [[nodiscard]] constexpr auto at(K const& key) const -> mapped_type const& {
    return dictionary::at_impl(*this, key).value();
  }

  constexpr auto operator[](key_type const& key) -> mapped_type&
    requires detail::dict_subscriptable<key_type const&, dictionary>
  {
    return get_or_init(key);
  }

  constexpr auto operator[](key_type&& key) -> mapped_type&
    requires detail::dict_subscriptable<key_type&&, dictionary>
  {
    return get_or_init(std::move(key));
  }

  template <detail::dict_subscriptable<dictionary> K>
  constexpr auto operator[](K&& key) -> mapped_type& {
    return get_or_init(std::forward<K>(key));
  }

  /**
   * @brief Check if key is held by this dictionary
   *
   * @param key key value
   * @return true if key is held
   * @return false otherwise
   */
  [[nodiscard]] constexpr auto contains(key_type const& key) const noexcept -> bool { return find(key) != end(); }

  /**
   * @copybrief dictionary::contains(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail dictionary::contains(key_type const&)
   */
  template <detail::hashed_lookup_key<dictionary> K>
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
   * @copybrief dictionary::count(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail dictionary::count(key_type const&)
   */
  template <detail::hashed_lookup_key<dictionary> K>
  [[nodiscard]] constexpr auto count(K const& key) const noexcept -> size_type {
    return contains(key) ? 1 : 0;
  }

  /**
   * @brief Find the iterator to the key
   *
   * @param key key value
   * @return iterator iterator to the element with the mapped value or end otherwise
   */
  [[nodiscard]] constexpr auto find(key_type const& key) noexcept -> iterator {
    auto bucket = base::find_in(key, keys_);
    if (bucket != base::end()) { return begin() + static_cast<difference_type>(*bucket); }
    return end();
  }

  /**
   * @copybrief dictionary::find(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail dictionary::find(key_type const&)
   */
  template <detail::hashed_lookup_key<dictionary> K>
  [[nodiscard]] constexpr auto find(K const& key) noexcept -> iterator {
    auto bucket = base::find_in(key, keys_);
    if (bucket != base::end()) { return begin() + static_cast<difference_type>(*bucket); }
    return end();
  }

  /**
   * @copydoc dictionary::find(key_type const&)
   */
  [[nodiscard]] constexpr auto find(key_type const& key) const noexcept -> const_iterator {
    auto bucket = base::find_in(key, keys_);
    if (bucket != base::end()) { return begin() + static_cast<difference_type>(*bucket); }
    return end();
  }

  /**
   * @copybrief dictionary::find(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail dictionary::find(key_type const&)
   */
  template <detail::hashed_lookup_key<dictionary> K>
  [[nodiscard]] constexpr auto find(K const& key) const noexcept -> const_iterator {
    auto bucket = base::find_in(key, keys_);
    if (bucket != base::end()) { return begin() + static_cast<difference_type>(*bucket); }
    return end();
  }

  /**
   * @brief Find the range of buckets holding key. Values are unique so the range is always of size 1 or 0 depending if
   * the key is held.
   *
   * @tparam K
   * @param key key value
   * @return [first, last) range containing key if found, {end, end} otherwise
   */
  [[nodiscard]] constexpr auto equal_range(key_type const& key) noexcept -> std::pair<iterator, iterator> {
    auto iter = find(key);
    if (iter == end()) { return {end(), end()}; }
    return {iter, iter + 1};
  }

  /**
   * @copybrief dictionary::equal_range(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail dictionary::equal_range(key_type const&)
   */
  template <detail::hashed_lookup_key<dictionary> K>
  [[nodiscard]] constexpr auto equal_range(K const& key) noexcept -> std::pair<iterator, iterator> {
    auto iter = find(key);
    if (iter == end()) { return {end(), end()}; }
    return {iter, iter + 1};
  }

  /**
   * @copydoc dictionary::equal_range(key_type const&)
   */
  [[nodiscard]] constexpr auto equal_range(key_type const& key) const noexcept
      -> std::pair<const_iterator, const_iterator> {
    auto iter = find(key);
    if (iter == end()) { return {end(), end()}; }
    return {iter, iter + 1};
  }

  /**
   * @copybrief dictionary::equal_range(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail dictionary::equal_range(key_type const&)
   */
  template <detail::hashed_lookup_key<dictionary> K>
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
  [[nodiscard]] constexpr auto bucket_count() const noexcept -> size_type { return base::bucket_count(); }

  /**
   * @brief Maximum number of buckets dependant on the hash type, system limits are ignored
   *
   * @return size_type
   */
  [[nodiscard]] constexpr auto max_bucket_count() const noexcept -> size_type {
    return base::template max_bucket_count<key_type>();
  }

  /**
   * @brief Number of elements the bucket points to
   *
   * @param bucket index to the bucket
   * @return size_type 1 if bucket points to a key value, 0 otherwise
   */
  [[nodiscard]] constexpr auto bucket_size(size_type bucket) const noexcept -> size_type {
    FLAT_HASH_ASSERT(bucket < bucket_count());
    return (base::begin() + static_cast<base::difference_type>(bucket)).holds_value() ? 1 : 0;
  }

  /**
   * @brief Find bucket index holding key
   *
   * @param key key value
   * @return size_type index to the hash table bucket
   */
  [[nodiscard]] constexpr auto bucket(key_type const& key) const noexcept -> size_type {
    return static_cast<size_type>(base::find_in(key, keys_) - base::begin());
  }

  /**
   * @copybrief dictionary::bucket(key_type const&)
   * Heterogeneous lookup overload.
   *
   * @copydetail dictionary::bucket(key_type const&)
   */
  template <detail::hashed_lookup_key<dictionary> K>
  [[nodiscard]] constexpr auto bucket(K const& key) const noexcept -> size_type {
    return static_cast<size_type>(base::find_in(key, keys_) - base::begin());
  }

  // Hash policy
  /**
   * @brief Current load factor
   *
   * @return float
   */
  [[nodiscard]] constexpr auto load_factor() const noexcept -> float { return base::load_factor(size()); }

  /**
   * @brief Maximum load factor
   *
   * @return float
   */
  [[nodiscard]] constexpr auto max_load_factor() const noexcept -> float { return base::max_load_factor(); }

  /**
   * @brief Set the number of buckets and rehash the table. count = 0 will force rehash the table without changing the
   * number of buckets.
   *
   * @param count new number of buckets
   */
  constexpr void rehash(size_type count)
    requires detail::mutable_range<index_container>
  {
    base::rehash(count, keys_);
  }

  /**
   * @brief Reserve space for keys and buckets, may require a rehash
   *
   * @param n number of keys to reserve for
   */
  constexpr void reserve(size_type n)
    requires detail::reservable_dict<dictionary>
  {
    detail::containers::reserve(keys_, n);
    detail::containers::reserve(values_, n);
    if (n <= size()) return;
    base::reserve_for(keys_);
  }

  // Observers
  /**
   * @brief Hasher accessor
   *
   * @return hasher
   */
  [[nodiscard]] constexpr auto hash_function() const noexcept(std::is_nothrow_copy_constructible_v<hasher>) -> hasher {
    return base::hash_function();
  }

  /**
   * @brief Key equality comparison accessor
   *
   * @return key_equal
   */
  [[nodiscard]] constexpr auto key_eq() const noexcept(std::is_nothrow_copy_constructible_v<key_equal>) -> key_equal {
    return base::key_eq();
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
  constexpr auto probing() noexcept -> probing_policy& { return base::probing(); }

  /**
   * @brief Probing policy accessor
   *
   * @return probing_policy const&
   */
  [[nodiscard]] constexpr auto probing() const noexcept -> probing_policy const& { return base::probing(); }

  /**
   * @brief Swap two dictionaries.
   *
   * @param lhs
   * @param rhs
   */
  constexpr friend void swap(dictionary& lhs, dictionary& rhs) noexcept(detail::nothrow_swappable_dict<dictionary>)
    requires detail::swappable_dict<dictionary>
  {
    lhs.swap(rhs);
  }

  /**
   * @brief Erase values matching a predicate
   *
   * @param self dictionary
   * @param predicate predicate
   * @return size_type number of elements erased
   */
  constexpr friend auto erase_if(dictionary& self, std::predicate<const_reference> auto&& predicate) -> size_type
      requires(detail::containers::erasable<key_container>&& detail::containers::erasable<value_container>&&
                   detail::mutable_range<index_container>) { return self.erase_if(predicate); }

  /**
   * @brief Accessor for the hash table
   *
   * @return hash_table const&
   */
  [[nodiscard]] constexpr auto table() const noexcept -> hash_table const& {
    return base::table();
  }

  [[nodiscard]] constexpr auto keys() const noexcept -> keys_const_view {
    return keys_const_view(static_cast<base const&>(*this), keys_);
  }
  [[nodiscard]] constexpr auto values() noexcept -> values_view { return values_view(values_); }
  [[nodiscard]] constexpr auto values() const noexcept -> values_const_view { return values_const_view(values_); }

 private:
  key_container keys_;
  value_container values_;
  FLAT_HASH_NO_UNIQUE_ADDRESS detail::maybe_empty<Traits> traits_;

  using mutable_range_type = detail::mutable_dictionary_range<key_container, value_container>;

  [[nodiscard]] constexpr auto mutable_range() noexcept -> mutable_range_type {
    return mutable_range_type(keys_, values_);
  }

  template <std::ranges::input_range R>
    requires((std::ranges::borrowed_range<R> &&
              detail::dict_pair_assignable<std::ranges::range_reference_t<R>, dictionary>) ||
             (!std::ranges::borrowed_range<R> &&
              detail::dict_pair_assignable<std::ranges::range_value_t<R>&&, dictionary>))
  constexpr void assign(R&& values) {
    auto range = mutable_range();
    if constexpr (detail::containers::resizable<key_container> && detail::containers::resizable<value_container>) {
      range.resize(std::ranges::size(values));
    } else {
      FLAT_HASH_ASSERT(std::ranges::size(keys_) == std::ranges::size(values_),
                       "Different sized keys and values: {} and {}", std::ranges::size(keys_),
                       std::ranges::size(values_));
    }
    [[maybe_unused]] auto n = std::ranges::size(keys_);

    index_type added = 0;
    auto const first = range.begin();
    auto out = first;
    bool overfull = false;
    for (auto&& pair : values) {
      std::ranges::subrange keys(first.key_iter(), out.key_iter());
      base::template try_insert_at<ordering>(
          std::get<0>(pair), keys, out.key_iter(), [&out, &pair, &added, &overfull, &values, n](index_type) {
            if constexpr (!detail::containers::resizable<key_container>) {
              // check that we are still within the container size
              if (added == n) [[unlikely]] {  // LCOV_EXCL_LINE
                FLAT_HASH_ASSERT(false, "Tried to add more unique values than the container could hold ({:d}): {}", n,
                                 detail::maybe_format_arg(values));
                overfull = true;
                return false;  // don't add
              }
            }

            auto out_ref = *out;
            if constexpr (std::ranges::borrowed_range<R>) {
              out_ref = pair;
            } else {
              out_ref = std::move(pair);
            }

            ++out;
            ++added;
            return true;
          });

      if (overfull) [[unlikely]] { break; }  // LCOV_EXCL_LINE from assertion
    }

    if constexpr (detail::containers::resizable<key_container> && detail::containers::resizable<value_container>) {
      range.resize(added);
    } else {
      FLAT_HASH_ASSERT(added == n, "Not enough unique values ({:d}), expected {:d}: {}", added, n,
                       detail::maybe_format_arg(values));
    }
  }

  template <detail::dict_pair_insertible<dictionary> K>
  constexpr auto try_insert_impl(const_iterator pos, K&& value) -> std::pair<iterator, bool> {
    auto offset = pos - cbegin();
    FLAT_HASH_ASSERT(offset <= ssize(), "Iterator past the end: {} <= {}", offset, ssize());

    auto [index, inserted] = base::template try_insert_at<ordering>(
        std::get<0>(value), keys_, pos.key_iter(), [this, &value, offset](index_type) {
          auto out = mutable_range();
          auto iter = out.begin() + offset;
          detail::containers::policy_insert<ordering>(out, iter, std::forward<K>(value));
        });

    return {begin() + static_cast<difference_type>(index), inserted};
  }

  template <detail::dict_pair_insertible<dictionary> K>
  constexpr auto insert_impl(const_iterator pos, K&& value) -> iterator {
    auto [iter, added] = try_insert_impl(pos, std::forward<K>(value));

    if (!added) { traits_.on_duplicate_key(iter.key()); }
    return iter;
  }

  template <detail::hashed_lookup_key<dictionary> K, class M>
    requires std::constructible_from<mapped_type, M&&> &&
             std::is_assignable_v<mapped_type&, M&&>
             constexpr auto insert_or_assign_impl(const_iterator pos, K&& key, M&& obj) -> std::pair<iterator, bool> {
    auto [iter, inserted] =
        try_insert_impl(cend(), detail::forward_as_pair(std::forward<K>(key), std::forward<M>(obj)));
    if (!inserted) { iter.value() = std::forward<M>(obj); }
    return {iter, inserted};
  }

  template <class K>
  constexpr auto extract_impl(K const& key) -> std::optional<mutable_value_type> {
    std::optional<mutable_value_type> value = std::nullopt;
    base::template try_erase<ordering>(keys_, key, [this, &value](index_type index) {
      auto out = mutable_range();
      auto pos = out.begin() + static_cast<difference_type>(index);
      value = detail::containers::extract<ordering>(out, pos);
    });
    return value;
  }

  constexpr auto extract_impl(const_iterator pos) -> mutable_value_type {
    std::optional<mutable_value_type> value = std::nullopt;
    base::template try_erase<ordering>(keys_, pos.key_iter(), [this, &value](index_type index) {
      auto out = mutable_range();
      auto iter = out.begin() + static_cast<difference_type>(index);
      value = detail::containers::extract<ordering>(out, iter);
    });
    return *value;
  }

  template <std::ranges::input_range R>
    requires(detail::dict_pair_insertible<std::ranges::range_reference_t<R>, dictionary>)
  constexpr void merge_unique(R&& source) {
    auto const old_size = size();

    auto&& values = [&source]() -> decltype(auto) {
      if constexpr (is_dictionary<R>) {
        // cannot consume keys otherwise
        return source.mutable_range();
      } else {
        return source;
      }
    }();

    [[maybe_unused]] auto const old_ssize = static_cast<difference_type>(old_size);
    auto out = mutable_range();

    FLAT_HASH_TRY {
      for (auto&& v : values) {
        // guaranteed to contain unique values, only add values that are not already in this set
        if (!contains(std::get<0>(v))) {
          if constexpr (std::ranges::borrowed_range<R>) {
            detail::containers::emplace_back(out, v);
          } else {
            detail::containers::emplace_back(out, std::move(v));
          }
        }
      }

      base::ensure_load_factor(keys_, 0, true);
    }
    FLAT_HASH_CATCH(...) {
      // erasing from the end is much less likely to throw if it does
      detail::containers::erase_after(out, out.begin() + old_ssize);
      FLAT_HASH_THROW();
    }

    // no need to rehash previous keys as values were simply placed into key container
    auto const s = static_cast<index_type>(size());
    for (auto i = static_cast<index_type>(old_size); i < s; ++i) {
      base::insert_new_key(base::hash(detail::containers::at(keys_, i)), i);
    }
  }

  template <std::ranges::input_range R>
    requires(detail::dict_pair_insertible<std::ranges::range_reference_t<R>, dictionary>)
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
      if constexpr (detail::reservable_dict<dictionary>) {
        reserve(size() + std::ranges::size(range));
      } else {
        // don't need to be strict about hash table size if the range can have duplicates
        base::ensure_load_factor(keys_, /* extra = */ std::ranges::size(range), /* strict = */ unique_range<R>);
      }
    }
  }

  constexpr auto erase_if(std::predicate<std::pair<key_type&, mapped_type&>> auto&& predicate,
                          bool update_hashes = true) -> size_type
    requires detail::containers::erasable<key_container> && detail::containers::erasable<value_container> &&
             detail::mutable_range<index_container>
  {
    size_type erased = detail::containers::policy_erase_if<ordering>(mutable_range(), predicate);
    if (update_hashes && erased != 0) { rehash(0); }
    return erased;
  }

  template <class Dict, detail::hashed_lookup_key<std::remove_cvref_t<Dict>> K>
  [[nodiscard]] constexpr static auto at_impl(Dict& dict, K const& key) -> std::ranges::iterator_t<Dict&> {
    auto iter = dict.find(key);
    if (iter == dict.end()) [[unlikely]] {  // LCOV_EXCL_LINE
      FLAT_HASH_THROW(std::out_of_range(FLAT_HASH_FORMAT_NS format("Key error: {}", detail::maybe_format_arg(key))));
    }
    return iter;
  }

  template <class K>
  [[nodiscard]] constexpr auto get_or_init(K&& key) -> mapped_type& {
    using missing_key_type_r = decltype(traits().on_missing_key(std::declval<key_type const&>()));
    // deferred constructor is not movable so use a reference instead
    auto lazy_mapped_value = detail::containers::make_constructor<mapped_type>([this]() {
      // keys are inserted first so the last one corresponds to the value that is to be
      // inserted at the end
      key_type const& k = detail::containers::back(keys_);
      if constexpr (std::convertible_to<missing_key_type_r, mapped_type>) {
        return traits().on_missing_key(k);
      } else if constexpr (std::constructible_from<mapped_type, missing_key_type_r>) {
        return detail::containers::construct<mapped_type>(traits().on_missing_key(k));
      } else {
        (void)traits().on_missing_key(k);
        return detail::containers::construct<mapped_type>();
      }
    });
    return try_insert_impl(cend(), detail::forward_as_pair(std::forward<K>(key), lazy_mapped_value)).first->second;
  }
};

FLAT_HASH_NAMESPACE_END

template <class Key, class Value, class Traits, class Char>
  requires(flat_hash::detail::formattable<Key, Char> && flat_hash::detail::formattable<Value, Char>)
struct FLAT_HASH_FORMAT_NS formatter<flat_hash::dictionary<Key, Value, Traits>, Char> {
  constexpr static inline Char default_separator[] = {':', ' ', '\0'};

  FLAT_HASH_FORMAT_NS formatter<Key, Char> key_formatter{};
  FLAT_HASH_FORMAT_NS formatter<Value, Char> value_formatter{};
  flat_hash::detail::range_format_options options{};
  std::basic_string_view<Char> separator = default_separator;

  enum struct parse_step {
    initial,
    separator,
    end,
  };

  // fmt basic_format_parse_context also has ErrorHandler template argument
  template <class... Args>
  constexpr auto parse(FLAT_HASH_FORMAT_NS basic_format_parse_context<Args...>& ctx) {
    auto [p, opts] = flat_hash::detail::parse_range_format(ctx);
    options = opts;
    auto const end = ctx.end();
    ctx.advance_to(p);
    if (p == end || *p == '}') [[unlikely]] { return p; }  // already at the end

    auto text_begin = p;
    parse_step step = parse_step::initial;
    while (p != end) {  // LCOV_EXCL_LINE error to reach p == end
      auto const c = *p++;
      if (c == '{') {
        if (step == parse_step::separator && text_begin != p - 1) {
          // keys were parsed, text after it is the separator if it is not empty
          separator = std::basic_string_view<Char>(text_begin, p - 1);
        }

        // inside key/value spec
        if (p == end) [[unlikely]] { FLAT_HASH_ON_FORMAT_ERROR(ctx, "unmatched '}' in format string"); }
        if (*p == ':') {
          // start of subspec
          ++p;
        } else if (*p != '}') [[unlikely]] {  // LCOV_EXCL_LINE error check
          // probably missing : in the subspec, don't allow anything else to match non-default format specs
          // no arg_id so a custom spec can only start with a ':'
          FLAT_HASH_ON_FORMAT_ERROR(ctx, "missing ':' in format string");
        }

        // parse the subspec, first subspec is always for keys
        ctx.advance_to(p);
        if (step == parse_step::initial) {
          p = key_formatter.parse(ctx);
          step = parse_step::separator;  // separator is next
        } else {
          p = value_formatter.parse(ctx);
          step = parse_step::end;
        }
        if (p == end) [[unlikely]] { FLAT_HASH_ON_FORMAT_ERROR(ctx, "unmatched '}' in format string"); }
        ++p;  // parse returned pointer to '}', skip
        if (step == parse_step::separator) {
          // parsing separator before the spec terminates or there is a value subspec
          text_begin = p;
        }
      } else if (c == '}') {
        // end of format spec
        --p;
        if (step != parse_step::end && text_begin != p) {
          // no value subspec
          separator = std::basic_string_view<Char>(text_begin, p);
        }
        break;
      } else if (step == parse_step::end) [[unlikely]] {  // LCOV_EXCL_LINE error check
        // now just waiting for the end of the format spec
        FLAT_HASH_ON_FORMAT_ERROR(ctx, "invalid format string past the last subspec");
      }
    }

    return p;
  }

  template <class OutputIt>
  auto format(flat_hash::dictionary<Key, Value, Traits> const& dict,
              FLAT_HASH_FORMAT_NS basic_format_context<OutputIt, Char>& context) const -> OutputIt {
    auto strings = flat_hash::detail::get_set_separators(options);
    return flat_hash::detail::format_range(dict, context, strings, [this](auto const& key_value, auto& ctx) {
      auto&& [key, value] = key_value;
      auto out = flat_hash::detail::write_range_item(key, ctx, key_formatter);
      ctx.advance_to(std::ranges::copy(separator, out).out);
      return flat_hash::detail::write_range_item(value, ctx, value_formatter);
    });
  }
};

#if defined(FLAT_HASH_USE_FMTLIB)
#  include <fmt/ranges.h>

// disable ambiguous formatter specializations
template <class Key, class Value, class Traits, class Char>
  requires(flat_hash::detail::formattable<Key, Char> && flat_hash::detail::formattable<Value, Char>)
struct fmt::is_range<flat_hash::dictionary<Key, Value, Traits>, Char> : std::false_type {};
template <class Key, class Value, class Traits>
  requires(flat_hash::detail::formattable<Key, char> && flat_hash::detail::formattable<Value, char>)
struct fmt::detail::is_map<flat_hash::dictionary<Key, Value, Traits>> : std::false_type {};
#endif
