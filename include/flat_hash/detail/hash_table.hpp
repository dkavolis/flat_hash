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

#include <algorithm>
#include <bit>
#include <cstdint>
#include <type_traits>
#include <variant>
#include <vector>

#include "bits.hpp"
#include "config.hpp"
#include "containers.hpp"
#include "format.hpp"
#include "probing_policy.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {

template <index_range Container = std::vector<std::uint32_t>,
          probing::probing_policy<Container> Policy = probing::robin_hood>
class hash_table;

constexpr static auto always_false = [](std::integral auto) noexcept { return false; };
using always_false_t = decltype(always_false);

struct reserved_data {
  std::uint64_t values;
  std::uint8_t bits;
};

template <std::ranges::random_access_range R, probing::probing_iterator Iter>
[[nodiscard]] constexpr auto iterator_at(R& container [[maybe_unused]], Iter const& iterator) noexcept
    -> std::ranges::iterator_t<R> {
  if constexpr (std::integral<decltype(*iterator)>) {
    auto offset = static_cast<std::ranges::range_difference_t<R>>(*iterator);
    FLAT_HASH_ASSERT(offset < std::ranges::ssize(container), "Offset {:d} out of bounds for container of size {:d}",
                     offset, std::ranges::size(container));
    return std::ranges::begin(container) + offset;
  } else {
    return *iterator;
  }
}

template <class T>
concept has_on_clear = requires(T& t) { t.on_clear(); };

/**
 * @brief Reason why the probing terminated
 */
enum struct found {
  insertion,
  predicate,
};

/**
 * @brief A wrapper over a container for use as a hash table
 *
 * @tparam Container A resizable random-access container of unsigned integrals
 */
template <index_range Container, probing::probing_policy<Container> Policy>
class hash_table : public containers::maybe_enable_allocator_type<Container> {
  template <index_range C, probing::probing_policy<C>>
  friend class hash_table;

 public:
  using value_type = std::ranges::range_value_t<Container>;
  using difference_type = std::ranges::range_difference_t<Container>;
  using policy = Policy;
  using probing_iterator = probing::iterator_t<policy, Container>;

  constexpr static bool uses_tombstones = !probing::disable_tombstones_v<Policy>;

  /**
   * @brief Empty bucket value
   */
  constexpr static value_type npos = empty_bucket_v<value_type>;

  /**
   * @brief Value indicating a bucket which had its stored value removed
   */
  constexpr static value_type tombstone = uses_tombstones ? tombstone_v<value_type> : npos;

  /**
   * @brief Default number of buckets
   */
  constexpr static std::uint64_t default_size = 32;

  /**
   * @brief A wrapper over Container::(const_)iterator for easier checking if the bucket holds anything and
   * dereferencing
   */
  class iterator : public iterator_facade<iterator> {
   public:
    using base_iterator = std::ranges::iterator_t<Container const>;
    using reference = value_type;

    constexpr iterator() noexcept(std::is_nothrow_constructible_v<base_iterator>) = default;
    constexpr iterator(base_iterator it, policy const* p) noexcept(std::is_nothrow_move_constructible_v<base_iterator>)
        : it_(std::move(it)), policy_(p) {}
    template <class U>
      requires std::constructible_from<base_iterator, U>
    constexpr iterator(U it, policy const* p) noexcept(noexcept(base_iterator(it))) : it_(it), policy_(p) {}

    [[nodiscard]] constexpr auto is_empty() const noexcept(nothrow_dereference<base_iterator>) -> bool {
      return *it_ == npos;
    }
    [[nodiscard]] constexpr auto is_unused() const noexcept(nothrow_dereference<base_iterator>) -> bool {
      return *it_ >= tombstone;
    }
    [[nodiscard]] constexpr auto holds_value() const noexcept(nothrow_dereference<base_iterator>) -> bool {
      return !is_unused();
    }

    [[nodiscard]] constexpr auto raw() const noexcept(nothrow_dereference<base_iterator>) -> reference { return *it_; }

    [[nodiscard]] constexpr auto dereference() const noexcept(nothrow_dereference<base_iterator>) -> reference {
      FLAT_HASH_ASSERT(policy_ != nullptr, "Cannot decode value with null policy");
      return holds_value() ? policy_->decode(*it_) : *it_;
    }
    constexpr void increment() noexcept(nothrow_increment<base_iterator>) { ++it_; }
    constexpr void decrement() noexcept(nothrow_decrement<base_iterator>) { --it_; }

    template <std::sentinel_for<base_iterator> S>
    [[nodiscard]] constexpr auto equals(S const& other) const noexcept(nothrow_equals<base_iterator, S>) -> bool {
      return it_ == other;
    }
    [[nodiscard]] constexpr auto equals(iterator other) const noexcept(nothrow_equals<base_iterator>) -> bool {
      return it_ == other.it_;
    }

    template <std::sized_sentinel_for<base_iterator> S>
    [[nodiscard]] constexpr auto distance_to(S const& lhs) const noexcept(nothrow_distance_to<base_iterator, S>)
        -> difference_type {
      return lhs - it_;
    }
    [[nodiscard]] constexpr auto distance_to(iterator lhs) const noexcept(nothrow_distance_to<base_iterator>)
        -> difference_type {
      return lhs.it_ - it_;
    }

    constexpr void advance(difference_type n) noexcept(nothrow_advance<base_iterator>) { it_ += n; }

    [[nodiscard]] constexpr operator base_iterator() const
        noexcept(std::is_nothrow_copy_constructible_v<base_iterator>) {
      return it_;
    }

   private:
    base_iterator it_;
    policy const* policy_;
  };

  using const_iterator = iterator;

  /**
   * @brief Default constructor, will avoid allocations unless Container or Policy allocate in default constructors
   */
  constexpr hash_table() noexcept(
      std::is_nothrow_default_constructible_v<Container>&& std::is_nothrow_default_constructible_v<Policy>) {
    std::ranges::fill(indices_, npos);
  };

  /**
   * @brief Construct hash table from an existing container
   *
   * @param container index container, size of non-resizable containers must be a power of two
   */
  constexpr explicit hash_table(Container container,
                                policy p = policy()) noexcept((std::is_nothrow_move_constructible_v<Container> &&
                                                               std::is_nothrow_move_constructible_v<Policy>))
    requires(std::is_move_constructible_v<Container> && std::is_move_constructible_v<Policy>)
  : indices_(std::move(container)), policy_(std::move(p)) {
    // make sure the size is a power of two
    resize_at_least(bucket_count());
  }

  /**
   * @brief Construct hash table with at least a number of buckets
   *
   * @param buckets at least the number of buckets to use
   */
  constexpr explicit hash_table(std::uint64_t buckets, policy p = policy(),
                                optional_allocator<Container> const& allocator = optional_allocator<Container>())
    requires(containers::sized_constructible<Container> && std::is_move_constructible_v<Policy>)
  : indices_(containers::make_container<Container>(std::bit_ceil(buckets), npos, allocator)), policy_(std::move(p)) {}

  /**
   * @brief Construct hash table, bucket_count will be ignored if the underlying container cannot be constructed from a
   * size
   *
   */
  constexpr explicit hash_table(std::uint64_t buckets, policy p = policy(),
                                optional_allocator<Container> const& allocator = optional_allocator<Container>())
    requires(!containers::sized_constructible<Container> && std::is_move_constructible_v<Policy>)
  : indices_(containers::make_container<Container>(buckets, npos, allocator)), policy_(std::move(p)) {
    if constexpr (!static_sized<Container>) {
      // power of 2 enforced by concepts if the container is statically sized
      FLAT_HASH_ASSERT(std::has_single_bit(bucket_count()),
                       "Expected the bucket count to be a power of two, instead got {:d}", bucket_count());
    }
  }

  template <class C>
    requires(std::same_as<std::ranges::range_value_t<C>, value_type> && std::constructible_from<Container, C>)
  constexpr explicit(!std::convertible_to<C, Container>)
      hash_table(hash_table<C, Policy>& other) noexcept((std::is_nothrow_constructible_v<Container, C&> &&
                                                         std::is_nothrow_copy_constructible_v<Policy>))
      : indices_(other.indices_), policy_(other.policy_) {}
  template <class C>
    requires(std::same_as<std::ranges::range_value_t<C>, value_type> && std::constructible_from<Container, C const>)
  constexpr explicit(!std::convertible_to<C const, Container>)
      hash_table(hash_table<C, Policy> const& other) noexcept((std::is_nothrow_constructible_v<Container, C const&> &&
                                                               std::is_nothrow_copy_constructible_v<Policy>))
      : indices_(other.indices_), policy_(other.policy_) {}

  [[nodiscard]] constexpr auto cbegin() const noexcept -> const_iterator {
    return const_iterator(std::ranges::cbegin(indices_), &probing_policy());
  }
  [[nodiscard]] constexpr auto cend() const noexcept -> const_iterator {
    return const_iterator(std::ranges::cend(indices_), &probing_policy());
  }

  [[nodiscard]] constexpr auto begin() const noexcept -> const_iterator { return cbegin(); }
  [[nodiscard]] constexpr auto end() const noexcept -> const_iterator { return cend(); }

  /**
   * @brief Resize the underlying container to at least new_size number of buckets. Asserts that the container size is
   * valid if it is not resizable and clears all buckets
   *
   * @param new_size number of buckets
   * @return true if resized
   * @return false otherwise
   */
  constexpr auto resize_at_least(std::uint64_t new_size) -> bool {
    std::uint64_t s = std::bit_ceil(new_size);
    auto old_size = bucket_count();
    if constexpr (containers::resizable<Container>) {
      static_assert(mutable_range<Container>, "Resizable containers must also satisfy output_range requirements");
      if (s == old_size) return false;

      // if resizing from empty use default size to start with
      if (old_size == 0) { s = std::max(default_size, s); }
      containers::resize(indices_, s, npos);

      // do clearing after resizing since it should not throw, ensures strong exception guarantee
      std::ranges::fill_n(std::ranges::begin(indices_), static_cast<difference_type>(std::min(old_size, s)), npos);
      if constexpr (has_on_clear<policy>) { policy_.get().on_clear(); }
      return true;
    } else {
      // cannot resize, just make sure the size is valid
      FLAT_HASH_ASSERT(old_size == std::bit_ceil(old_size), "Size must be a power of two but is {:d}", old_size);
      FLAT_HASH_ASSERT(old_size >= s, "Container is not resizable");

      return false;
    }
  }

  /**
   * @brief Get the number of reserved values and bits
   */
  [[nodiscard]] constexpr auto reserved() const noexcept -> reserved_data {
    return {
        .values = npos - tombstone + 1,
        .bits = policy_.get().reserved_bits(),
    };
  }

  /**
   * @brief Number of buckets in the container
   *
   * @return std::uint64_t
   */
  [[nodiscard]] constexpr auto bucket_count() const noexcept -> std::uint64_t { return std::ranges::size(indices_); }

  // Mutable operations require at least an output ranges

  /**
   * @brief Clear out all buckets, requires a mutable container
   *
   */
  constexpr void clear()
    requires mutable_range<Container>
  {
    std::ranges::fill(indices_, npos);
    if constexpr (has_on_clear<policy>) { policy_.get().on_clear(); }
  }

  /**
   * @brief Overwrite value at pos with value
   *
   * @param pos iterator to bucket to overwrite
   * @param v new value
   */
  constexpr void overwrite(const_iterator pos, value_type v) noexcept {
    FLAT_HASH_ASSERT(pos - cbegin() < std::ranges::ssize(indices_), "Iterator past the end");
    auto it = std::ranges::begin(indices_) + (pos - cbegin());
    *it = policy_.get().reencode(pos.raw(), v);
  }

  /**
   * @brief Swap bucket values
   *
   * @param lhs
   * @param rhs
   */
  constexpr void swap_buckets(const_iterator lhs, const_iterator rhs) noexcept {
    value_type left = *lhs;
    overwrite(lhs, *rhs);
    overwrite(rhs, left);
  }

  /**
   * @brief Find the bucket satisfying a predicate
   *
   * @param hash hash value
   * @param predicate predicate
   * @return iterator to bucket satisfying predicate or hash_table::end() otherwise
   */
  [[nodiscard]] constexpr auto find(std::uint64_t hash, std::predicate<value_type> auto const& predicate) const
      -> const_iterator {
    auto [probe_it, return_reason] = find_if(hash, predicate);

    if (return_reason != found::predicate) { return cend(); }

    std::ranges::iterator_t<Container const> it;
    if constexpr (std::unsigned_integral<decltype(*probe_it)>) {
      it = std::ranges::begin(indices_) + static_cast<difference_type>(*probe_it);
    } else {
      it = *probe_it;
    }

    return const_iterator(it, &probing_policy());
  }

  template <std::predicate<value_type> TPredicate>
  constexpr auto find_insertion_bucket(std::uint64_t hash, TPredicate&& predicate) const
      -> std::pair<const_iterator, std::optional<probing_iterator>> {
    auto [probe_it, return_reason] = find_if(hash, predicate);

    auto it = cbegin() + static_cast<difference_type>(*probe_it);
    if constexpr (!std::same_as<always_false_t, std::remove_cvref_t<TPredicate>>) {
      if (return_reason == found::predicate) { return {it, std::nullopt}; }
    }

    return {it, probe_it};
  }

  constexpr void insert(const_iterator pos, probing_iterator const& state, value_type value) noexcept {
    auto offset = pos - cbegin();
    auto it = std::ranges::begin(indices_) + offset;
    policy_.get().pre_insert(indices_, state);
    *it = policy_.get().encode(value, state);
  }

  constexpr void clear_bucket(const_iterator pos) noexcept
    requires mutable_range<Container>
  {
    std::ranges::iterator_t<Container> it = std::ranges::begin(indices_) + (pos - cbegin());
    *it = tombstone;
    policy_.get().post_erase(indices_, it);
  }

  /**
   * @brief Get the allocator object, requires allocator aware container
   *
   * @return Container::allocator_type
   */
  [[nodiscard]] constexpr auto get_allocator() const noexcept(containers::nothrow_gettable_allocator<Container>)
      -> containers::allocator_t<Container>
    requires(containers::gettable_allocator<Container>)
  {
    return containers::get_allocator(indices_);
  }

  /**
   * @brief Mutate all filled buckets satisfying the predicate
   *
   * @param predicate predicate for mutation
   * @param fn mutation function
   */
  constexpr void mutate_if(std::predicate<value_type> auto&& predicate,
                           invocable_r<value_type, value_type> auto&& fn) noexcept
    requires mutable_range<Container>
  {
    for (value_type& payload : indices_) {
      if (payload >= tombstone) continue;
      value_type index = policy_.get().decode(payload);
      // TODO: benchmark with/without predicate
      if (predicate(index)) { payload = policy_.get().reencode(payload, fn(index)); }
    }
  }

  /**
   * @brief Accessor to probing policy
   */
  constexpr auto probing_policy() noexcept -> policy& { return policy_.get(); }
  [[nodiscard]] constexpr auto probing_policy() const noexcept -> policy const& { return policy_.get(); }

 private:
  Container indices_;
  FLAT_HASH_NO_UNIQUE_ADDRESS maybe_empty<policy> policy_;

  [[nodiscard]] constexpr auto find_if(std::uint64_t hash, always_false_t /* unused */) const noexcept
      -> std::pair<probing_iterator, found> {
    auto probe = policy_.get().probe(indices_, hash);

    while (probe != probing::lookup_end{}) {
      value_type payload = *iterator_at(indices_, probe);
      // predicate always returns false so there's no need to look for any values matching it
      if (payload >= tombstone) { break; }

      ++probe;
    }

    return {probe, found::insertion};
  }

  template <std::predicate<value_type> Predicate>
  [[nodiscard]] constexpr auto find_if(std::uint64_t hash, Predicate& predicate) const
      -> std::pair<probing_iterator, found> {
    // keep track of the insertion bucket in case there is no valid predicate
    std::optional<probing_iterator> insert_it = std::nullopt;

    auto probe = policy_.get().probe(indices_, hash);

    while (probe != probing::lookup_end{}) {
      value_type payload = *iterator_at(indices_, probe);

      if (payload == npos) {
        // empty bucket, no need to continue anymore
        break;
      }

      if constexpr (uses_tombstones) {
        // cache the first empty bucket for insertion
        if (payload == tombstone) [[unlikely]] {
          if (!insert_it) { insert_it = probe; }

          // bucket does not contain any payload
          ++probe;
          continue;
        }
      }

      // check if predicate is satisfied
      value_type index = policy_.get().decode(payload);
      if (predicate(index)) { return {probe, found::predicate}; }

      if constexpr (!uses_tombstones) {
        // cache the end of probing range for insertion
        if (probe == probing::insert_end{} && !insert_it) [[unlikely]] { insert_it = probe; }
      }

      ++probe;
    }

    // did not find what we were looking for
    // return the insert iterator if it's set, or the current one
    return {insert_it.value_or(probe), found::insertion};
  }

  friend constexpr void swap(hash_table& lhs,
                             hash_table& rhs) noexcept(nothrow_swappable<Container>&& nothrow_swappable<policy>)
    requires(std::is_swappable_v<Container> && std::is_swappable_v<policy>)
  {
    std::ranges::swap(lhs.indices_, rhs.indices_);
    std::ranges::swap(lhs.policy_, rhs.policy_);
  }
};

}  // namespace detail

FLAT_HASH_NAMESPACE_END

template <class Container, class Policy, class Char>
  requires(flat_hash::detail::formattable<std::ranges::range_value_t<Container>>)
struct FLAT_HASH_FORMAT_NS formatter<flat_hash::detail::hash_table<Container, Policy>, Char> {
  using subformatter_t = formatter<std::ranges::range_value_t<Container>, Char>;
  using empty_formatter_t = FLAT_HASH_FORMAT_NS formatter<int, Char>;
  using table_type = flat_hash::detail::hash_table<Container, Policy>;
  constexpr static bool assume_identical_formatters = std::is_fundamental_v<std::ranges::range_value_t<Container>> &&
                                                      sizeof(subformatter_t) == sizeof(empty_formatter_t);

  subformatter_t subformatter{};
  empty_formatter_t empty_formatter{};
  flat_hash::detail::range_format_options options{};

  template <class... Args>
  constexpr auto parse(FLAT_HASH_FORMAT_NS basic_format_parse_context<Args...>& ctx) {
    auto [it, opts] = flat_hash::detail::parse_range_format(ctx);
    options = opts;

    ctx.advance_to(it);
    auto out = subformatter.parse(ctx);

    // rather than parsing twice just make a bit copy of the formatter, fmt has unions in the formatter which doesn't
    // work at compile time
    // both fmt and std parse format strings for all integrals identically
    if constexpr (assume_identical_formatters) {
      if (!std::is_constant_evaluated()) {
        // only a trivial copy and it doesn't invoke UB as reinterpret_cast would
        empty_formatter = std::bit_cast<empty_formatter_t>(subformatter);
      }
    } else {
      // cannot be guaranteed that the two formatters are fundamentally the same
      ctx.advance_to(it);
      empty_formatter.parse(ctx);
    }
    return out;
  }

  template <class OutputIt>
  auto format(table_type const& table, FLAT_HASH_FORMAT_NS basic_format_context<OutputIt, Char>& context) -> OutputIt {
    auto strings = flat_hash::detail::get_list_separators(options);
    return flat_hash::detail::format_range(table, context, strings, [this](auto&& value, auto& ctx) -> OutputIt {
      if (value == table_type::npos) { return empty_formatter.format(-1, ctx); }
      if constexpr (table_type::uses_tombstones) {
        if (value == table_type::tombstone) [[unlikely]] { return empty_formatter.format(-2, ctx); }
      }
      return subformatter.format(value, ctx);
    });
  }
};

#if defined(FLAT_HASH_USE_FMTLIB)
#  include <fmt/ranges.h>

// set is also a range which makes formatter specialization ambiguous, disable range check since we provide a
// formatter already
template <class Container, class Policy, class Char>
  requires(flat_hash::detail::formattable<std::ranges::range_value_t<Container>>)
struct fmt::is_range<flat_hash::detail::hash_table<Container, Policy>, Char> : std::false_type {};
#endif
