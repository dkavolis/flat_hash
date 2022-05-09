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

#include <bit>
#include <concepts>
#include <ranges>

#include "bits.hpp"
#include "config.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace probing {

/**
 * @brief Sentinel representing probing iterator equal to insert position
 */
struct insert_end {};

/**
 * @brief Sentinel representing probing iterator equal to lookup end, empty buckets are handled independently
 */
struct lookup_end {};

template <class T>
concept probing_end_type =
    std::same_as<insert_end, std::remove_cvref_t<T>> || std::same_as<lookup_end, std::remove_cvref_t<T>>;

template <class T>
concept probing_iterator = std::input_iterator<T> && requires(T it) {
  { *it } -> std::integral;
} && detail::weakly_equality_comparable_with<T, insert_end> && detail::weakly_equality_comparable_with<T, lookup_end>;

template <class T, class R>
concept prober_for = std::ranges::random_access_range<R> &&
    requires(T const& policy, R const& range, std::uint64_t hash) {
  { policy.probe(range, hash) } -> probing_iterator;
};

template <class T, std::ranges::random_access_range R>
  requires prober_for<T, R>
using iterator_t = decltype(std::declval<T const&>().probe(std::declval<R&>(), 0));

/**
 * @brief Empty probing info struct that templated probing iterators can convert to if encoding and pre_insert do not
 * require any state information
 */
struct empty_probing_info {};

template <class T, class R>
concept probing_info_decayable = prober_for<T, R> && requires {
  typename T::probing_info;
  requires std::convertible_to < iterator_t<T, R>,
  typename T::probing_info > ;
};

template <class T, class R>
struct probing_info_type;

template <class T, class R>
  requires prober_for<T, R>
struct probing_info_type<T, R> {
  using type = iterator_t<T, R>;
};
template <class T, class R>
  requires probing_info_decayable<T, R>
struct probing_info_type<T, R> {
  using type = typename T::probing_info;
};

template <class T, class R>
using probing_info_t = probing_info_type<T, R>::type;

template <class T, class R = std::vector<std::uint32_t>>
concept probing_policy = std::semiregular<T> && prober_for<T, R> && requires(T const& policy, std::uint64_t hash) {
  { policy.reserved_bits() }
  noexcept->std::unsigned_integral;
  { policy.max_load_factor() }
  noexcept->std::floating_point;
  requires requires(std::ranges::range_value_t<R> payload) {
    { policy.decode(payload) }
    noexcept->std::same_as<std::ranges::range_value_t<R>>;
  };
  // immutable ranges don't need encode/replace and pre_insert/post_erase
  requires(!detail::mutable_range<R> || requires(std::ranges::range_value_t<R> v, probing_info_t<T, R> info) {
    { policy.encode(v, info) }
    noexcept->std::same_as<std::ranges::range_value_t<R>>;
    { policy.reencode(v, v) }
    noexcept->std::same_as<std::ranges::range_value_t<R>>;
    requires requires(T & pol, R & container, std::ranges::iterator_t<R const> pos) {
      {pol.pre_insert(container, info)};
      {pol.post_erase(container, pos)};
    };
  });
};

/**
 * @brief Disable tombstones if your probing policy takes care of empty slots on removal
 *
 * @tparam T probing policy type
 */
template <class T>
constexpr inline bool disable_tombstones_v = false;

/**
 * @brief Basic probing policy which is only missing probe(...) method, for use with policies that don't encode any
 * additional data into buckets
 */
struct basic_probing_policy {
  [[nodiscard]] static constexpr auto reserved_bits() noexcept -> std::uint8_t { return 0; }
  [[nodiscard]] static constexpr auto max_load_factor() noexcept -> double { return 0.75; }

  template <std::unsigned_integral U>
  [[nodiscard]] constexpr auto decode(U v) const noexcept -> U {
    return v;
  }
  template <std::unsigned_integral U>
  [[nodiscard]] constexpr auto reencode(U /* value */, std::type_identity_t<U> new_value) const noexcept -> U {
    return new_value;
  }
  template <std::unsigned_integral U, class T>
  [[nodiscard]] constexpr auto encode(U v, T /* state */) const noexcept -> U {
    return v;
  }

  template <std::ranges::random_access_range R, class T>
  constexpr void pre_insert(R const& /* r */, T /* state */) const noexcept {
    // nothing to do
  }
  template <std::ranges::random_access_range R>
  constexpr void post_erase(R const& /* r */, std::ranges::iterator_t<R const> const& /* pos */) const noexcept {
    // nothing to do
  }
};

/**
 * @brief Probing policy based on Python, works well even with poor hash functions but also has poor cache locality
 */
struct python : basic_probing_policy {
  constexpr static std::size_t default_perturb_shift = 5;
  using probing_info = empty_probing_info;

  struct iterator : iterator_facade<iterator> {
    std::uint64_t perturb;
    std::uint64_t index;
    std::uint64_t mask;

    constexpr void increment() noexcept {
      // https://github.com/python/cpython/blob/77195cd44b2506cda88a3cfc98918526068b1d46/Objects/dictobject.c#L862-L931
      perturb >>= default_perturb_shift;
      index = (index * 5 + perturb + 1) & mask;
    }
    [[nodiscard]] constexpr auto dereference() const noexcept -> std::uint64_t { return index; }
    [[nodiscard]] static constexpr auto equals(insert_end /*unused*/) noexcept -> bool { return false; }
    [[nodiscard]] static constexpr auto equals(lookup_end /*unused*/) noexcept -> bool { return false; }
    [[nodiscard]] constexpr operator probing_info() const noexcept { return {}; }
  };

  template <std::ranges::random_access_range R>
  [[nodiscard]] constexpr auto probe(R const& r, std::uint64_t hash) const noexcept -> iterator {
    auto size = std::ranges::size(r);
    FLAT_HASH_ASSERT(std::has_single_bit(size), "Expected the container size to be a power of 2, instead got {:d}",
                     size);
    auto mask = size - 1;
    return {
        {},  // facade base
        /* .perturb = */ hash,
        /* .index = */ hash & mask,
        /* .mask = */ mask,
    };
  }

  [[nodiscard]] static constexpr auto max_load_factor() noexcept -> double { return 0.666666666666666666667; }
};

/**
 * @brief Quadratic probing policy, less clumping than linearnoexcept
 */
struct quadratic : basic_probing_policy {
  using probing_info = empty_probing_info;

  struct iterator : iterator_facade<iterator> {
    std::uint64_t i;
    std::uint64_t index;
    std::uint64_t mask;

    constexpr void increment() noexcept {
      index = (index + i * i) & mask;
      ++i;
    }
    [[nodiscard]] constexpr auto dereference() const noexcept -> std::uint64_t { return index; }
    [[nodiscard]] static constexpr auto equals(insert_end /*unused*/) noexcept -> bool { return false; }
    [[nodiscard]] static constexpr auto equals(lookup_end /*unused*/) noexcept -> bool { return false; }
    [[nodiscard]] constexpr operator probing_info() const noexcept { return {}; }
  };

  template <std::ranges::random_access_range R>
  [[nodiscard]] constexpr auto probe(R const& r, std::uint64_t hash) const noexcept -> iterator {
    auto size = std::ranges::size(r);
    FLAT_HASH_ASSERT(std::has_single_bit(size), "Expected the container size to be a power of 2, instead got {:d}",
                     size);
    auto mask = size - 1;
    return {
        {},  // facade base
        /* .i = */ 0,
        /* .index = */ hash & mask,
        /* .mask = */ mask,
    };
  }
};

/**
 * @brief Robin-Hood hashing policy with linear probing and back-shifting
 * https://programming.guide/robin-hood-hashing.html
 */
class robin_hood {
 public:
  constexpr static std::uint8_t default_psl_bits = 4;

  constexpr robin_hood() noexcept = default;
  constexpr explicit robin_hood(std::uint8_t bits) noexcept : psl_bits_(bits) {}

  struct probing_info {
    std::uint64_t index;
    std::uint16_t psl;
  };

  template <std::random_access_iterator It>
  struct iterator : iterator_facade<iterator<It>> {
    using difference_type = std::iter_difference_t<It>;

    It begin{};
    std::iter_value_t<It> psl_mask{};
    std::uint64_t index{};
    std::uint64_t mask{};
    std::uint16_t psl = 0;
    std::uint16_t max_psl = std::numeric_limits<std::uint16_t>::max();

    constexpr void increment() noexcept {
      ++index;
      index &= mask;
      ++psl;
    }
    [[nodiscard]] constexpr auto dereference() const noexcept -> std::uint64_t { return index; }
    [[nodiscard]] constexpr auto equals(insert_end /*unused*/) const noexcept -> bool {
      return psl > static_cast<std::uint16_t>(begin[static_cast<difference_type>(index)] & psl_mask);
    }
    [[nodiscard]] constexpr auto equals(lookup_end /*unused*/) const noexcept -> bool { return psl > max_psl; }
    [[nodiscard]] constexpr operator probing_info() const noexcept {
      return {
          .index = index,
          .psl = psl,
      };
    }
  };

  template <std::ranges::random_access_range R>
  [[nodiscard]] constexpr auto probe(R const& r, std::uint64_t hash) const noexcept
      -> iterator<std::ranges::iterator_t<R const>> {
    auto size = std::ranges::size(r);
    FLAT_HASH_ASSERT(std::has_single_bit(size), "Expected the container size to be a power of 2, instead got {:d}",
                     size);
    auto mask = size - 1;
    return {
        {},  // facade base...
        /* .begin = */ std::ranges::begin(r),
        /* .psl_mask = */ psl_mask<std::ranges::range_value_t<R>>(),
        /* .index = */ hash & mask,
        /* .mask = */ mask,
        /* .psl = */ 0,
        /* .max_psl = */ max_psl_,
    };
  }

  [[nodiscard]] constexpr auto reserved_bits() const noexcept -> std::uint8_t { return psl_bits_; }
  [[nodiscard]] static constexpr auto max_load_factor() noexcept -> double { return 0.75; }

  [[nodiscard]] constexpr auto decode(std::unsigned_integral auto v) const noexcept { return v >> psl_bits_; }

  template <std::unsigned_integral U>
  [[nodiscard]] constexpr auto encode(U v, probing_info info) const noexcept {
    return encode_psl(v, info.psl);
  }

  template <std::integral T>
  [[nodiscard]] constexpr auto reencode(T payload, std::type_identity_t<T> new_value) const noexcept -> T {
    return encode_psl(new_value, decode_psl(payload));
  }

  constexpr void on_clear() noexcept { max_psl_ = 0; }

  template <std::ranges::random_access_range R>
    requires detail::mutable_range<R>
  constexpr void pre_insert(R& r, probing_info info) noexcept {
    constexpr auto empty_value = empty_bucket_v<std::ranges::range_value_t<R>>;
    update_max_psl(info.psl);
    auto new_width = static_cast<std::uint8_t>(std::bit_width(info.psl));

    if (new_width > psl_bits_) [[unlikely]] {
      // number of data bits has changed so have to reencode the entire range
      change_width(r, new_width);
    }

    auto index = static_cast<std::ranges::range_difference_t<R>>(info.index);
    auto begin = std::ranges::begin(r);
    auto it = begin + index;

    // cache the data in the next bucket to do the shifting in a single pass
    auto payload = *it;
    if (payload == empty_value) {
      // bucket is already empty
      return;
    }

    auto next = it;
    auto end = std::ranges::end(r);
    auto next_payload = payload;
    std::uint16_t psl = decode_psl(payload);
    auto max_psl = static_cast<std::uint16_t>(1U << psl_bits_);

    // payload is cached and it is going to be shifted out so can easily clear the bucket
    *it = empty_value;
    while (true) {
      // find the next bucket where this is going to be shifted to
      while (true) {
        ++next;
        ++psl;  // current psl if the value was shifted now

        // wrap around
        if (next == end) [[unlikely]] { next = begin; }

        // can shift into empty bucket
        if (*next == empty_value) { break; }
        // only shift if the next bucket has lower PSL
        if (decode_psl(*next) < psl) { break; }
      }

      next_payload = *next;
      // shift the value from the current bucket to the next and increment PSL
      auto data = decode(payload);
      // ensure that shifted psl always fits
      // LCOV_EXCL_START
      // this would too annoying to try and hit if it's even possible
      if (psl > max_psl) [[unlikely]] { change_width(r, static_cast<std::uint8_t>(std::bit_width(psl))); }
      // LCOV_EXCL_STOP
      update_max_psl(psl);
      *next = encode_psl(data, psl);

      payload = next_payload;

      if (next_payload == empty_value) {
        // nothing to shift
        return;
      }

      psl = decode_psl(payload);
    }

    FLAT_HASH_UNREACHABLE();
  }

  template <std::ranges::random_access_range R>
    requires detail::mutable_range<R>
  constexpr void post_erase(R& r, std::ranges::iterator_t<R const> pos) const noexcept {
    constexpr auto empty_value = empty_bucket_v<std::ranges::range_value_t<R>>;

    auto distance = pos - std::ranges::cbegin(r);

    auto begin = std::ranges::begin(r);
    auto end = std::ranges::end(r);
    auto it = begin + distance;
    auto next = it;

    while (true) {
      ++next;

      // wrap around
      if (next == end) [[unlikely]] { next = begin; }

      auto payload = *next;
      auto psl = decode_psl(payload);

      if (payload == empty_value || psl == 0) {
        // next bucket is already in the correct place, no shifting needed
        // clear the current bucket and we're done
        *it = empty_value;
        return;
      }

      // backshift the data
      auto data = decode(payload);
      *it = encode_psl(data, psl - 1);
      it = next;
    }

    FLAT_HASH_UNREACHABLE();
  }

 private:
  std::uint16_t max_psl_ = 0;
  std::uint8_t psl_bits_ = default_psl_bits;

  template <std::integral T>
  [[nodiscard]] constexpr auto psl_mask() const noexcept -> T {
    return static_cast<T>(1 << psl_bits_) - 1;
  }

  template <std::integral T>
  [[nodiscard]] constexpr auto decode_psl(T value) const noexcept -> std::uint16_t {
    return static_cast<std::uint16_t>(value & psl_mask<T>());
  }

  template <std::integral T>
  [[nodiscard]] constexpr auto encode_psl(T value, std::uint16_t psl) const noexcept -> T {
    return value << psl_bits_ | psl;
  }

  template <std::integral T>
  [[nodiscard]] constexpr auto change_psl_width(T value, std::uint8_t width) const noexcept -> T {
    return ((value >> psl_bits_) << width) | decode_psl(value);
  }

  template <std::ranges::range R>
  constexpr auto change_width(R& range, std::uint8_t new_width) noexcept {
    constexpr auto empty_value = empty_bucket_v<std::ranges::range_value_t<R>>;
    for (auto& bucket_value : range) {
      if (bucket_value != empty_value) { bucket_value = change_psl_width(bucket_value, new_width); }
    }

    psl_bits_ = new_width;
  }

  template <std::integral I>
  constexpr auto update_max_psl(I psl) noexcept {
    max_psl_ = std::max(max_psl_, static_cast<std::uint16_t>(psl));
  }
};

template <>
constexpr inline bool disable_tombstones_v<robin_hood> = true;

}  // namespace probing

FLAT_HASH_NAMESPACE_END
