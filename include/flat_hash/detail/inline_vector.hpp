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

#include "config.hpp"
#include "containers/utility.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {
// small vector with bare minimum API for adding/removing elements from a static storage, remaining API is from a
// generic implementation in containers
template <mutable_ T, std::size_t N>
class inline_vector {
 private:
  template <mutable_, std::size_t>
  friend class inline_vector;

 public:
  using iterator = T*;
  using const_iterator = T const*;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;

  constexpr inline_vector() noexcept = default;
  constexpr explicit inline_vector(size_type n) noexcept(std::is_nothrow_constructible_v<T>) { resize(n); }
  constexpr inline_vector(size_type n, T const& value) noexcept(std::is_nothrow_copy_constructible_v<T>) {
    resize(n, value);
  }
  template <std::forward_iterator It, std::sentinel_for<It> S>
    requires(std::constructible_from<T, std::iter_reference_t<It>>)
  constexpr inline_vector(It first, S last) noexcept(std::is_nothrow_constructible_v<T, std::iter_reference_t<It>>) {
    while (first != last) {
      auto&& value = *first;
      emplace_back(std::forward<decltype(value)>(value));
      ++first;
    }
  }
  constexpr inline_vector(std::initializer_list<T> init) noexcept(std::is_nothrow_copy_constructible_v<T>) {
    for (auto& v : init) { emplace_back(v); }
  }

  constexpr inline_vector(inline_vector const& other) noexcept(std::is_nothrow_copy_constructible_v<T>) { copy(other); }

  constexpr inline_vector(inline_vector&& other) noexcept(std::is_nothrow_move_constructible_v<T>) {
    move(std::move(other));
  }

  template <std::size_t S>
  constexpr inline_vector(inline_vector<T, S> const& other) noexcept(std::is_nothrow_copy_constructible_v<T>) {
    copy(other);
  }

  template <std::size_t S>
  constexpr inline_vector(inline_vector<T, S>&& other) noexcept(std::is_nothrow_move_constructible_v<T>) {
    move(std::move(other));
  }

  constexpr auto operator=(inline_vector const& other) noexcept(std::is_nothrow_copy_constructible_v<T>)
      -> inline_vector& {
    return copy(other);
  }

  constexpr auto operator=(inline_vector&& other) noexcept(std::is_nothrow_move_constructible_v<T>) -> inline_vector& {
    return move(std::move(other));
  }

  template <std::size_t S>
  constexpr auto operator=(inline_vector<T, S> const& other) noexcept(std::is_nothrow_copy_constructible_v<T>)
      -> inline_vector& {
    return copy(other);
  }

  template <std::size_t S>
  constexpr auto operator=(inline_vector<T, S>&& other) noexcept(std::is_nothrow_move_constructible_v<T>)
      -> inline_vector& {
    return move(std::move(other));
  }

  constexpr ~inline_vector() noexcept(std::is_nothrow_destructible_v<T>)
    requires(!std::is_trivially_destructible_v<T>)
  {
    clear();
  }

  constexpr ~inline_vector() noexcept
    requires(std::is_trivially_destructible_v<T>)
  = default;

  template <class... Args>
    requires(containers::constructible_from<T, Args...>)
  constexpr auto emplace_back(Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args...>) -> T& {
    FLAT_HASH_ASSERT(count_ < N, "Storage is already full");

    return containers::construct_at(elem_ptr(count_++), std::forward<Args>(args)...);
  }

  constexpr void pop_back() noexcept(std::is_nothrow_destructible_v<T>) {
    containers::destroy(*std::launder(elem_ptr(--count_)));
  }

  constexpr void resize(size_type n) noexcept(
      std::is_nothrow_default_constructible_v<T>&& std::is_nothrow_destructible_v<T>)
    requires(std::is_default_constructible_v<T>)
  {
    FLAT_HASH_ASSERT(n < N, "Cannot resize to {:d} or more, got {:d}", N, n);
    while (count_ < n) { emplace_back(); }
    while (count_ > n) { pop_back(); }
  }
  constexpr void resize(size_type n, T const& value) noexcept(
      std::is_nothrow_copy_constructible_v<T>&& std::is_nothrow_destructible_v<T>)
    requires(std::is_copy_constructible_v<T>)
  {
    FLAT_HASH_ASSERT(n < N, "Cannot resize to {:d} or more, got {:d}", N, n);
    while (count_ < n) { emplace_back(value); }
    while (count_ > n) { pop_back(); }
  }
  [[nodiscard]] constexpr auto size() const noexcept -> size_type { return count_; }
  [[nodiscard]] constexpr auto ssize() const noexcept -> difference_type {
    return static_cast<difference_type>(count_);
  }
  [[nodiscard]] constexpr auto capacity() const noexcept -> size_type { return N; }

  constexpr auto begin() noexcept -> iterator { return data(); }
  [[nodiscard]] constexpr auto begin() const noexcept -> const_iterator { return data(); }
  constexpr auto end() noexcept -> iterator { return begin() + ssize(); }
  [[nodiscard]] constexpr auto end() const noexcept -> const_iterator { return begin() + ssize(); }

  [[nodiscard]] constexpr auto data() noexcept -> T* { return std::launder(elem_ptr(0)); }
  [[nodiscard]] constexpr auto data() const noexcept -> T const* { return std::launder(elem_ptr(0)); }

  constexpr void append_from_capacity(difference_type n) noexcept {
    auto new_size = n + static_cast<difference_type>(count_);
    FLAT_HASH_ASSERT(new_size >= 0 && new_size <= static_cast<difference_type>(N),
                     "Invalid use of capacity - size is {:d}", new_size);
    count_ = static_cast<size_type>(new_size);
  }

  constexpr void clear() noexcept(std::is_nothrow_destructible_v<T>) {
    containers::destroy_range(*this);
    count_ = 0;
  }

 private:
  // std::aligned_storage_t is deprecated in C++23
  alignas(alignof(T)) std::byte storage_[sizeof(T) * N];
  size_type count_ = 0;

  [[nodiscard]] constexpr auto elem_ptr(size_type index) noexcept -> T* {
    FLAT_HASH_ASSUME(index <= N);
    return reinterpret_cast<T*>(&storage_[0]) + static_cast<difference_type>(index);
  }
  [[nodiscard]] constexpr auto elem_ptr(size_type index) const noexcept -> T const* {
    FLAT_HASH_ASSUME(index <= N);
    return reinterpret_cast<T const*>(&storage_[0]) + static_cast<difference_type>(index);
  }

  template <std::size_t S = N>
  constexpr auto copy(inline_vector<T, S> const& other) noexcept(std::is_nothrow_copy_constructible_v<T>)
      -> inline_vector& {
    if constexpr (S == N) {
      if (this == std::addressof(other)) { return *this; }
    }
    clear();
    count_ = std::min(N, other.size());
    std::ranges::uninitialized_copy(other, *this);
    return *this;
  }

  template <std::size_t S = N>
  constexpr auto move(inline_vector<T, S>&& other) noexcept(std::is_nothrow_move_constructible_v<T>) -> inline_vector& {
    if constexpr (S == N) {
      if (this == std::addressof(other)) { return *this; }
    }
    clear();
    count_ = std::min(N, other.size());
    auto [in, out] = std::ranges::uninitialized_move(other, *this);
    containers::destroy_range(std::ranges::subrange(in, other.end()));
    other.count_ = 0;

    return *this;
  }
};
}  // namespace detail

FLAT_HASH_NAMESPACE_END
