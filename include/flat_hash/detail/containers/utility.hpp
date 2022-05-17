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

#include <algorithm>  // for std::ranges::for_each
#include <span>
#include <utility>

#include "../config.hpp"
#include "../macros.hpp"
#include "allocators.hpp"
#include "concepts.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {

// https://quuxplusone.github.io/blog/2018/05/17/super-elider-round-2/
template <std::invocable F>
  requires(!std::same_as<void, std::invoke_result_t<F>>)
struct deferred_constructor {
 public:
  using value_type = std::invoke_result_t<F>;

  constexpr explicit deferred_constructor(F function) noexcept(std::is_nothrow_constructible_v<F, F&&>)
      : function_(std::forward<F>(function)) {}

  // disable constructions that accept any type like std::any
  deferred_constructor(deferred_constructor&&) = delete;

  constexpr operator value_type() noexcept(std::is_nothrow_invocable_v<F>) { return function_(); }

 private:
  FLAT_HASH_NO_UNIQUE_ADDRESS F function_;
};

template <class T>
struct is_std_array : std::false_type {};
template <class T, std::size_t N>
struct is_std_array<std::array<T, N>> : std::true_type {};

// MSVC returns true for std::array for whatever reason
template <class T, class... Args>
concept std_constructible_from = (is_std_array<T>::value && brace_constructible<T, Args...>) ||
                                 (!is_std_array<T>::value && std::constructible_from<T, Args...>);

static_assert(!std_constructible_from<std::array<int, 5>, std::uint32_t, int>);
// static_assert(!std::constructible_from<std::array<int, 5>, std::uint32_t, int>); // fails on MSVC
static_assert(!brace_constructible<std::array<int, 5>, std::uint32_t, int>);
static_assert(brace_constructible<std::array<int, 5>, int, int, int, int, int>);

template <class T>
struct _construct_fn {
  template <class... Args>
    requires(brace_constructible<T, Args...> || std_constructible_from<T, Args...>)
  [[nodiscard]] constexpr auto operator()(Args&&... args) const
      noexcept(nothrow_brace_constructible<T, Args...> or std::is_nothrow_constructible_v<T, Args...>) -> T {
    if constexpr (std_constructible_from<T, Args...>) {
      return T(std::forward<Args>(args)...);
    } else {
      return T{std::forward<Args>(args)...};
    }
  }
};

template <class T, class... Args>
concept constructible_from = requires(Args... args) {
                               { _construct_fn<T>{}(std::forward<Args>(args)...) };
                             };

template <class T, class... Args>
concept nothrow_constructible_from = requires(Args... args) {
                                       { _construct_fn<T>{}(std::forward<Args>(args)...) } noexcept;
                                     };

/**
 * @brief Deferred constructor
 *
 * @tparam T type to construct
 * @tparam Args constructor argument types
 * @param args constructor arguments
 * @return std::convertible_to<T> auto object that will construct T from the provided arguments on conversion
 */
template <class T, class... Args>
  requires constructible_from<T, Args...>
[[nodiscard]] constexpr auto make_constructor(Args&&... args) noexcept -> std::convertible_to<T> auto{
  return deferred_constructor(
      [... args = std::forward<Args>(args)]() mutable noexcept(nothrow_constructible_from<T, Args...>) -> T {
        return _construct_fn<T>{}(std::forward<decltype(args)>(args)...);
      });
}

template <class T, class... Args>
  requires constructible_from<T, Args...>
[[nodiscard]] constexpr auto make_constructor_ref(Args&&... args) noexcept -> std::convertible_to<T> auto{
  return deferred_constructor([&args...]() mutable noexcept(nothrow_constructible_from<T, Args...>) -> T {
    return _construct_fn<T>{}(std::forward<Args>(args)...);
  });
}

template <class T, class F>
  requires(std::is_invocable_r_v<T, F>)
[[nodiscard]] constexpr auto make_constructor(F&& f) noexcept -> deferred_constructor<F> {
  return deferred_constructor<F>(std::forward<F>(f));
}

template <class T, class F>
[[nodiscard]] constexpr auto make_constructor(deferred_constructor<F>& f) noexcept -> deferred_constructor<F>& {
  return f;
}

struct _construct_at_fn {
  template <mutable_ T, class F>
    requires(std::is_invocable_r_v<T, F>)
  constexpr auto operator()(T* ref, F&& function) const
      noexcept(std::is_nothrow_invocable_v<F>&& std::is_nothrow_constructible_v<T, std::invoke_result_t<F>>) -> T& {
    return *std::construct_at(ref, make_constructor<T>(std::forward<F>(function)));
  }

  template <mutable_ T, class... Args>
    requires constructible_from<T, Args...>
  constexpr auto operator()(T* ref, Args&&... args) const noexcept(nothrow_constructible_from<T, Args...>) -> T& {
    return *std::construct_at(ref, make_constructor<T>(std::forward<Args>(args)...));
  }

  template <mutable_ Alloc, mutable_ T, class F>
    requires(std::is_invocable_r_v<T, F> && maybe_allocator<Alloc>)
  constexpr auto operator()(Alloc& allocator [[maybe_unused]], T* ref, F&& function) const
      noexcept(std::is_nothrow_invocable_v<F>&& std::is_nothrow_constructible_v<T, std::invoke_result_t<F>> &&
               !is_allocator<Alloc>) -> T& {
    if constexpr (is_allocator<Alloc>) {
      using traits = std::allocator_traits<Alloc>;
      traits::construct(allocator, ref, make_constructor<T>(std::forward<F>(function)));
      return *ref;
    } else {
      return (*this)(ref, std::forward<F>(function));
    }
  }

  template <mutable_ Alloc, mutable_ T, class... Args>
    requires(constructible_from<T, Args...> && maybe_allocator<Alloc>)
  constexpr auto operator()(Alloc& allocator [[maybe_unused]], T* ref, Args&&... args) const
      noexcept(nothrow_constructible_from<T, Args...> && !is_allocator<Alloc>) -> T& {
    if constexpr (is_allocator<Alloc>) {
      using traits = std::allocator_traits<Alloc>;
      traits::construct(allocator, ref, make_constructor<T>(std::forward<Args>(args)...));
      return *ref;
    } else {
      return (*this)(ref, std::forward<Args>(args)...);
    }
  }
};

template <class T>
inline constexpr _construct_fn<T> construct;
inline constexpr _construct_at_fn construct_at;

// get reference at position index, has an assertion on index value
template <std::ranges::random_access_range R>
[[nodiscard]] constexpr auto at(R& container, std::ranges::range_size_t<R> index) noexcept
    -> std::ranges::range_reference_t<R> {
  FLAT_HASH_ASSERT(index < std::ranges::size(container), "Index {:d} out of range for size {:d}", index,
                   std::ranges::size(container));
  if constexpr (subscriptable<R, std::ranges::range_size_t<R>>) {
    FLAT_HASH_DIAGNOSTIC_PUSH()
    FLAT_HASH_DIAGNOSTIC_IGNORED_CLANG_GCC("-Wsign-conversion")
    // subrange takes in range_difference_t instead of every other container taking in range_size_t...
    return container[index];
    FLAT_HASH_DIAGNOSTIC_POP()
  } else {
    auto offset = static_cast<std::ranges::range_difference_t<R>>(index);
    return *(std::ranges::begin(container) + offset);
  }
}

// get first element in range
template <std::ranges::random_access_range R>
[[nodiscard]] constexpr auto front(R& container) -> std::ranges::range_reference_t<R> {
  FLAT_HASH_ASSERT(!std::ranges::empty(container), "Container is empty!");
  return *std::ranges::begin(container);
}

// get last element in range
template <std::ranges::random_access_range R>
[[nodiscard]] constexpr auto back(R& container) -> std::ranges::range_reference_t<R> {
  FLAT_HASH_ASSERT(!std::ranges::empty(container), "Container is empty!");
  return *std::ranges::rbegin(container);
}

template <std::ranges::range R, std::integral I>
[[nodiscard]] constexpr auto size_hint_or(R const& r [[maybe_unused]], I v [[maybe_unused]]) noexcept {
  if constexpr (std::ranges::sized_range<R>) {
    return std::ranges::size(r);
  } else {
    return v;
  }
}

template <std::ranges::random_access_range R>
[[nodiscard]] constexpr auto mutable_iterator(R& range, std::ranges::iterator_t<R const> iter) noexcept
    -> std::ranges::iterator_t<R> {
  auto offset = iter - std::ranges::cbegin(range);
  return std::ranges::begin(range) + offset;
}

struct _destroy_fn {
  template <mutable_ T>
  constexpr void operator()(T& ref) const noexcept(std::is_nothrow_destructible_v<T>) {
    std::destroy_at(std::addressof(ref));
  }

  template <mutable_ Alloc, mutable_ T>
    requires maybe_allocator<Alloc>
  constexpr void operator()(Alloc& allocator [[maybe_unused]], T& ref) const
      noexcept(std::is_nothrow_constructible_v<T> && !is_allocator<Alloc>) {
    if constexpr (is_allocator<Alloc>) {
      using traits = std::allocator_traits<Alloc>;
      traits::destroy(allocator, std::addressof(ref));
    } else {
      (*this)(ref);
    }
  }
};

inline constexpr _destroy_fn destroy;

template <mutable_ T>
[[nodiscard]] constexpr auto destructive_move(T& ref) noexcept(
    std::is_nothrow_move_constructible_v<T>and std::is_nothrow_destructible_v<T>) -> T {
  // throwing destructors and moves are a bad idea anyway
  T value = std::move(ref);
  destroy(ref);
  return value;
}

template <mutable_ Alloc, mutable_ T>
  requires maybe_allocator<Alloc>
[[nodiscard]] constexpr auto destructive_move(Alloc& allocator, T& ref) noexcept(
    std::is_nothrow_move_constructible_v<T>and std::is_nothrow_destructible_v<T> && !is_allocator<Alloc>) -> T {
  // throwing destructors and moves are a bad idea anyway
  T value = std::move(ref);
  destroy(allocator, ref);
  return value;
}

template <std::ranges::range R>
  requires mutable_<R>
constexpr void destroy_range(R&& r) noexcept(std::is_nothrow_destructible_v<std::ranges::range_value_t<R>>) {
  if constexpr (!std::is_trivially_destructible_v<std::ranges::range_value_t<R>>) { std::ranges::for_each(r, destroy); }
}

template <maybe_allocator Alloc, std::ranges::range R>
  requires mutable_<R> && mutable_<Alloc>
constexpr void destroy_range(Alloc& allocator,
                             R&& r) noexcept(std::is_nothrow_destructible_v<std::ranges::range_value_t<R>> &&
                                             !is_allocator<Alloc>) {
  std::ranges::for_each(r, [&allocator](auto&& v) { destroy(allocator, v); });
}

template <std::ranges::range R>
using decay_reference_t =
    std::conditional_t<std::ranges::contiguous_range<R>,
                       std::span<std::remove_reference_t<std::ranges::range_reference_t<R&>>>, R&>;
template <std::ranges::range R>
using decay_iterator_t = std::ranges::iterator_t<decay_reference_t<R>>;

template <std::ranges::range R>
[[nodiscard]] constexpr auto decay(R& r) noexcept -> decay_reference_t<R> {
  if constexpr (std::ranges::contiguous_range<R>) {
    return std::span(r);
  } else {
    return r;
  }
}

// for some unknown reasons std::span iterators are not interconvertible with GCC/MSVC even though they can be views to
// the same container
// https://godbolt.org/z/P4Gn5jxeE

template <class T>
concept is_span_iterator =
    requires {
      requires std::same_as<typename std::span<std::remove_reference_t<std::iter_reference_t<T>>>::iterator,
                            std::remove_cvref_t<T>>;
    };

// since GCC/MSVC doesn't convert compatible span iterators, we have to use std::bit_cast to do it manually, this
// concept checks if both types are span iterators and use compatible pointers
template <class From, class To>
concept span_iterator_convertible_to =
    is_span_iterator<From> && is_span_iterator<To> &&
    (std::same_as<From, To> ||
     std::convertible_to<typename std::iterator_traits<From>::pointer, typename std::iterator_traits<To>::pointer>);

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
