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

#include <cstdint>
#include <functional>
#include <string>

#include "bits.hpp"
#include "config.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {
template <class T>
struct hash : std::hash<T> {};

template <class Char, class Traits = std::char_traits<Char>>
struct string_hash {
  using is_transparent = void;

  [[nodiscard]] constexpr auto operator()(std::basic_string_view<Char, Traits> str) const noexcept -> std::size_t {
    return std::hash<std::basic_string_view<Char, Traits>>{}(str);
  }

  template <class T>
    requires(std::constructible_from<std::basic_string_view<Char, Traits>, T>)
  [[nodiscard]] constexpr auto operator()(T const& value) const
      noexcept(noexcept(std::basic_string_view<Char, Traits>(value))) -> std::size_t {
    return (*this)(std::basic_string_view<Char, Traits>(value));
  }
};

template <pointer T>
struct pointer_hash {
  using is_transparent = void;

  [[nodiscard]] constexpr auto operator()(T ptr) const noexcept -> std::size_t { return std::hash<T>{}(ptr); }

  template <smart_pointer_to<T> U>
  [[nodiscard]] constexpr auto operator()(U const& ptr) const noexcept(noexcept(ptr.get())) -> std::size_t {
    return std::hash<T>{}(ptr.get());
  }
};

template <smart_pointer T>
struct smart_pointer_hash {
  using is_transparent = void;
  using element_type = typename T::element_type;

  [[nodiscard]] constexpr auto operator()(T const& ptr) const noexcept(noexcept(std::hash<T>{}(ptr))) -> std::size_t {
    return std::hash<T>{}(ptr);
  }

  [[nodiscard]] constexpr auto operator()(element_type const* ptr) const noexcept -> std::size_t {
    return std::hash<element_type const*>{}(ptr);
  }

  template <smart_pointer_to<element_type const*> U>
  [[nodiscard]] constexpr auto operator()(U const& ptr) const noexcept(noexcept(ptr.get())) -> std::size_t {
    return std::hash<typename U::element_type const*>{}(ptr.get());
  }
};

template <pointer T>
struct pointer_hash_with_null : pointer_hash<T> {
  using pointer_hash<T>::operator();

  [[nodiscard]] constexpr auto operator()(std::nullptr_t) const noexcept -> std::size_t {
    return std::hash<T>{}(static_cast<T>(nullptr));
  }
};

template <std::signed_integral I>
struct signed_int_hash {
  using is_transparent = void;
  constexpr static I max_value = std::numeric_limits<I>::max();
  constexpr static I min_value = std::numeric_limits<I>::min();

  [[nodiscard]] constexpr auto operator()(I value) const noexcept -> std::size_t { return std::hash<I>{}(value); }

  template <std::integral T>
    requires(sizeof(T) < sizeof(I))
  [[nodiscard]] constexpr auto operator()(T value) const noexcept -> std::size_t {
    // can cast without loss of information, even unsigned values
    return std::hash<I>{}(static_cast<I>(value));
  }

  template <std::signed_integral T>
    requires(sizeof(T) > sizeof(I))
  [[nodiscard]] constexpr auto operator()(T value) const noexcept -> std::size_t {
    if (value > max_value || value < min_value) [[unlikely]] { return std::hash<T>{}(value); }

    // value can be fully represented by I so no loss of information
    return std::hash<I>{}(static_cast<I>(value));
  }
};

template <std::unsigned_integral U>
struct unsigned_int_hash {
  using is_transparent = void;
  constexpr static U max_value = std::numeric_limits<U>::max();

  [[nodiscard]] constexpr auto operator()(U value) const noexcept -> std::size_t { return std::hash<U>{}(value); }

  template <std::unsigned_integral T>
    requires(sizeof(T) < sizeof(U))
  [[nodiscard]] constexpr auto operator()(T value) const noexcept -> std::size_t {
    // can cast without loss of information
    return std::hash<U>{}(static_cast<U>(value));
  }

  template <std::unsigned_integral T>
    requires(sizeof(T) > sizeof(U))
  [[nodiscard]] constexpr auto operator()(T value) const noexcept -> std::size_t {
    if (value > max_value) [[unlikely]] { return std::hash<T>{}(value); }

    // value can be fully represented by U so no loss of information
    return std::hash<U>{}(static_cast<U>(value));
  }
};

template <class Char, class Allocator, class Traits>
struct hash<std::basic_string<Char, Traits, Allocator>> : string_hash<Char, Traits> {};

template <class Char, class Traits>
struct hash<std::basic_string_view<Char, Traits>> : string_hash<Char, Traits> {};

template <detail::pointer T>
struct hash<T> : pointer_hash<T> {};

template <detail::smart_pointer T>
struct hash<T> : smart_pointer_hash<T> {};

template <std::signed_integral T>
struct hash<T> : signed_int_hash<T> {};

template <std::unsigned_integral T>
struct hash<T> : unsigned_int_hash<T> {};

}  // namespace detail

FLAT_HASH_NAMESPACE_END
