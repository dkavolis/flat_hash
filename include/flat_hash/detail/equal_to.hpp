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

#include "bits.hpp"
#include "config.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {

struct equal_to : std::equal_to<void> {
  using std::equal_to<void>::operator();

  template <pointer T, smart_pointer_to<T> U>
  [[nodiscard]] constexpr auto operator()(T lhs, U const& rhs) const noexcept(noexcept(rhs.get())) -> bool {
    return lhs == rhs.get();
  }

  template <pointer T, smart_pointer_to<T> U>
  [[nodiscard]] constexpr auto operator()(U const& rhs, T lhs) const noexcept(noexcept(rhs.get())) -> bool {
    return lhs == rhs.get();
  }

  template <smart_pointer T, smart_pointer V>
    requires(std::equality_comparable_with<typename T::element_type const*, typename V::element_type const*>)
  [[nodiscard]] constexpr auto operator()(T const& lhs, V const& rhs) const
      noexcept(noexcept(lhs.get()) && noexcept(rhs.get())) -> bool {
    return lhs.get() == rhs.get();
  }
};
}  // namespace detail

FLAT_HASH_NAMESPACE_END
