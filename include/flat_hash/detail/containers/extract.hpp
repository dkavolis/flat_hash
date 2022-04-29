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

#include "../macros.hpp"
#include "container_traits.hpp"
#include "erase.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {
template <class Container>
concept extractable = erasable<Container>;

template <ordering_policy Policy = ordering_policy::preserved, extractable Container>
[[nodiscard]] constexpr auto extract(Container& container, std::ranges::iterator_t<Container const> pos)
    -> std::ranges::range_value_t<Container> {
  FLAT_HASH_ASSERT(pos != std::ranges::cend(container));
  auto value = std::move(*mutable_iterator(container, pos));

  policy_erase<Policy>(container, pos);
  return value;
}
}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
