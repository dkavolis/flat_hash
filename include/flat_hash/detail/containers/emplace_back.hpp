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

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {

template <class Container, class... Args>
concept back_emplaceable = traits_back_emplaceable<Container, Args...> || traits_insertible<Container, Args...> ||
                           (std::constructible_from<std::ranges::range_value_t<Container>, Args...> &&
                            traits_insertible<Container, std::ranges::range_value_t<Container>>);

template <class Container, class... Args>
  requires back_emplaceable<Container, Args...>
constexpr auto emplace_back(Container& container, Args&&... args) -> std::ranges::range_reference_t<Container> {
  using value_t = std::ranges::range_value_t<Container>;

  if constexpr (traits_back_emplaceable<Container, Args...>) {
    container_traits<Container>::emplace_back(container, std::forward<Args>(args)...);
  } else if constexpr (traits_insertible<Container, Args...>) {
    container_traits<Container>::insert(container, std::ranges::cend(container), std::forward<Args>(args)...);
  } else if constexpr (traits_insertible<Container, value_t>) {
    container_traits<Container>::insert(container, std::ranges::cend(container),
                                        make_constructor<value_t>(std::forward<Args>(args)...));
  }

  return back(container);
}

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
