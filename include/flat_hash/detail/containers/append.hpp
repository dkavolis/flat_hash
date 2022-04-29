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
#include "assign.hpp"
#include "capacity.hpp"
#include "container_traits.hpp"
#include "emplace_back.hpp"
#include "reserve.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {
template <class S, class Container>
concept appendable_to =
    std::ranges::range<Container> && std::ranges::range<S> &&
    ((appendable_from_capacity<Container> &&
      std::is_assignable_v<std::ranges::range_reference_t<Container>, std::ranges::range_value_t<S>>) ||
     traits_insertible<Container, S> ||
     traits_insertible<Container, std::ranges::iterator_t<S>, std::ranges::sentinel_t<S>> ||
     back_emplaceable<Container, std::ranges::range_value_t<S>>);

struct _append_fn {
  template <class Container, appendable_to<Container> S>
  constexpr auto operator()(Container& container, S&& source) const -> std::ranges::iterator_t<Container> {
    auto offset = std::ranges::ssize(container);

    assign_or_append(
        container, std::forward<S>(source), [&container, offset] { return appendable_begin(container) + offset; },
        [](Container& container_, S&& source_) {
          if constexpr (traits_insertible<Container, S>) {
            container_traits<Container>::insert(container_, std::ranges::cend(container_), std::forward<S>(source_));
          } else if constexpr (traits_insertible<Container, std::ranges::iterator_t<S>, std::ranges::sentinel_t<S>>) {
            container_traits<Container>::insert(container_, std::ranges::cend(container_), std::ranges::begin(source_),
                                                std::ranges::end(source_));
          } else {
            maybe_reserve_for_extra(container_, source_);

            for (auto&& value : source_) {
              if constexpr (std::ranges::borrowed_range<S>) {
                emplace_back(container_, value);
              } else {
                emplace_back(container_, std::move(value));
              }
            }
          }
        });

    return std::ranges::begin(container) + offset;
  }
};

inline constexpr _append_fn append;

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
