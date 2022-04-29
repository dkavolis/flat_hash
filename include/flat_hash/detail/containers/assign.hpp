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
#include "capacity.hpp"
#include "clear.hpp"
#include "container_traits.hpp"
#include "reserve.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {

template <class S, class Container>
concept assignable_to =
    std::ranges::range<Container> && std::ranges::range<S> &&
    ((appendable_from_capacity<Container> &&
      std::is_assignable_v<std::ranges::range_reference_t<Container&>, std::ranges::range_value_t<S>>) ||
     traits_assignable<Container, S> ||
     traits_assignable<Container, std::ranges::iterator_t<S>, std::ranges::sentinel_t<S>>);

template <std::ranges::sized_range Container, std::ranges::input_range Source>
constexpr void maybe_reserve_for_extra(Container& container, Source const& source [[maybe_unused]]) {
  if constexpr (reservable<Container> && std::ranges::sized_range<Source>) {
    auto const cap = capacity(container);
    auto const old_size = std::ranges::size(container);
    auto const extra_size = std::ranges::size(source);
    if (cap < old_size + extra_size) {
      auto const new_size = growth_size(cap, old_size, extra_size);
      reserve(container, new_size);
    }
  } else {
    return;
  }
}

template <class Container, class Source>
constexpr void assign_or_append(Container& container, Source&& source, auto lazy_target_fn, auto&& fallback) {
  if constexpr (appendable_from_capacity<Container>) {
    maybe_reserve_for_extra(container, source);
    auto target = lazy_target_fn();
    auto const begin = target;
    auto const last = target + static_cast<std::ranges::range_difference_t<Container>>(capacity(container));

    if constexpr (std::ranges::borrowed_range<Source>) {
      target = std::ranges::uninitialized_copy(source, std::ranges::subrange(target, last)).out;
    } else {
      target = std::ranges::uninitialized_move(std::move(source), std::ranges::subrange(target, last)).out;
    }

    append_from_capacity(container, target - begin);
  } else {
    fallback(container, std::forward<Source>(source));
  }
}

struct _assign_to_empty_fn {
  template <class Container, assignable_to<Container> S>
  constexpr void operator()(Container& container, S&& source) const {
    FLAT_HASH_ASSERT(std::ranges::empty(container));

    assign_or_append(
        container, std::forward<S>(source), [&container] { return appendable_begin(container); },
        [](Container& container_ [[maybe_unused]], S&& src [[maybe_unused]]) {
          if constexpr (traits_assignable<Container, S>) {
            container_traits<Container>::assign(container_, std::forward<S>(src));
          } else if constexpr (traits_assignable<Container, std::ranges::iterator_t<S>, std::ranges::sentinel_t<S>>) {
            container_traits<Container>::assign(container_, std::ranges::begin(src), std::ranges::end(src));
          } else {
            static_assert(appendable_from_capacity<Container>, "something went wrong!");
          }
        });
  }
};

struct _assign_fn {
  template <class Container, assignable_to<Container> S>
  constexpr void operator()(Container& container, S&& source) const {
    if constexpr (traits_assignable<Container, S>) {
      container_traits<Container>::assign(container, std::forward<S>(source));
    } else if constexpr (traits_assignable<Container, std::ranges::iterator_t<S>, std::ranges::sentinel_t<S>>) {
      container_traits<Container>::assign(container, std::ranges::begin(source), std::ranges::end(source));
    } else if constexpr (appendable_from_capacity<Container>) {
      clear(container);
      _assign_to_empty_fn{}(container, std::forward<S>(source));
    } else {
      FLAT_HASH_UNREACHABLE();
    }
  }
};

inline constexpr _assign_to_empty_fn assign_to_empty;
inline constexpr _assign_fn assign;

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
