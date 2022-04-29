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

#include <iterator>

#include "../macros.hpp"
#include "append.hpp"
#include "capacity.hpp"
#include "container_traits.hpp"
#include "reserve.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {
template <class T, class Container>
concept insertible_to = traits_insertible<Container, T> ||
                        (std::convertible_to<T, std::ranges::range_value_t<Container>> &&
                         back_emplaceable<Container, T>) ||
                        (std::ranges::range<T> &&
                         (appendable_to<T, Container> ||
                          traits_insertible<Container, std::ranges::iterator_t<T>, std::ranges::sentinel_t<T>>));

/**
 * @brief Reorder elements according to ordering_policy after appending such as when inserting
 *
 * @tparam Policy
 * @tparam It
 * @param first start of the range
 * @param middle iterator to the element that should be first
 * @param last end of the range
 */
template <ordering_policy Policy = ordering_policy::preserved, std::random_access_iterator It>
constexpr void order_elements(It first, It middle,
                              It last) noexcept(std::is_nothrow_move_assignable_v<std::iter_value_t<It>>) {
  if constexpr (Policy == ordering_policy::preserved) {
    // need to rotate elements into place
    std::ranges::rotate(first, middle, last);
  } else if constexpr (Policy == ordering_policy::relaxed) {
    // only swap the inserted elements
    while (middle != last) {
      std::ranges::iter_swap(first, middle);
      ++first;
      ++middle;
    }
  } else {
    static_assert(Policy == ordering_policy::preserved, "missing branch!");
  }
}

template <ordering_policy Policy>
struct _policy_insert_fn {
  template <class Container, insertible_to<Container> Source>
    requires(std::ranges::range<Source> && !std::constructible_from<std::ranges::range_value_t<Container>, Source>)
  constexpr auto operator()(Container& container, std::ranges::iterator_t<Container const> pos, Source&& source) const
      -> std::ranges::iterator_t<Container> {
    auto const offset = pos - std::ranges::cbegin(container);

    if constexpr (Policy == ordering_policy::preserved && traits_insertible<Container, Source>) {
      container_traits<Container>::insert(container, pos, std::forward<Source>(source));
    } else if constexpr (Policy == ordering_policy::preserved &&
                         traits_insertible<Container, std::ranges::iterator_t<Source>,
                                           std::ranges::sentinel_t<Source>>) {
      container_traits<Container>::insert(container, pos, std::ranges::begin(source), std::ranges::end(source));
    } else {
      auto const ssize = std::ranges::ssize(container);
      // append is the only throwing method unless swaps can throw
      /* TODO: if capacity is not enough, can construct a new container and move items directly into final spots
       * though not sure of the benefits if the items need to be initialized somehow first without
       * append_from_capacity. For now a simple implementation.
       */
      append(container, std::forward<Source>(source));

      auto const begin = std::ranges::begin(container);
      auto first = begin + offset;
      if (offset == ssize) {
        return first;  // inserting at the end, nothing to do
      }

      auto const last = std::ranges::end(container);
      auto middle = begin + ssize;
      if (last == middle) {
        return first;  // nothing inserted
      }

      order_elements<Policy>(first, middle, last);
    }

    return std::ranges::begin(container) + offset;
  }

  template <class Container, insertible_to<Container> T>
    requires(std::constructible_from<std::ranges::range_value_t<Container>, T>)
  constexpr auto operator()(Container& container, std::ranges::iterator_t<Container const> pos, T&& value) const
      -> std::ranges::iterator_t<Container> {
    auto const offset = pos - std::ranges::cbegin(container);

    if constexpr (traits_insertible<Container, T> && Policy == ordering_policy::preserved) {
      container_traits<Container>::insert(container, pos, std::forward<T>(value));
    } else {
      auto const ssize = std::ranges::ssize(container);
      emplace_back(container, std::forward<T>(value));

      auto const begin = std::ranges::begin(container);
      auto first = begin + offset;
      if (offset == ssize) {
        return first;  // inserted at the end
      }

      auto middle = begin + ssize;
      if constexpr (Policy == ordering_policy::preserved) {
        auto const last = std::ranges::end(container);
        std::ranges::rotate(first, middle, last);
      } else if constexpr (Policy == ordering_policy::relaxed) {
        std::ranges::iter_swap(first, middle);
      } else {
        static_assert(Policy == ordering_policy::preserved, "missing branch!");
      }
    }

    return std::ranges::begin(container) + offset;
  }
};

struct _runtime_policy_insert_fn {
  template <class Container, insertible_to<Container> T>
  constexpr auto operator()(ordering_policy policy, Container& container, std::ranges::iterator_t<Container const> pos,
                            T&& value) const -> std::ranges::iterator_t<Container> {
    switch (policy) {
      case ordering_policy::relaxed:
        return _policy_insert_fn<ordering_policy::relaxed>{}(container, pos, std::forward<T>(value));
      case ordering_policy::preserved:
        return _policy_insert_fn<ordering_policy::preserved>{}(container, pos, std::forward<T>(value));
      default: FLAT_HASH_UNREACHABLE();
    }
  }
};

template <ordering_policy Policy>
struct _insert_fn : _policy_insert_fn<Policy>, _runtime_policy_insert_fn {
  using _policy_insert_fn<Policy>::operator();
  using _runtime_policy_insert_fn::operator();
};

template <ordering_policy Policy>
inline constexpr _insert_fn<Policy> policy_insert{};
inline constexpr _insert_fn<ordering_policy::preserved> insert{};
}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
