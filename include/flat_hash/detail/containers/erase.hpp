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

#include <concepts>
#include <memory>

#include "../macros.hpp"
#include "capacity.hpp"
#include "container_traits.hpp"
#include "get_allocator.hpp"
#include "policy.hpp"
#include "remove.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {

template <class Container>
concept erasable = traits_range_erasable<Container> || ((traits_erasable<Container> || traits_back_popable<Container> ||
                                                         appendable_from_capacity<Container>) &&
                                                        mutable_range<Container>);

enum struct destruct_policy {
  skip,
  destruct,
};

template <destruct_policy policy = destruct_policy::destruct, erasable Container>
constexpr auto erase_after(Container& container, std::ranges::iterator_t<Container const> pos_)
    -> std::ranges::iterator_t<Container> {
  auto const last = std::ranges::end(container);
  if constexpr (traits_range_erasable<Container>) {
    container_traits<Container>::erase(container, pos_, last);
  } else {
    auto const pos = mutable_iterator(container, pos_);
    auto const count = last - pos;

    if constexpr (appendable_from_capacity<Container>) {
      if constexpr (policy == destruct_policy::destruct) {
        auto allocator = maybe_get_allocator(container);
        destroy_range(allocator, std::ranges::subrange(pos, last));
      }
      append_from_capacity(container, -count);
    } else {
      auto const ssize = std::ranges::ssize(container);
      for (auto i [[maybe_unused]] : std::views::iota(ssize - count, ssize) | std::views::reverse) {
        if constexpr (traits_back_popable<Container>) {
          // std::vector is not marked noexcept but it doesn't throw
          container_traits<Container>::pop_back(container);
        } else {
          // should not throw as it is only destroying the object, no assignments
          container_traits<Container>::erase(container, std::ranges::begin(container) + i);
        }
      }
    }
  }
  return std::ranges::end(container);
}

template <ordering_policy Policy>
struct _erase_fn {
  template <erasable Container>
  constexpr auto operator()(Container& container, std::ranges::iterator_t<Container const> first_,
                            std::ranges::iterator_t<Container const> last_) const
      -> std::ranges::iterator_t<Container> {
    using diff_t = std::ranges::range_difference_t<Container>;

    diff_t const offset = first_ - std::ranges::cbegin(container);

    if (first_ == last_) [[unlikely]] { return std::ranges::begin(container) + offset; }

    FLAT_HASH_ASSERT(last_ > first_, "Reversed range!");

    if constexpr (traits_range_erasable<Container> && Policy == ordering_policy::preserved) {
      container_traits<Container>::erase(container, first_, last_);
    } else {
      auto count = policy_remove<Policy>(container, first_, last_);
      // remove destroys removed elements
      erase_after<destruct_policy::skip>(container, std::ranges::cend(container) - count);
    }

    return std::ranges::begin(container) + offset;
  }

  template <erasable Container>
  constexpr auto operator()(Container& container, std::ranges::iterator_t<Container const> pos) const
      -> std::ranges::iterator_t<Container> {
    FLAT_HASH_ASSERT(pos != std::ranges::cend(container), "Cannot erase past-the-end element");
    return (*this)(container, pos, std::ranges::next(pos));
  }

  template <erasable Container>
  constexpr auto operator()(ordering_policy policy, Container& container,
                            std::ranges::iterator_t<Container const> first,
                            std::ranges::iterator_t<Container const> last) const -> std::ranges::iterator_t<Container> {
    switch (policy) {
      case ordering_policy::relaxed: return _erase_fn<ordering_policy::relaxed>{}(container, first, last);
      case ordering_policy::preserved: return _erase_fn<ordering_policy::preserved>{}(container, first, last);
      default: FLAT_HASH_UNREACHABLE();
    }
  }

  template <erasable Container>
  constexpr auto operator()(ordering_policy policy, Container& container,
                            std::ranges::iterator_t<Container const> pos) const -> std::ranges::iterator_t<Container> {
    switch (policy) {
      case ordering_policy::relaxed: return _erase_fn<ordering_policy::relaxed>{}(container, pos);
      case ordering_policy::preserved: return _erase_fn<ordering_policy::preserved>{}(container, pos);
      default: FLAT_HASH_UNREACHABLE();
    }
  }
};

template <class T, class Pred>
concept std_erasable_if = requires(T& r, Pred pred) {
                            { std::erase_if(r, pred) } -> std::integral;
                          };

template <ordering_policy Policy>
struct _erase_if_fn {
  template <erasable R, std::predicate<std::ranges::range_reference_t<R>> Pred>
  constexpr auto operator()(R&& r, Pred pred) const -> std::ranges::range_size_t<R> {
    using size_type = std::ranges::range_size_t<R>;
    if constexpr (std_erasable_if<R, Pred> && Policy == ordering_policy::preserved) {
      return std::erase_if(r, pred);
    } else {
      auto first = std::ranges::begin(r);
      auto const end = std::ranges::end(r);
      auto last = end;
      size_type removed = 0;

      if constexpr (Policy == ordering_policy::preserved || !std::ranges::bidirectional_range<R>) {
        // remove_if returns subrange of removed elements
        // https://en.cppreference.com/w/cpp/algorithm/ranges/remove
        last = std::ranges::remove_if(first, end, pred).begin();
      } else {
        while (first != last) {
          // fill in the holes from the end of the range
          if (std::invoke(pred, *first)) {
            *first = std::ranges::iter_move(--last);
          } else {
            ++first;
          }
        }
      }

      removed = static_cast<size_type>(std::ranges::distance(last, end));
      // remove_if doesn't invalidate iterators since it only shifts the elements around
      erase_after(r, last);

      return removed;
    }
  }

  template <erasable R, std::predicate<std::ranges::range_reference_t<R>> Pred>
  constexpr auto operator()(ordering_policy policy, R&& r, Pred pred) const -> std::ranges::range_size_t<R> {
    switch (policy) {
      case ordering_policy::relaxed: return _erase_if_fn<ordering_policy::relaxed>{}(std::forward<R>(r), pred);
      case ordering_policy::preserved: return _erase_if_fn<ordering_policy::preserved>{}(std::forward<R>(r), pred);
      default: FLAT_HASH_UNREACHABLE();
    }
  }
};

template <ordering_policy Policy>
inline constexpr _erase_fn<Policy> policy_erase{};
inline constexpr _erase_fn<ordering_policy::preserved> erase{};

template <ordering_policy Policy>
inline constexpr _erase_if_fn<Policy> policy_erase_if{};
inline constexpr _erase_if_fn<ordering_policy::preserved> erase_if{};

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
