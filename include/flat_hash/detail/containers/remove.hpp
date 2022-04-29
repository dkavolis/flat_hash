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

#include "../macros.hpp"
#include "policy.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail::containers {

template <class Container>
concept removable = mutable_range<Container>;

template <ordering_policy Policy>
struct _policy_remove_fn;

template <>
struct _policy_remove_fn<ordering_policy::preserved> {
  template <removable Container>
  constexpr auto operator()(Container& container, std::ranges::iterator_t<Container const> first_,
                            std::ranges::iterator_t<Container const> last_) const
      -> std::ranges::range_difference_t<Container> {
    using diff_t = std::ranges::range_difference_t<Container>;

    if (first_ == last_) [[unlikely]] { return 0; }

    FLAT_HASH_ASSERT(last_ > first_, "Reversed range!");

    auto middle = mutable_iterator(container, last_);
    auto target = mutable_iterator(container, first_);
    diff_t const count = middle - target;
    auto const last = std::ranges::end(container);
    auto source = middle;
    auto allocator = maybe_get_allocator(container);

    while (target != middle && source != last) {
      auto* ptr = std::addressof(*target);
      destroy(allocator, *ptr);
      construct_at(allocator, ptr, [source] { return destructive_move(*source); });

      ++target;
      ++source;
    }

    // [begin, target) now contains the final range, need to remove the remaining elements
    // [source, last) contains moved from and destroyed elements
    // [target, middle) may contain elements to remove
    if constexpr (appendable_from_capacity<Container>) {
      if (source != last) {
        // [middle, source) contains destroyed objects, all the elements to remove have been destroyed
        std::ranges::uninitialized_move(std::ranges::subrange(source, last) |
                                            std::views::transform([](auto& ref) { return destructive_move(ref); }),
                                        std::ranges::subrange(middle, last));
      } else {
        // [target, middle) contains objects to remove
        destroy_range(allocator, std::ranges::subrange(target, middle));
      }
    }

    return count;
  }

  template <removable Container>
  constexpr auto operator()(Container& container, std::ranges::iterator_t<Container const> pos) const
      -> std::ranges::range_difference_t<Container> {
    FLAT_HASH_ASSERT(pos != std::ranges::cend(container), "Cannot remove past-the-end element");
    return (*this)(container, pos, std::ranges::next(pos));
  }
};

template <>
struct _policy_remove_fn<ordering_policy::relaxed> {
  template <removable Container>
  constexpr auto operator()(Container& container, std::ranges::iterator_t<Container const> first_,
                            std::ranges::iterator_t<Container const> last_) const
      -> std::ranges::range_difference_t<Container> {
    using diff_t = std::ranges::range_difference_t<Container>;

    if (first_ == last_) [[unlikely]] { return 0; }
    FLAT_HASH_ASSERT(last_ > first_, "Reversed range!");

    auto target = mutable_iterator(container, first_);
    auto const middle = mutable_iterator(container, last_);
    diff_t const count = middle - target;
    auto source = std::ranges::end(container);
    auto allocator = maybe_get_allocator(container);

    while (target != middle && source != middle) {
      --source;

      auto* ptr = std::addressof(*target);
      destroy(allocator, *ptr);
      if constexpr (appendable_from_capacity<Container>) {
        construct_at(allocator, ptr, [source] { return destructive_move(*source); });
      } else {
        // no reason to destroy the moved-from object they will be destroyed by other methods
        construct_at(allocator, ptr, [source] { return std::move(*source); });
      }

      ++target;
    }

    // target == middle -> all removed elements moved to the end, source is the new end
    // source == middle -> [target, middle) contains alive elements, [middle, end(container)) - destroyed, target is the
    // new end
    if constexpr (appendable_from_capacity<Container>) {
      if (source == middle) { destroy_range(allocator, std::ranges::subrange(target, middle)); }
    }

    return count;
  }

  template <removable Container>
  constexpr auto operator()(Container& container, std::ranges::iterator_t<Container const> pos) const
      -> std::ranges::range_difference_t<Container> {
    FLAT_HASH_ASSERT(pos != std::ranges::cend(container), "Cannot remove past-the-end element");
    return (*this)(container, pos, std::ranges::next(pos));
  }
};

struct _runtime_policy_remove_fn {
  template <removable Container>
  constexpr auto operator()(ordering_policy policy, Container& container,
                            std::ranges::iterator_t<Container const> first,
                            std::ranges::iterator_t<Container const> last) const
      -> std::ranges::range_difference_t<Container> {
    switch (policy) {
      case ordering_policy::relaxed: return _policy_remove_fn<ordering_policy::relaxed>{}(container, first, last);
      case ordering_policy::preserved: return _policy_remove_fn<ordering_policy::preserved>{}(container, first, last);
      default: FLAT_HASH_UNREACHABLE();
    }
  }

  template <removable Container>
  constexpr auto operator()(ordering_policy policy, Container& container,
                            std::ranges::iterator_t<Container const> pos) const
      -> std::ranges::range_difference_t<Container> {
    switch (policy) {
      case ordering_policy::relaxed: return _policy_remove_fn<ordering_policy::relaxed>{}(container, pos);
      case ordering_policy::preserved: return _policy_remove_fn<ordering_policy::preserved>{}(container, pos);
      default: FLAT_HASH_UNREACHABLE();
    }
  }
};

template <ordering_policy Policy>
struct _remove_fn : _policy_remove_fn<Policy>, _runtime_policy_remove_fn {
  using _policy_remove_fn<Policy>::operator();
  using _runtime_policy_remove_fn::operator();
};

template <ordering_policy Policy>
inline constexpr _remove_fn<Policy> policy_remove{};
inline constexpr _remove_fn<ordering_policy::preserved> remove{};

}  // namespace detail::containers

FLAT_HASH_NAMESPACE_END
