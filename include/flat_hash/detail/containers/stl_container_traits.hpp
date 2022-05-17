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

#include <ranges>
#include <utility>

#include "../macros.hpp"
#include "allocators.hpp"
#include "concepts.hpp"
#include "utility.hpp"

FLAT_HASH_NAMESPACE_BEGIN

template <std::ranges::random_access_range R>
struct stl_container_traits {
  // type aliases are not required to be implemented by custom container_traits
  // only one of the overloads need to be provided for the associated functionality
  using size_type = std::ranges::range_size_t<R>;
  using value_type = std::ranges::range_value_t<R>;
  using reference = std::ranges::range_reference_t<R>;
  using difference_type = std::ranges::range_difference_t<R>;
  using const_reference = std::ranges::range_reference_t<R const>;
  using iterator = std::ranges::iterator_t<R>;
  using const_iterator = std::ranges::iterator_t<R const>;
  using allocator_type = detail::containers::allocator_t<R>;

  /**
   * @brief Construct container with a given size
   *
   * @param size initial container size
   *
   * @return container
   */
  [[nodiscard]] constexpr static auto construct(size_type size) noexcept(
      detail::containers::nothrow_constructible_from<R, size_type>) -> R
      requires(!detail::containers::allocator_aware<R> && detail::containers::constructible_from<R, size_type>) {
        return detail::containers::construct<R>(size);
      }

  /**
   * @brief Construct container with a given size filled with value
   *
   * @param size initial container size
   * @param value fill value
   *
   * @return container
   */
  [[nodiscard]] constexpr static auto construct(size_type size, value_type const& value) noexcept(
      detail::containers::nothrow_constructible_from<R, size_type, value_type const&>) -> R
      requires(!detail::containers::allocator_aware<R> &&
               detail::containers::constructible_from<R, value_type, size_type>) {
        return detail::containers::construct<R>(size, value);
      }

  /**
   * @brief Construct container with a given size and allocator
   *
   * @param size initial container size
   * @param allocator allocator
   *
   * @return container
   */
  [[nodiscard]] constexpr static auto construct(size_type size, optional_allocator<R> const& allocator) noexcept(
      detail::containers::nothrow_constructible_from<R, size_type, allocator_type>) -> R
      requires(detail::containers::allocator_aware<R>&&
                   detail::containers::constructible_from<R, size_type, allocator_type>) {
        return detail::containers::construct<R>(size, allocator);
      }

  /**
   * @brief Construct container with a given size and allocator and filled with value
   *
   * @param size initial container size
   * @param value fill value
   * @param allocator allocator
   *
   * @return container
   */
  [[nodiscard]] constexpr static auto construct(
      size_type size, value_type const& value,
      optional_allocator<R> const&
          allocator) noexcept((detail::containers::nothrow_constructible_from<R, size_type, value_type,
                                                                              allocator_type>)) -> R
      requires(detail::containers::allocator_aware<R>&&
                   detail::containers::constructible_from<R, size_type, value_type, allocator_type>) {
        return detail::containers::construct<R>(size, value, allocator);
      }

  /**
   * @brief Resize the container
   * Enabled if container.resize(new_size) is valid
   *
   * @param container container to resize
   * @param new_size new size of the container
   */
  constexpr static void resize(R& container, size_type new_size)
    requires detail::containers::member_resize<R, size_type>
  {
    container.resize(new_size);
  }

  /**
   * @brief Resize the container
   * Enabled if container.resize(new_size, value) is valid
   *
   * @param container container to resize
   * @param new_size new size of the container
   * @param value value to use for the new items
   */
  constexpr static void resize(R& container, size_type new_size, value_type const& value)
    requires detail::containers::member_resize<R, size_type, value_type const&>
  {
    container.resize(new_size, value);
  }

  /**
   * @brief Reserve memory in the container
   * Enable if container.reserve(n) is valid
   *
   * @param container
   * @param n reserve this many items
   */
  constexpr static void reserve(R& container, size_type n)
    requires detail::containers::member_reserve<R, size_type>
  {
    container.reserve(n);
  }

  /**
   * @brief Query the capacity of the container
   * Enabled if container.capacity() is valid and returns size_type
   *
   * @param container
   * @return size_type capacity of the container
   */
  [[nodiscard]] constexpr static auto capacity(R const& container) noexcept(
      detail::containers::member_nothrow_capacity<R>) -> size_type
    requires detail::containers::member_capacity<R>
  {
    return container.capacity();
  }

  /**
   * @brief Query the allocator of the container
   * Enabled if container.get_allocator() is valid
   *
   * @param container
   * @return allocator used by the container
   */
  [[nodiscard]] constexpr static auto get_allocator(R const& container) noexcept -> allocator_type
      requires(detail::containers::member_get_allocator<R>) { return container.get_allocator(); }

  /**
   * @brief Clear the container
   * Enabled if container.clear() is valid
   *
   * @param container
   */
  constexpr static void clear(R& container) noexcept(detail::containers::member_nothrow_clear<R>)
    requires detail::containers::member_clear<R>
  {
    container.clear();
  }

  template <std::input_iterator It, std::sentinel_for<It> S>
    requires detail::containers::member_assign<R, It, S>
  constexpr static void assign(R& container, It first, S last) {
    container.assign(first, last);
  }

  template <std::ranges::range S>
    requires(detail::containers::member_assign<R, S> ||
             detail::containers::member_assign<R, std::ranges::iterator_t<S>, std::ranges::sentinel_t<S>>)
  constexpr static void assign(R& container, S&& source) {
    if constexpr (detail::containers::member_assign<R, S>) {
      container.assign(std::forward<S>(source));
    } else {
      container.assign(std::ranges::begin(source), std::ranges::end(source));
    }
  }

  /**
   * @brief Emplace value at the back of the container
   * Enabled if the container has valid emplace_back(args...),  or push_back, add, insert or emplace methods that take
   * value_type&& arguments
   *
   * @tparam Args
   * @param container
   * @param args arguments to value_type constructor
   * @return reference to the emplaced element (preferred)
   */
  template <class... Args>
    requires(detail::containers::constructible_from<value_type, Args...> &&
             (detail::containers::member_push_back<R, value_type> ||
              detail::containers::member_emplace_back<R, value_type> ||
              detail::containers::member_insert<R, value_type> || detail::containers::member_add<R, value_type> ||
              detail::containers::member_emplace<R, value_type>))
  constexpr static auto emplace_back(R& container, Args&&... args) -> decltype(auto) {
    auto&& lazy_value = detail::containers::make_constructor<value_type>(std::forward<Args>(args)...);
    if constexpr (detail::containers::member_emplace_back<R, value_type>) {
      return container.emplace_back(lazy_value);
    } else if constexpr (detail::containers::member_push_back<R, value_type>) {
      return container.push_back(lazy_value);
    } else if constexpr (detail::containers::member_add<R, value_type>) {
      return container.add(lazy_value);
    } else if constexpr (detail::containers::member_insert<R, value_type>) {
      return container.insert(std::ranges::end(container), lazy_value);
    } else if constexpr (detail::containers::member_emplace<R, value_type>) {
      return container.emplace(std::ranges::end(container), lazy_value);
    } else {
      FLAT_HASH_UNREACHABLE();
    }
  }

  /**
   * @brief Erase element at position pos
   * Enabled if container.erase(pos) or container.remove(pos) is valid
   * Should not throw if pos == end(container)
   *
   * @param container
   * @param pos iterator to element to erase
   * @return iterator following the removed element (preferred)
   */
  constexpr static auto erase(R& container, const_iterator pos) -> decltype(auto)
    requires(detail::containers::member_erase<R, const_iterator> ||
             detail::containers::member_remove<R, const_iterator>)
  {
    if constexpr (detail::containers::member_erase<R, const_iterator>) {
      return container.erase(pos);
    } else if constexpr (detail::containers::member_remove<R, const_iterator>) {
      return container.remove(pos);
    } else {
      FLAT_HASH_UNREACHABLE();
    }
  }

  /**
   * @brief Erase elements in range [first, last)
   * Enabled if container.erase(first, last) is valid
   * Should not throw if last == end(container)
   *
   * @param container
   * @param first iterator to the first element to erase
   * @param last iterator to the end of the range of element to remove
   * @return iterator following the last removed element (prefferred)
   */
  constexpr static auto erase(R& container, const_iterator first, const_iterator last) -> decltype(auto)
    requires(detail::containers::member_erase<R, const_iterator, const_iterator>)
  {
    return container.erase(first, last);
  }

  /**
   * @brief Insert a range of values starting at pos
   *
   * @tparam Values input range of values convertible to container value_type
   * @param container
   * @param pos position to insert values at
   * @param values values to insert
   * @return iterator to the first element inserted (preffered)
   */
  template <std::ranges::input_range Values>
    requires(detail::containers::member_insert<R, Values> ||
             (detail::containers::member_insert<R, value_type&&> &&
              detail::containers::constructible_from<value_type, Values>) ||
             detail::containers::member_insert<R, std::ranges::iterator_t<Values>, std::ranges::sentinel_t<Values>>)
  constexpr static auto insert(R& container, const_iterator pos, Values&& values) -> decltype(auto) {
    if constexpr (detail::containers::member_insert<R, Values>) {
      return container.insert(pos, std::forward<Values>(values));
    } else if constexpr (detail::containers::constructible_from<value_type, Values>) {
      return container.insert(pos, detail::containers::construct<value_type>(std::forward<Values>(values)));
    } else {
      return container.insert(pos, std::ranges::begin(values), std::ranges::end(values));
    }
  }

  /**
   * @brief Insert element at pos
   * Enabled if container.emplace(pos, args...) or container.insert(pos, value_type) is valid
   *
   * @param container
   * @param pos iterator to position where the element is inserted
   * @param value value to insert
   * @return iterator to the inserted element (preffered)
   */
  constexpr static auto insert(R& container, const_iterator pos, value_type&& value) -> decltype(auto)
    requires(detail::containers::member_insert<R, value_type> || detail::containers::member_emplace<R, value_type>)
  {
    if constexpr (detail::containers::member_emplace<R, value_type>) {
      return container.emplace(pos, std::move(value));
    } else if constexpr (detail::containers::member_insert<R, value_type>) {
      return container.insert(pos, std::move(value));
    } else {
      FLAT_HASH_UNREACHABLE();
    }
  }

  /**
   * @copydoc stl_containers_traits::insert(R&, const_iterator, value_type&&)
   */
  template <class T>
    requires(detail::containers::member_insert<R, T> || detail::containers::member_emplace<R, T>)
  constexpr static auto insert(R& container, const_iterator pos, T&& value) -> decltype(auto) {
    if constexpr (detail::containers::member_emplace<R, T>) {
      return container.emplace(pos, std::forward<T>(value));
    } else if constexpr (detail::containers::member_insert<R, T>) {
      return container.insert(pos, std::forward<T>(value));
    } else {
      FLAT_HASH_UNREACHABLE();
    }
  }

  /**
   * @brief Remove the last element from the container. Should not throw.
   *
   * @param container
   * @return removed element (preffered)
   */
  constexpr static auto pop_back(R& container) noexcept(detail::containers::member_nothrow_pop_back<R>)
      -> decltype(auto)
    requires(detail::containers::member_pop_back<R>)
  {
    return container.pop_back();
  }

  /**
   * @brief Remove the first element from the container. Should not throw.
   *
   * @param container
   * @return removed element (preffered)
   */
  constexpr static auto pop_front(R& container) noexcept(detail::containers::member_nothrow_pop_front<R>)
      -> decltype(auto)
    requires(detail::containers::member_pop_front<R>)
  {
    return container.pop_front();
  }

  /**
   * @brief Convenience function that allows constructing and destructing objects inside capacity for potentially
   * better codegen. Should be constant time that only updates the size or end pointer members. Should not throw.
   *
   * @param container
   * @param n size difference
   */
  constexpr static void append_from_capacity(R& container, difference_type n) noexcept(
      detail::containers::member_nothrow_append_from_capacity<R>)
    requires detail::containers::member_append_from_capacity<R>
  {
    container.append_from_capacity(n);
  }
};

FLAT_HASH_NAMESPACE_END
