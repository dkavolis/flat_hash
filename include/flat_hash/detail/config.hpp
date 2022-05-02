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

#include "macros.hpp"

#if defined(FLAT_HASH_ENABLE_ASSERTIONS)
#  include <cstdio>     // for stderr
#  include <exception>  // for std::terminate
#  if !defined(FLAT_HASH_USE_FMTLIB)
#    include <array>            // for std::array
#    include <iterator>         // for std::back_insert_iterator
#    include <memory_resource>  // for std::pmr::monotonic_buffer_resource
#    include <string>           // for std::string
#  endif
#  include <functional>  // for std::function
#endif

#include <concepts>  // for std::invocable
#include <numeric>
#include <source_location>  // for std::source_location
#include <string_view>      // for std::string_view

#include "format.hpp"

#define ITERATOR_FACADE_NS flat_hash
#include "iterator_facade.hpp"
#undef ITERATOR_FACADE_NS

FLAT_HASH_NAMESPACE_BEGIN

template <std::unsigned_integral T>
constexpr static T empty_bucket_v = std::numeric_limits<T>::max();
template <std::unsigned_integral T>
constexpr static T tombstone_v = empty_bucket_v<T> - 1;

namespace detail {

// LCOV_EXCL_START
#if defined(FLAT_HASH_ENABLE_ASSERTIONS)

inline auto vprint_to(FILE* stream, std::string_view format, FLAT_HASH_FORMAT_NS format_args args) -> std::size_t {
#  if defined(FLAT_HASH_USE_FMTLIB)
  fmt::basic_memory_buffer<char, 1024> str;
  // fmt can use appender to extract the underlying buffer
  fmt::appender out{str};
#  else
  // imitate fmt::basic_memory_buffer with pmr string and a monotonic buffer
  std::array<char, 1024> buffer;
  std::pmr::monotonic_buffer_resource resource{&buffer, sizeof(buffer)};
  std::pmr::string str{&resource};
  str.reserve(sizeof(buffer));
  std::back_insert_iterator out{str};
#  endif
  FLAT_HASH_FORMAT_NS vformat_to(out, format, args);

  // don't use iostreams
  return std::fwrite(str.data(), 1, str.size(), stream);
}

template <class... Args>
inline auto print_to(FILE* stream, format_string<Args...> format, Args&&... args) -> std::size_t {
  return vprint_to(stream, decay_format_string<Args...>(format),
                   FLAT_HASH_FORMAT_NS make_format_args(std::forward<Args>(args)...));
}

FLAT_HASH_NOINLINE inline void default_assertion_handler(std::source_location src, std::string_view format,
                                                         FLAT_HASH_FORMAT_NS format_args args) noexcept {
  print_to(stderr, "Assertion failed in {:s} at {:s}({:d}) with:\n\t", src.function_name(), src.file_name(),
           src.line());
  vprint_to(stderr, format, args);
  std::fputc('\n', stderr);

  (void)std::fflush(stderr);
}

[[nodiscard]] inline auto assertion_handler() noexcept
    -> std::function<void(std::source_location, std::string_view, FLAT_HASH_FORMAT_NS format_args)>& {
  // this should not allocate on first initialization
  static std::function<void(std::source_location, std::string_view, FLAT_HASH_FORMAT_NS format_args)> handler_ =
      default_assertion_handler;
  return handler_;
}

[[noreturn]] FLAT_HASH_NOINLINE inline void vassertion_failed(std::source_location src, std::string_view format,
                                                              FLAT_HASH_FORMAT_NS format_args args) noexcept {
  // don't care about exceptions, terminating anyway
  // this allows to ignore assertions in marking functions noexcept
  assertion_handler()(src, format, std::move(args));
  std::terminate();
}

template <class... Args>
[[noreturn]] void assertion_failed(std::source_location src, format_string<Args...> format, Args&&... args) noexcept {
  vassertion_failed(src, decay_format_string<Args...>(format),
                    FLAT_HASH_FORMAT_NS make_format_args(std::forward<Args>(args)...));
}

#  define FLAT_HASH_ASSERT(condition, ...)                                                      \
    {                                                                                           \
      if (!(condition)) [[unlikely]] {                                                          \
        ::flat_hash::detail::assertion_failed(std::source_location::current(), "" __VA_ARGS__); \
      }                                                                                         \
    }
#else
#  define FLAT_HASH_ASSERT(condition, ...) FLAT_HASH_ASSUME(condition)
#endif

}  // namespace detail

template <std::invocable<std::source_location, std::string_view, FLAT_HASH_FORMAT_NS format_args> T>
void set_assertion_handler(T&& handler [[maybe_unused]]) {
#if defined(FLAT_HASH_ENABLE_ASSERTIONS)
  detail::assertion_handler() = std::forward<T>(handler);
#endif
}

inline void reset_assertion_handler() noexcept {
#if defined(FLAT_HASH_ENABLE_ASSERTIONS)
  detail::assertion_handler() = detail::default_assertion_handler;
#endif
}
// LCOV_EXCL_STOP

FLAT_HASH_NAMESPACE_END
