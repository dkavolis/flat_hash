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

#if !defined(FLAT_HASH_USE_FMTLIB) && !__has_include(<format>)
#  error "C++20 <format> is required but is not found in standard library, consider using fmt"
#endif

#if defined(FLAT_HASH_USE_FMTLIB)
#  define FLAT_HASH_FORMAT_NS fmt::
#else
#  define FLAT_HASH_FORMAT_NS std::
#endif

#include <iterator>
#include <string_view>
#include <utility>

#if defined(FLAT_HASH_USE_FMTLIB)
#  if defined(_MSC_VER)
#    pragma warning(push)
#    pragma warning(disable : 4189)
#  endif
#  include <fmt/format.h>
#  if defined(_MSC_VER)
#    pragma warning(pop)
#  endif
#else
#  include <format>  // for std::format_to, std::make_format_args
#endif

#include "macros.hpp"

FLAT_HASH_NAMESPACE_BEGIN

namespace detail {

template <class Fn, class R, class... Args>
concept invocable_r = std::is_invocable_r<R, Fn, Args...>::value;

template <class T, class Context>
concept has_formatter = requires(T const& val, Context& ctx) {
                          std::declval<typename Context::template formatter_type<T>>().format(val, ctx);
                        };

#if defined(FLAT_HASH_USE_FMTLIB)
template <class... Args>
using format_string = fmt::format_string<Args...>;

template <class T, class Context>
concept has_fallback_formatter =
    requires(T const& val, Context& ctx) {
      std::declval<fmt::detail::fallback_formatter<T, typename Context::char_type>>().format(val, ctx);
    };

template <class T, class Char = char>
concept formattable_impl = (has_formatter<T, fmt::format_context> || has_fallback_formatter<T, fmt::format_context>);

template <class T, class Char = char>
concept formattable = ((!std::ranges::range<T> && formattable_impl<T, Char>) ||
                       // ranges are not formattable if values are not formattable
                       (std::ranges::range<T> && formattable_impl<std::ranges::range_value_t<T>, Char>));

#else
// even if std implements compile time format string checking, the type itself is for exposition only... can't do
// compile time format string checking without referencing hidden types (╯°□°）╯︵ ┻━┻
template <class... Args>
using format_string = std::string_view;

template <class T, class Char = char>
concept formattable = std::is_fundamental_v<std::remove_cvref_t<T>> || has_formatter<T, std::format_context>;
#endif

// LCOV_EXCL_START
template <class... Args>
[[nodiscard]] auto decay_format_string(format_string<Args...> format) noexcept -> std::string_view {
#ifdef FLAT_HASH_USE_FMTLIB
  fmt::string_view str = format;
  return {str.data(), str.size()};
#else
  return format;
#endif
}
// LCOV_EXCL_STOP

template <class Char, std::output_iterator<Char> It>
constexpr auto write_to(It iterator, std::string_view str) -> It {
  for (char c : str) {
    *iterator = Char{c};
    ++iterator;
  }

  return iterator;
}

struct separators {
  std::string_view prefix;
  std::string_view separator;
  std::string_view postfix;
};

struct range_format_options {
  bool new_lines = false;
};

[[nodiscard]] constexpr auto get_set_separators(range_format_options options) noexcept -> separators {
  if (options.new_lines) {
    return {
        .prefix = "{\n\t",
        .separator = ",\n\t",
        .postfix = ",\n}",
    };
  }

  return {
      .prefix = "{",
      .separator = ", ",
      .postfix = "}",
  };
}

[[nodiscard]] constexpr auto get_list_separators(range_format_options options) noexcept -> separators {
  if (options.new_lines) {
    return {
        .prefix = "[\n\t",
        .separator = ",\n\t",
        .postfix = ",\n]",
    };
  }

  return {
      .prefix = "[",
      .separator = ", ",
      .postfix = "]",
  };
}

// std::format doesn't have on_error member like fmt does
#ifdef FLAT_HASH_USE_FMTLIB
#  define FLAT_HASH_ON_FORMAT_ERROR(ctx, ...) ctx.on_error(__VA_ARGS__)
#else
#  define FLAT_HASH_ON_FORMAT_ERROR(ctx, ...) throw std::format_error(__VA_ARGS__)
#endif

template <class... Args>
[[nodiscard]] constexpr auto parse_range_format(FLAT_HASH_FORMAT_NS basic_format_parse_context<Args...>& ctx)
    -> std::pair<decltype(ctx.begin()), range_format_options> {
  range_format_options options;
  auto it = ctx.begin();
  auto const end = ctx.end();

  using Char = std::iter_value_t<decltype(it)>;
  if (it == end) { return {it, options}; }

  // the formatting errors here would be caught at compile time so exclude them from coverage

  if (*it == Char{'l'}) {
    options.new_lines = true;
    ++it;
    // LCOV_EXCL_START
    if (it == end) [[unlikely]] { FLAT_HASH_ON_FORMAT_ERROR(ctx, "Unterminated format string!"); }
    // LCOV_EXCL_STOP
  }

  // LCOV_EXCL_BR_START
  switch (*it) {
    case Char{':'}: {
      // switching to a subformatter
      ++it;
      break;
    }
    case Char{'}'}: {
      // end of the format string
      break;
    }
      // LCOV_EXCL_START
    default:
      [[unlikely]] {
        FLAT_HASH_ON_FORMAT_ERROR(ctx, "invalid format specifier!");
        break;
      }
      // LCOV_EXCL_STOP
  }
  // LCOV_EXCL_BR_STOP

  return {it, options};
}

template <class Char, std::ranges::sized_range R, class OutputIt,
          invocable_r<OutputIt, std::ranges::range_reference_t<R const>,
                      FLAT_HASH_FORMAT_NS basic_format_context<OutputIt, Char>&>
              FormatFn>
[[nodiscard]] constexpr auto format_range(R const& r, FLAT_HASH_FORMAT_NS basic_format_context<OutputIt, Char>& context,
                                          separators const& strings, FormatFn&& format) -> OutputIt {
  using reference_type = std::ranges::range_reference_t<R>;
  constexpr bool string_like = std::convertible_to<reference_type, std::basic_string_view<Char>>;
  auto it = write_to<Char>(context.out(), strings.prefix);

  std::int64_t i = std::ranges::ssize(r);
  for (auto&& value : r) {
    if constexpr (string_like) { *it++ = Char{'"'}; }
    context.advance_to(it);
    it = format(value, context);
    if constexpr (string_like) { *it++ = Char{'"'}; }

    --i;

    if (i != 0) [[likely]] { it = write_to<Char>(it, strings.separator); }
  }

  return write_to<Char>(it, strings.postfix);
}

template <class Char>
constexpr inline auto unformattable_str = std::to_array<Char>({
    Char{'{'},
    Char{'?'},
    Char{'}'},
});

template <class Char = char, class T>
[[nodiscard]] constexpr auto maybe_format_arg(T&& value) noexcept -> decltype(auto) {
  if constexpr (formattable<T, Char>) {
    return std::forward<T>(value);
  } else {
    return std::basic_string_view<Char>(unformattable_str<Char>.data(), unformattable_str<Char>.size());
  }
}

}  // namespace detail

FLAT_HASH_NAMESPACE_END
