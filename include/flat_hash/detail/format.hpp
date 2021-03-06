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

template <class T, std::size_t N, class F>
consteval void for_each_type(F&& function) {
  auto expander = [&function]<std::size_t... I>(std::index_sequence<I...>) {
    ((void)function(std::declval<std::tuple_element_t<I, std::remove_cvref_t<T>>>()), ...);
  };

  expander(std::make_index_sequence<N>{});
}

template <class T, class Pred>
[[nodiscard]] consteval auto all_of_types(Pred pred) -> bool {
  bool all = true;
  for_each_type<T>([pred, &all]<class E>(E&&) { all &= pred(std::declval<E>()); });
  return all;
}

template <class T, std::size_t N>
concept has_tuple_element = requires { typename std::tuple_element<N, T>::type; };

template <class T>
concept tuple_like = requires {
                       typename std::tuple_size<std::remove_cvref_t<T>>;
                       { std::tuple_size<std::remove_cvref_t<T>>::value } -> std::convertible_to<std::size_t>;
                     };

template <class T, class Char = char>
concept formattable_tuple = requires {
                              requires tuple_like<T>;
                              requires all_of_types<T, std::tuple_size<std::remove_cvref_t<T>>::value>(
                                  []<class E>(E&&) -> bool { return formattable_impl<std::remove_cvref_t<E>, Char>; });
                            };

template <class T, class Char = char>
concept formattable =
    requires {
      requires !std::ranges::range<T> && !tuple_like<T>;
      requires formattable_impl<T, Char>;
    } ||
    // ranges are not formattable if values are not formattable
    requires {
      requires std::ranges::range<T>;
      requires formattable_impl<std::ranges::range_value_t<T>, Char>;
    } ||
    // structured bindings are not formattable if any of their items are not formattable
    requires {
      requires tuple_like<T>;
      requires formattable_tuple<T, Char>;
    };

#else
  // clang-format off
// even if std implements compile time format string checking, the type itself is for exposition only... can't do
// compile time format string checking without referencing hidden types (??????????????????? ?????????
template <class... Args>
using format_string = std::string_view;

template <class T, class Char = char>
concept formattable = std::is_fundamental_v<std::remove_cvref_t<T>> || has_formatter<T, std::format_context>;
// clang-format on
#endif

// LCOV_EXCL_START
template <class... Args>
[[nodiscard]] auto decay_format_string(format_string<Args...> format) noexcept -> std::string_view {
#ifdef FLAT_HASH_USE_FMTLIB
  fmt::string_view str = format;
  return {str.data(), str.size()};
#else
    // clang-format off
  return format;
// clang-format on
#endif
}
// LCOV_EXCL_STOP

template <class Char = char>
struct separators {
  std::basic_string_view<Char> prefix;
  std::basic_string_view<Char> separator;
  std::basic_string_view<Char> postfix;
};

struct range_format_options {
  bool new_lines = false;
};

[[nodiscard]] constexpr auto get_set_separators(range_format_options options) noexcept -> separators<char> {
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

[[nodiscard]] constexpr auto get_list_separators(range_format_options options) noexcept -> separators<char> {
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
#  define FLAT_HASH_ON_FORMAT_ERROR(ctx, ...) FLAT_HASH_THROW(std::format_error(__VA_ARGS__))
#endif

template <class... Args>
[[nodiscard]] constexpr auto parse_range_format(FLAT_HASH_FORMAT_NS basic_format_parse_context<Args...>& ctx)
    -> std::pair<decltype(ctx.begin()), range_format_options> {
  range_format_options options;
  auto it = ctx.begin();
  auto const end = ctx.end();

  if (it == end) { return {it, options}; }

  // the formatting errors here would be caught at compile time so exclude them from coverage

  if (*it == 'l') {
    options.new_lines = true;
    ++it;
    // LCOV_EXCL_START
    if (it == end) [[unlikely]] { FLAT_HASH_ON_FORMAT_ERROR(ctx, "Unterminated format string!"); }
    // LCOV_EXCL_STOP
  }

  // LCOV_EXCL_BR_START
  switch (*it) {
    case ':': {
      // switching to a subformatter
      ++it;
      break;
    }
    case '}': {
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

template <class Value, class OutputIt, class Char, class Format>
constexpr auto write_range_item(Value const& value, FLAT_HASH_FORMAT_NS basic_format_context<OutputIt, Char>& ctx,
                                Format&& format) {
  constexpr bool string_like = std::convertible_to<Value const&, std::basic_string_view<Char>>;
  auto out = ctx.out();
  if constexpr (string_like) { *out++ = '"'; }
  ctx.advance_to(out);
  if constexpr (std::invocable<Format, Value const&, decltype((ctx))>) {
    out = format(value, ctx);
  } else {
    out = format.format(value, ctx);
  }
  if constexpr (string_like) { *out++ = '"'; }
  return out;
}

// TODO: function_ref with direct construction from a compatible formatter

template <class Char, std::ranges::sized_range R, class OutputIt,
          invocable_r<OutputIt, std::ranges::range_reference_t<R const>,
                      FLAT_HASH_FORMAT_NS basic_format_context<OutputIt, Char>&>
              FormatFn>
[[nodiscard]] constexpr auto format_range(R const& r, FLAT_HASH_FORMAT_NS basic_format_context<OutputIt, Char>& context,
                                          separators<char> const& strings, FormatFn&& format) -> OutputIt {
  auto it = std::ranges::copy(strings.prefix, context.out()).out;

  std::int64_t i = std::ranges::ssize(r);
  for (auto&& value : r) {
    context.advance_to(it);
    it = write_range_item(value, context, format);
    --i;

    if (i != 0) [[likely]] { it = std::ranges::copy(strings.separator, it).out; }
  }

  return std::ranges::copy(strings.postfix, it).out;
}

template <class Char>
constexpr inline auto unformattable_str = std::to_array<Char>({'{', '?', '}'});

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
