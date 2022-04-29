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

#define FLAT_HASH_NAMESPACE_BEGIN \
  namespace flat_hash {           \
  inline namespace v0 {
#define FLAT_HASH_NAMESPACE_END \
  }                             \
  }

#if defined(_MSC_VER)
#  define FLAT_HASH_ALWAYS_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#  define FLAT_HASH_ALWAYS_INLINE inline __attribute__((always_inline))
#else
#  define FLAT_HASH_ALWAYS_INLINE inline
#endif

#if defined(_MSC_VER)
#  define FLAT_HASH_NOINLINE __declspec(noinline)
#elif defined(__GNUC__) || defined(__clang__)
#  define FLAT_HASH_NOINLINE __attribute__((noinline))
#else
#  define FLAT_HASH_NOINLINE
#endif

// https://devblogs.microsoft.com/cppblog/msvc-cpp20-and-the-std-cpp20-switch/
#if _MSC_VER >= 1929  // VS2019 v16.10 and later (_MSC_FULL_VER >= 192829913 for VS 2019 v16.9)
// Works with /std:c++14 and /std:c++17, and performs optimization
#  define FLAT_HASH_NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
#else
// no-op in MSVC v14x ABI
#  define FLAT_HASH_NO_UNIQUE_ADDRESS [[no_unique_address]]
#endif

#ifdef _MSC_VER
#  define FLAT_HASH_ASSUME(cond) __assume(cond)
#elif defined(__clang__)
#  define FLAT_HASH_ASSUME(cond) __builtin_assume(cond)
#elif defined(__GNUC__)
#  define FLAT_HASH_ASSUME(cond)                             \
    {                                                        \
      if (!(cond)) [[unlikely]] { __builtin_unreachable(); } \
    }
#else
#  define FLAT_HASH_ASSUME(cond)
#endif

#ifdef _MSC_VER
#  define FLAT_HASH_UNREACHABLE() __assume(0)
#else
#  define FLAT_HASH_UNREACHABLE() __builtin_unreachable()
#endif
