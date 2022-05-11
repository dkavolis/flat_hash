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

#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>

#include "detail/macros.hpp"
#include "dictionary_fwd.hpp"
#include "set_fwd.hpp"

// there are no forward declarations/cannot forward declare standard containers so these are in a separate file to avoid
// increasing compile times

template <class Key, class Compare, class Allocator>
inline constexpr bool flat_hash::is_unique_range<std::set<Key, Compare, Allocator>> = true;

template <class Key, class Hash, class Compare, class Allocator>
inline constexpr bool flat_hash::is_unique_range<std::unordered_set<Key, Hash, Compare, Allocator>> = true;

template <class Key, class T, class Compare, class Allocator>
inline constexpr bool flat_hash::is_unique_map<std::map<Key, T, Compare, Allocator>> = true;

template <class Key, class T, class Hash, class Compare, class Allocator>
inline constexpr bool flat_hash::is_unique_map<std::unordered_map<Key, T, Hash, Compare, Allocator>> = true;
