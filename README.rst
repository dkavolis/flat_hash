===============
flat_hash
===============

.. image:: https://github.com/dkavolis/flat_hash/actions/workflows/msvc.yml/badge.svg
        :target: https://github.com/dkavolis/flat_hash/actions/workflows/msvc.yml/badge.svg
        :alt: Github Workflow MSVC Build flat_hash Status

.. image:: https://github.com/dkavolis/flat_hash/actions/workflows/gcc.yml/badge.svg
        :target: https://github.com/dkavolis/flat_hash/actions/workflows/gcc.yml/badge.svg
        :alt: Github Workflow GCC Build flat_hash Status

.. image:: https://github.com/dkavolis/flat_hash/actions/workflows/clang.yml/badge.svg
        :target: https://github.com/dkavolis/flat_hash/actions/workflows/clang.yml/badge.svg
        :alt: Github Workflow Clang Build flat_hash Status

.. image:: https://readthedocs.org/projects/flat_hash/badge/?version=latest
        :target: https://flat_hash.readthedocs.io/en/latest/?badge=latest
        :alt: Documentation Status


Customizable flat associative containers for C++20


* Free software: MIT
* Documentation: https://flat_hash.readthedocs.io.
* Clang is broken until `concepts bug`_ is fixed


Features
--------

* Indirect open addressing
* Space efficient, only uses as much memory as there is reserved for stored values + 4 bytes per bucket (configurable)
* Configurable probing schemes
* Insertion order can be preserved at the cost of slower arbitrary insertions and deletions
* Similar API to ``std::unordered_set``
* Iteration performance only depends on the underlying container
* ``constexpr`` enabled
* Heterogeneous lookup enabled if both the hasher and equality comparator
  define ``is_transparent`` types, enabled for integrals, string types and smart pointers by default
* Works as an adapter to any random access ranges
* Set comparison operators:
   *  equality: ``==``, ``!=``
   *  subset: ``<=``, ``>=``
   *  proper subset: ``<``, ``>``
* Similar to LLVM ``StringMap``
* Provides fully dynamic, small and static sets, all convertible to a provided view type
* ``<format>`` and ``fmtlib`` support

TODO
----

* Implement set operations: union, intersection, difference, symmetric_difference
* Implement dictionary
* Documentation
* Enable hash caching
* Benchmarks
* Death tests when Catch supports them

.. _concepts bug: https://github.com/llvm/llvm-project/issues/44178
