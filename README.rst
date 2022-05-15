===============
[WIP] flat_hash
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


.. image:: https://codecov.io/gh/dkavolis/flat_hash/branch/master/graph/badge.svg?token=P425OBVMLW
        :target: https://codecov.io/gh/dkavolis/flat_hash
        :alt: Coverage

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
* Configurable probing schemes:

    * ``probing::quadratic`` [default]
    * ``probing::python``
    * ``probing::robin_hood``

* Insertion order can be preserved at the cost of slower arbitrary insertions and deletions

    * ``ordering_policy::preserved`` [default]
    * ``ordering_policy::relaxed``

* Similar API to ``std::unordered_set`` and ``std::unordered_map``
* Iteration performance only depends on the underlying containers
* ``constexpr`` enabled
* Heterogeneous lookup enabled if both the hasher and equality comparator
  define ``is_transparent`` types, enabled for integrals, string types and smart pointers by default
* Works as an adapter to any random access ranges
* Set comparison operators:

    :equality:      ``==``, ``!=``
    :subset:        ``<=``, ``>=``
    :proper subset: ``<``, ``>``

* Provides fully dynamic, inline and static sets and dictionaries, all convertible to a provided view type
* Direct access to key and value views in dictionaries through
  ``dictionary::keys()`` and ``dictionary::values()`` with ``keys()`` returning
  a set view.
* ``default_dict`` is possible with ``dictionary_traits::on_missing_key(key
  const&)`` returning a mapped value.
* ``<format>`` and ``fmtlib`` support
* Custom formatting of sets:

    :``format_spec``: ``[lines][:[key_spec]]``
    :``lines``:       ``"l"``
    :``key_spec``:    key format specification

    where ``lines`` adds a new line and tab characters in between items.
* Custom formatting of dictionaries:

    :``format_spec``: ``[lines][:[{key_spec}[separator]][{value_spec}]]``
    :``lines``:       ``"l"``
    :``key_spec``:    key format specification
    :``separator``:   <string of characters other than ``'{'`` or ``'}'``>, default is ``": "``
    :``value_spec``:  mapped value format specification

    If no key or value format specification is provided, default is
    assumed. ``separator`` can also appear by itself such as
    ``[lines][:[separator]]``.

Usage
-----

The package provides CMake targets

.. code-block:: cmake

    # Using with <format>:
    find_package(flat_hash CONFIG REQUIRED)
    target_link_libraries(main PRIVATE flat_hash::flat_hash)

    # Or using with fmtlib:
    find_package(flat_hash CONFIG REQUIRED COMPONENTS fmt)
    target_link_libraries(main PRIVATE flat_hash::flat_hash-fmt)

    # Or using the header-only fmt version
    target_link_libraries(main PRIVATE flat_hash::flat_hash-fmt-header-only)

TODO
----

* Implement set operations: union, intersection, difference, symmetric_difference
* Documentation
* Enable hash caching
* Benchmarks
* Death tests when Catch supports them

.. _concepts bug: https://github.com/llvm/llvm-project/issues/44178
