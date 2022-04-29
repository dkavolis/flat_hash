#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import gdb
import gdb.printing
from typing import (
    Any,
    Callable,
    Iterator,
    Optional,
    Protocol,
    Tuple,
    Type,
    runtime_checkable,
)

GdbObject = Any


@runtime_checkable
class PrettyPrinter(Protocol):
    def __init__(self, typename: str, val: GdbObject):
        ...

    def to_string(self) -> str:
        ...

    def display_hint(self) -> str:
        ...

    def children(self) -> Optional[Iterator[Tuple[str, GdbObject]]]:
        ...


_printer: Any = gdb.printing.RegexpCollectionPrettyPrinter("flat_hash")


def add_printer(base_name: str) -> Callable[[Type[PrettyPrinter]], Type[PrettyPrinter]]:
    def _register(cls: Type[PrettyPrinter]) -> Type[PrettyPrinter]:
        index = base_name.rfind(":")
        if index != -1:
            name = base_name[index + 1 :]
        else:
            name = base_name
        _printer.add_printer(name, f"^flat_hash::v0::{base_name}", cls)
        return cls

    return _register


class PointerIterator(Iterator[Tuple[str, GdbObject]]):
    def __init__(self, begin: GdbObject, size: int):
        self.count = 0
        self.begin = begin
        self.size = size

    def __iter__(self):
        return self

    def __next__(self) -> Tuple[str, GdbObject]:
        if self.count == self.size:
            raise StopIteration

        count = self.count
        self.count = self.count + 1
        return f"[{count}]", (self.begin + count).dereference()


@add_printer("detail::inline_vector")
class InlineVectorPrinter:
    "Print a flat_hash::detail::inline_vector"

    def __init__(self, typename: str, val: GdbObject):
        self.typename: str = typename
        self.capacity: int = val.type.template_argument(1)
        self.size: int = val["count_"]
        self.val = val
        self.child_type: str = val.type.template_arguments(0)

    def to_string(self):
        return f"{self.typename} of length {self.size}"

    def children(self):
        return PointerIterator(
            self.val["storage_"].cast(f"{self.child_type}*"), self.size
        )

    def display_hint(self):
        return "array"


gdb.printing.register_pretty_printer(gdb.current_objfile(), _printer)
