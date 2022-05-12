#!/usr/bin/env python3
# -*- coding:utf-8 -*-

# pyright: reportMissingParameterType=none

import argparse
import sys
from typing import Any, Sequence, Union
import utils


class CppFormatAction(argparse.Action):
    def __init__(
        self,
        option_strings: Sequence[str],
        dest: str,
        nargs: None = None,
        **kwargs: Any,
    ):
        if nargs is not None:
            raise ValueError("nargs not allowed")
        super().__init__(option_strings, dest, nargs=0, **kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        print("Formatting C++ files...")
        utils.clang_format(utils.cpp_files())


class CmakeFormatAction(argparse.Action):
    def __init__(
        self,
        option_strings: Sequence[str],
        dest: str,
        nargs: None = None,
        **kwargs: Any,
    ):
        if nargs is not None:
            raise ValueError("nargs not allowed")
        super().__init__(option_strings, dest, nargs=0, **kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        print("Formatting CMake files")
        utils.cmake_format(utils.cmake_files())


class IwyuAction(argparse.Action):
    def __init__(
        self,
        option_strings: Sequence[str],
        dest: str,
        nargs: None = None,
        **kwargs: Any,
    ):
        if nargs is not None:
            raise ValueError("nargs not allowed")
        super().__init__(option_strings, nargs=1, type=str, **kwargs, dest="binary_dir")

    def __call__(
        self,
        parser,
        namespace,
        values: Union[str, Sequence[Any], None],
        option_string=None,
    ):
        path: str
        if isinstance(values, str):
            path = values
        elif values is None:
            raise TypeError("binary dir path cannot be None")
        else:
            path = values[0]

        print(f"Running IWYU for {path}")
        utils.iwyu(path)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--cpp", action=CppFormatAction, help="Format C++ files using clang-format"
    )
    parser.add_argument(
        "--cmake",
        action=CmakeFormatAction,
        help="Format CMake files using cmake-format",
    )
    parser.add_argument(
        "--iwyu", action=IwyuAction, help="Run include-what-you-use on C++ files"
    )

    parser.parse_args()


if __name__ == "__main__":
    sys.exit(main())
