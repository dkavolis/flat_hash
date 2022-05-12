#!/usr/bin/env python3
# -*- coding:utf-8 -*-

# pyright: reportConstantRedefinition=none

import re
import subprocess
import sys
import pathlib
from typing import Iterable, Union
from multiprocessing import cpu_count

if sys.platform.startswith("linux"):
    CLANG_FORMAT = ["/usr/bin/clang-format"]
    IWYU = ["iwyu_tool"]
else:
    # windows has 14 at most, need 15 for concepts
    CLANG_FORMAT = ["wsl", "clang-format"]
    IWYU = ["wsl", "iwyu_tool"]

FILE_PATH = pathlib.Path(__file__).parent.absolute()
CPP_DIRS = (
    "../benchmarks",
    "../include",
    "../examples",
    "../src",
    "../tests",
    "../bindings",
)
CMAKE_DIRS = CPP_DIRS + ("../cmake", ".")
CLANG_GLOB = ("**/*.hpp", "**/*.h", "**/*.cpp")
CMAKE_GLOB = ("**/CMakeLists.txt", "**/*.cmake")
CMAKE_GLOB_IGNORE = ("**/portfile.cmake", "CodeCoverage.cmake")

WSL_PATH_REGEX = re.compile(r"^([\w]):[/\\]")


def fix_path(path: pathlib.Path) -> str:
    if sys.platform.startswith("win"):

        def replace(match: re.Match[str]) -> str:
            return f"/mnt/{match.group(1).lower()}/"

        return WSL_PATH_REGEX.sub(replace, str(path), 1).replace("\\", "/")

    return str(path)


def find_files(
    directories: list[pathlib.Path], patterns: Iterable[str]
) -> set[pathlib.Path]:
    all_files: set[pathlib.Path] = set()
    try:
        for directory in directories:
            for pattern in patterns:
                for file in directory.glob(f"{pattern}"):
                    all_files.add(file)
    finally:
        return all_files


def cpp_files() -> set[pathlib.Path]:
    cpp_dirs = [FILE_PATH / d for d in CPP_DIRS]
    return find_files(cpp_dirs, CLANG_GLOB)


def cmake_files() -> set[pathlib.Path]:
    cmake_dirs = [FILE_PATH / d for d in CMAKE_DIRS]
    return find_files(cmake_dirs, CMAKE_GLOB) - find_files(
        cmake_dirs, CMAKE_GLOB_IGNORE
    )


def clang_format(files: Iterable[pathlib.Path]) -> None:
    subprocess.check_call(CLANG_FORMAT + ["-i"] + [fix_path(file) for file in files])


def cmake_format(files: Iterable[pathlib.Path]) -> None:
    subprocess.check_call(
        [sys.executable, "-m", "cmakelang.format", "-i"]
        + [fix_path(file) for file in files]
    )


def iwyu(bin_dir: Union[str, pathlib.Path]) -> None:
    cdb = (pathlib.Path(bin_dir) / "compile_commands.json").absolute()
    if not cdb.exists():
        raise ValueError(f"Compilation database {cdb} doesn't exist")
    subprocess.check_call(
        IWYU
        + [
            "-o",
            "iwyu",
            "-j",
            str(cpu_count()),
            "-p",
            fix_path(cdb),
            "--",
            "-Xiwyu",
            "any",
            "-Xiwyu",
            "iwyu",
            "-Xiwyu",
            "args",
            "-Wno-unknown-warning-option",
        ]
    )
