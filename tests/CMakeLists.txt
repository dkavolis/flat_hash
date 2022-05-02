cmake_minimum_required(VERSION 3.20)

project(flat_hash_tests LANGUAGES CXX)

set(test_sources src/detail_tests.cpp src/set_tests.cpp)
include(../cmake/CompilerWarnings.cmake)

find_package(Catch2 3 CONFIG REQUIRED)

macro(add_tests NAME_ lib)
  add_executable(${NAME_} ${test_sources})
  set_project_warnings(${NAME_} PRIVATE true)

  target_link_libraries(${NAME_} PUBLIC Catch2::Catch2 Catch2::Catch2WithMain ${lib})
  target_compile_definitions(${NAME_} PRIVATE FLAT_HASH_ENABLE_ASSERTIONS)

  if(CMAKE_COMPILER_IS_GNUCXX)
    target_compile_options(${NAME_} PRIVATE "-fconcepts-diagnostics-depth=4")
  endif()

  if(MSVC)
    target_compile_options(${NAME_} PRIVATE "/bigobj")
  endif()

  add_test(NAME ${NAME_} COMMAND ${NAME_})
endmacro()

add_tests(flat_hash_tests flat_hash::flat_hash)
set(coverage_target flat_hash_tests)

if(fmt_FOUND)
  add_tests(flat_hash_tests-fmt flat_hash::flat_hash-fmt)
  set(coverage_target flat_hash_tests-fmt)
endif()

if(FLAT_HASH_ENABLE_CODE_COVERAGE)
  include(${CMAKE_SOURCE_DIR}/cmake/utils.cmake)
  enable_coverage(flat_hash_coverage ${coverage_target})
endif()
