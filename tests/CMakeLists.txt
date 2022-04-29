cmake_minimum_required(VERSION 3.20)

project(flat_hash_tests LANGUAGES CXX)

set(test_sources src/detail_tests.cpp src/set_tests.cpp)

add_executable(flat_hash_tests ${test_sources})

include(../cmake/CompilerWarnings.cmake)
set_project_warnings(flat_hash_tests PRIVATE true)

if(FLAT_HASH_ENABLE_CODE_COVERAGE)
  message(STATUS "Code coverage enabled")
  target_compile_options(flat_hash_tests PUBLIC -O0 -g -fprofile-arcs -ftest-coverage)
  target_link_options(flat_hash_tests PUBLIC -fprofile-arcs -ftest-coverage)
endif()

find_package(Catch2 3 CONFIG REQUIRED)
target_link_libraries(flat_hash_tests PUBLIC Catch2::Catch2 Catch2::Catch2WithMain flat_hash::flat_hash)
target_compile_definitions(flat_hash_tests PRIVATE FLAT_HASH_ENABLE_ASSERTIONS)

if(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  target_compile_options(flat_hash_tests PRIVATE "-fconcepts-diagnostics-depth=4")
endif()

if(MSVC)
  target_compile_options(flat_hash_tests PRIVATE "/bigobj")
endif()

add_test(NAME flat_hash_tests COMMAND flat_hash_tests)
