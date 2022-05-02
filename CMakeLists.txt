cmake_minimum_required(VERSION 3.20)

# Generate compile_commands.json for clang based tools
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(cmake/vcpkg.cmake)

vcpkg_feature(FLAT_HASH_BUILD_TESTS "Build tests" OFF "test")
vcpkg_feature(FLAT_HASH_USE_FMTLIB "Use fmtlib instead of <format>" OFF "fmt")

if(CMAKE_BUILD_TYPE MATCHES "Debug")
  set(_enable_assertions TRUE)
else()
  set(_enable_assertions FALSE)
endif()

option(FLAT_HASH_ENABLE_ASSERTIONS "Enable assertions" ${_enable_assertions})

option(FLAT_HASH_ENABLE_DOXYGEN "Enable Doxygen documentation builds of source." OFF)

include(CMakeDependentOption)
cmake_dependent_option(FLAT_HASH_ENABLE_CODE_COVERAGE "Enable code coverage through GCC." OFF "FLAT_HASH_BUILD_TESTS"
                       OFF)

project(flat_hash VERSION 0.0.1 LANGUAGES CXX)

message(STATUS "Started CMake for ${PROJECT_NAME} v${PROJECT_VERSION}...\n")

#
# Prevent building in the source directory
#

if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(
    FATAL_ERROR
      "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there.\n"
  )
endif()

add_library(flat_hash_base INTERFACE)

if(MSVC)
  target_sources(flat_hash_base INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/tools/flat_hash.natvis>
                                          $<INSTALL_INTERFACE:include/flat_hash/flat_hash.natvis>)
endif()

target_compile_features(flat_hash_base INTERFACE cxx_std_20)

target_compile_definitions(flat_hash_base
                           INTERFACE $<$<BOOL:${FLAT_HASH_ENABLE_ASSERTIONS}>:FLAT_HASH_ENABLE_ASSERTIONS>)

target_include_directories(flat_hash_base INTERFACE $<INSTALL_INTERFACE:include>
                                                    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

if(MSVC)
  target_compile_options(flat_hash_base INTERFACE "/Zc:preprocessor;/Zc:__cplusplus")
endif()

add_library(flat_hash INTERFACE)
target_link_libraries(flat_hash INTERFACE flat_hash_base)

if(MSVC)
  include(cmake/utils.cmake)
  set(CMAKE_REQUIRED_FLAGS "/std:c++20")
  check_cxx_feature_macro(__cpp_lib_ranges 202100 "ranges" HAVE_RANGES)
  if(NOT FLAT_HASH_USE_FMTLIB)
    check_cxx_feature_macro(__cpp_lib_format 202100 "format" HAVE_FORMAT)
  else()
    set(HAVE_FORMAT TRUE CACHE BOOL "")
  endif()

  # See https://github.com/microsoft/STL/issues/1814
  if(NOT HAVE_RANGES OR NOT HAVE_FORMAT)
    target_compile_features(flat_hash INTERFACE cxx_std_23)
  endif()
endif()

add_library(flat_hash::flat_hash ALIAS flat_hash)

if(FLAT_HASH_USE_FMTLIB)
  find_package(fmt 8 CONFIG REQUIRED)
else()
  find_package(fmt 8 CONFIG QUIET)
endif()

if(fmt_FOUND)
  include(cmake/flat_hashMacros.cmake)
  add_fmt_lib(flat_hash-fmt fmt::fmt)
endif()

include(cmake/CPM.cmake)
cpmaddpackage(
  NAME
  Format.cmake
  VERSION
  1.7.3
  GITHUB_REPOSITORY
  TheLartians/Format.cmake
  OPTIONS
  # set to yes skip cmake formatting
  "FORMAT_SKIP_CMAKE NO"
  # path to exclude (optional, supports regular expressions)
  "CMAKE_FORMAT_EXCLUDE cmake/CPM.cmake|portfile.cmake|CodeCoverage.cmake")

if(FLAT_HASH_ENABLE_DOXYGEN)
  set(DOXYGEN_CALLER_GRAPH YES)
  set(DOXYGEN_CALL_GRAPH YES)
  set(DOXYGEN_EXTRACT_ALL YES)
  set(DOXYGEN_GENERATE_XML YES)
  set(DOXYGEN_OUTPUT_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/docs/_build)

  find_package(Doxygen REQUIRED dot)
  doxygen_add_docs(doxygen-docs ${PROJECT_SOURCE_DIR}/include)
endif()

include(GNUInstallDirs)

# let install without building tests since this is only an interface library
set(CMAKE_SKIP_INSTALL_ALL_DEPENDENCY TRUE)

install(
  TARGETS flat_hash_base
  EXPORT flat_hash_baseTargets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  INCLUDES
  DESTINATION include
  PUBLIC_HEADER DESTINATION include)
install(TARGETS flat_hash EXPORT flat_hashTargets)

install(EXPORT flat_hash_baseTargets FILE flat_hash_baseTargets.cmake
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/flat_hash)
install(EXPORT flat_hashTargets FILE flat_hashTargets.cmake NAMESPACE flat_hash::
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/flat_hash)

configure_file(${CMAKE_CURRENT_LIST_DIR}/cmake/version.hpp.in include/flat_hash/version.hpp @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/flat_hash/version.hpp DESTINATION include/flat_hash)
if(MSVC)
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/tools/flat_hash.natvis DESTINATION include/flat_hash)
endif()

install(DIRECTORY include/flat_hash DESTINATION include)

include(CMakePackageConfigHelpers)
write_basic_package_version_file(flat_hashConfigVersion.cmake VERSION ${PROJECT_VERSION} COMPATIBILITY SameMajorVersion)

configure_package_config_file(
  ${CMAKE_CURRENT_LIST_DIR}/cmake/flat_hashConfig.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/flat_hashConfig.cmake
  INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/flat_hash)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/flat_hashConfig.cmake ${CMAKE_CURRENT_BINARY_DIR}/flat_hashConfigVersion.cmake
              ${CMAKE_CURRENT_LIST_DIR}/cmake/flat_hashMacros.cmake DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/flat_hash)

if(FLAT_HASH_BUILD_TESTS)
  enable_testing()
  add_subdirectory(tests)
endif()
