function(check_cxx_feature_macro MACRO VALUE HEADER VARIABLE)
  include(CheckCXXSourceCompiles)
  check_cxx_source_compiles(
    "#include <${HEADER}>
static_assert(${MACRO} > ${VALUE}L);
int main() {}" ${VARIABLE})
endfunction()

function(enable_coverage NAME_ TARGET)
  if(NOT CMAKE_COMPILER_IS_GNUCXX)
    message(FATAL_ERROR "Cannot enable coverage for ${CMAKE_CXX_COMPILER_ID}, GCC is required")
  endif()

  string(REPLACE "." ";" version ${CMAKE_CXX_COMPILER_VERSION})
  list(GET version 0 gcc_major)
  get_filename_component(PATH ${CMAKE_CXX_COMPILER} DIRECTORY)
  find_program(GCOV NAMES gcov-${gcc_major} gcov HINTS ${path} PATHS ENV GCOV)
  find_program(GCOVR NAMES gcovr PATHS ~/.local/bin /usr/bin ${CMAKE_SOURCE_DIR}/scripts/test ENV GCOVR)
  message(STATUS "GCOVR = ${GCOVR}")

  if(GCOV)
    message(STATUS "Code coverage enabled")

    include(${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CodeCoverage.cmake)
    set(GCOVR_PATH ${GCOVR} CACHE "FILEPATH" "Path to gcovr" FORCE)
    append_coverage_compiler_flags_to_target(${TARGET})
    # no-exceptions to get rid of exception handling branches
    target_compile_options(
      ${TARGET}
      PUBLIC -O0
             -g
             -fno-inline
             -fno-inline-small-functions
             -fno-default-inline
             -fno-exceptions
             --coverage)
    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 12)
      # no need to generate info for trivial inline functions
      target_compile_options(${TARGET} PUBLIC -ffold-simple-inlines)
    endif()

    target_compile_definitions(${TARGET} PUBLIC FLAT_HASH_COVERAGE)

    message(STATUS "using gcov = ${GCOV}")
    execute_process(COMMAND ${GCOV} --version)
    set(GCOVR_ADDITIONAL_ARGS --gcov-executable=${GCOV} --config=${CMAKE_SOURCE_DIR}/gcovr.cfg)
    setup_target_for_coverage_gcovr_html(
      NAME
      ${NAME_}_html
      EXECUTABLE
      ${TARGET}
      DEPENDENCIES
      ${TARGET}
      BASE_DIRECTORY
      ${CMAKE_SOURCE_DIR}/include)
    setup_target_for_coverage_gcovr_xml(
      NAME
      ${NAME_}_xml
      EXECUTABLE
      ${TARGET}
      DEPENDENCIES
      ${TARGET}
      BASE_DIRECTORY
      ${CMAKE_SOURCE_DIR}/include)
    target_link_libraries(${TARGET} PUBLIC gcov)
  endif()
endfunction()
