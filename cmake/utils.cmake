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

  if(GCOV)
    message(STATUS "Code coverage enabled")

    include(${CMAKE_CURRENT_FUNCTION_LIST_DIR}/CodeCoverage.cmake)
    append_coverage_compiler_flags_to_target(${TARGET})
    target_compile_options(${TARGET} PUBLIC -O0 -g -fno-inline -fno-inline-small-functions -fno-default-inline
                                            --coverage)

    message(STATUS "using gcov = ${GCOV}")
    execute_process(COMMAND ${GCOV} --version)
    set(GCOVR_ADDITIONAL_ARGS --gcov-executable=${GCOV})
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
