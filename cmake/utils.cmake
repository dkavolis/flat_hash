function(check_cxx_feature_macro MACRO VALUE HEADER VARIABLE)
  include(CheckCXXSourceCompiles)
  check_cxx_source_compiles(
    "#include <${HEADER}>
static_assert(${MACRO} > ${VALUE}L);
int main() {}" ${VARIABLE})
endfunction()
