vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO catchorg/Catch2
    REF v3.0.0-preview4
    SHA512 3a879da07dd5f4e2bcd0d5e1be0de5b0128930c3a5a343805b308ddc788618bb0a9d5ea6de673cf8745f7997eebafabb65d29ee39120bf82218f8df0f47be039
    HEAD_REF devel
    PATCHES
        fix-install-path.patch
)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS
        -DCATCH_INSTALL_DOCS=OFF
        -DCATCH_INSTALL_EXTRAS=ON
        -DCATCH_DEVELOPMENT_BUILD=OFF
        -DCMAKE_CXX_STANDARD=20
)

vcpkg_cmake_install()
if (NOT DEFINED VCPKG_BUILD_TYPE OR VCPKG_BUILD_TYPE STREQUAL "release")
    file(RENAME "${CURRENT_PACKAGES_DIR}/share/Catch2" "${CURRENT_PACKAGES_DIR}/share/catch2_")
    file(RENAME "${CURRENT_PACKAGES_DIR}/share/catch2_" "${CURRENT_PACKAGES_DIR}/share/catch2")
endif()
if (NOT DEFINED VCPKG_BUILD_TYPE OR VCPKG_BUILD_TYPE STREQUAL "debug")
    file(RENAME "${CURRENT_PACKAGES_DIR}/debug/share/Catch2" "${CURRENT_PACKAGES_DIR}/debug/share/catch2_")
    file(RENAME "${CURRENT_PACKAGES_DIR}/debug/share/catch2_" "${CURRENT_PACKAGES_DIR}/debug/share/catch2")
endif()

vcpkg_cmake_config_fixup(CONFIG_PATH "lib/cmake/Catch2")
vcpkg_fixup_pkgconfig()

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include")
file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/share")
file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/include/catch2/generators/internal" "${CURRENT_PACKAGES_DIR}/include/catch2/benchmark/internal")

if(NOT EXISTS "${CURRENT_PACKAGES_DIR}/include/catch2/catch_all.hpp")
    message(FATAL_ERROR "Main includes have moved. Please update the forwarder.")
endif()

file(WRITE "${CURRENT_PACKAGES_DIR}/include/catch.hpp" "#include <catch2/catch_all.hpp>")
file(INSTALL "${SOURCE_PATH}/LICENSE.txt" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}" RENAME copyright)
