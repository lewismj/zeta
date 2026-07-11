# Specify dependencies.
if(POLICY CMP0167)
    # Prefer Boost's package config (vcpkg-provided on Windows).
    cmake_policy(SET CMP0167 NEW)
endif()

if(WIN32)
    set(_zeta_boost_triplet "${VCPKG_TARGET_TRIPLET}")
    if(NOT _zeta_boost_triplet)
        set(_zeta_boost_triplet x64-windows)
    endif()

    set(_zeta_boost_prefix_candidates "")
    if(DEFINED VCPKG_INSTALLED_DIR AND VCPKG_INSTALLED_DIR)
        list(APPEND _zeta_boost_prefix_candidates "${VCPKG_INSTALLED_DIR}/${_zeta_boost_triplet}")
    endif()
    if(DEFINED ENV{VCPKG_ROOT})
        list(APPEND _zeta_boost_prefix_candidates "$ENV{VCPKG_ROOT}/installed/${_zeta_boost_triplet}")
    endif()
    if(EXISTS "${CMAKE_SOURCE_DIR}/../vcpkg/installed/${_zeta_boost_triplet}")
        list(APPEND _zeta_boost_prefix_candidates "${CMAKE_SOURCE_DIR}/../vcpkg/installed/${_zeta_boost_triplet}")
    endif()

    foreach(_zeta_boost_prefix IN LISTS _zeta_boost_prefix_candidates)
        if(EXISTS "${_zeta_boost_prefix}/share/boost/BoostConfig.cmake")
            list(PREPEND CMAKE_PREFIX_PATH "${_zeta_boost_prefix}")
            set(Boost_DIR "${_zeta_boost_prefix}/share/boost" CACHE PATH "Boost config path")
            break()
        endif()
    endforeach()

    if((NOT DEFINED BOOST_ROOT OR BOOST_ROOT STREQUAL "") AND DEFINED Boost_DIR AND Boost_DIR)
        get_filename_component(_zeta_boost_share "${Boost_DIR}" DIRECTORY)
        if(_zeta_boost_share)
            get_filename_component(BOOST_ROOT "${_zeta_boost_share}" DIRECTORY)
        endif()
    endif()
endif()

# stacktrace_windbg gives rich symbols on Windows; stacktrace_basic is the
# portable fallback on Linux/macOS (addresses only unless addr2line is present).
find_package(Boost 1.88 REQUIRED COMPONENTS unit_test_framework)
if(NOT Boost_FOUND)
    message(FATAL_ERROR "Boost not found.")
endif()

add_executable(zeta_core_test
    src/test_repr.cpp
    src/test_holdem.cpp
)

target_link_libraries(zeta_core_test
    PRIVATE
    zeta_core
    zeta-holdem
    Boost::unit_test_framework
)

target_compile_definitions(zeta_core_test PRIVATE BOOST_TEST_DYN_LINK)
target_compile_features(zeta_core_test PRIVATE cxx_std_23)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta_core_test PRIVATE stdc++ m)
endif()

add_test(NAME zeta_core_test COMMAND zeta_core_test)

add_custom_target(zeta_test
    COMMAND ${CMAKE_CTEST_COMMAND} -C $<CONFIG> --output-on-failure --test-dir ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS zeta_core_test
    USES_TERMINAL
    COMMENT "Running all tests in zeta/test"
)

if(WIN32)
    if(NOT DEFINED BOOST_ROOT)
        if(DEFINED ENV{BOOST_ROOT})
            set(BOOST_ROOT "$ENV{BOOST_ROOT}")
        elseif(Boost_INCLUDE_DIRS)
            list(GET Boost_INCLUDE_DIRS 0 _boost_inc)
            get_filename_component(BOOST_ROOT "${_boost_inc}" DIRECTORY)
        elseif(DEFINED Boost_DIR AND Boost_DIR)
            get_filename_component(_zeta_boost_share "${Boost_DIR}" DIRECTORY)
            get_filename_component(BOOST_ROOT "${_zeta_boost_share}" DIRECTORY)
        else()
            message(FATAL_ERROR "BOOST_ROOT not set.")
        endif()
    endif()

    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(BOOST_DLL_PATH "${BOOST_ROOT}/debug/bin")
    else()
        set(BOOST_DLL_PATH "${BOOST_ROOT}/bin")
    endif()
    message(STATUS "Looking for Boost DLLs in: ${BOOST_DLL_PATH}")
    file(GLOB BOOST_TEST_DLL "${BOOST_DLL_PATH}/boost_unit_test_framework*.dll")

    foreach(dll IN LISTS BOOST_TEST_DLL)
        add_custom_command(TARGET zeta_core_test POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${dll}"
            $<TARGET_FILE_DIR:zeta_core_test>
            COMMENT "Copying Boost.Test DLL to output directory"
        )
    endforeach()
endif()
