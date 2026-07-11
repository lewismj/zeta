cmake_minimum_required(VERSION 3.28)
project(zeta_core LANGUAGES CXX)

set(CMAKE_CXX_SCAN_FOR_MODULES ON)

add_library(zeta-core STATIC
        src/repr.h
        include/core.h
)
add_library(zeta_core ALIAS zeta-core)

target_include_directories(zeta-core
        PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include
        PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src
        PRIVATE ${PROJECT_SOURCE_DIR}
)

target_sources(zeta-core
        PRIVATE
        src/repr.cpp
)

# BMI2/PEXT support check
if(NOT MSVC)
    include(CheckCXXSourceCompiles)

    set(CMAKE_REQUIRED_FLAGS "-mbmi2")
    set(CMAKE_CXX_SCAN_FOR_MODULES_OLD ${CMAKE_CXX_SCAN_FOR_MODULES})
    set(CMAKE_CXX_SCAN_FOR_MODULES OFF)

    check_cxx_source_compiles("
        #include <immintrin.h>
        int main() {
            unsigned long long src = 0b10101010, mask = 0b1111;
            unsigned long long res = _pext_u64(src, mask);
            (void)res;
            return 0;
        }" PEXT_SUPPORTED)

    set(CMAKE_CXX_SCAN_FOR_MODULES ${CMAKE_CXX_SCAN_FOR_MODULES_OLD})

    if(PEXT_SUPPORTED)
        message(STATUS "PEXT instruction is supported.")
        target_compile_definitions(zeta-core PUBLIC USE_PEXT)
        target_compile_options(zeta-core PUBLIC "-mbmi2")
        target_link_options(zeta-core PUBLIC "-mbmi2")
    else()
        message(STATUS "PEXT instruction is not supported.")
    endif()
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    message(STATUS "Clang C++ compiler is being used.")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    message(STATUS "GCC C++ compiler is being used.")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    message(STATUS "MSVC C++ compiler is being used.")
else()
    message(STATUS "Unknown C++ compiler is being used: ${CMAKE_CXX_COMPILER_ID}")
endif()

# Enforce exhaustiveness on enum switches for Clang/GCC
if (NOT MSVC)
    target_compile_options(zeta-core PRIVATE -Wswitch-enum)
endif()

install(TARGETS zeta-core
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})