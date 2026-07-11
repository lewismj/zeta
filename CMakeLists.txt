cmake_minimum_required(VERSION 3.28)

project(zeta)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_CXX_STANDARD 23)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(CMAKE_CXX_EXTENSIONS OFF)
elseif(MSVC)
    set(CMAKE_CXX_STANDARD 23)
    set(CMAKE_CXX_STANDARD_REQUIRED ON) 
    add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:/std:c++latest>")
endif()

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options("$<$<CONFIG:Release>:-O3>")
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:-mpopcnt>")
endif()

option(ZETA_BUILD_TESTS "Build Zeta test targets" ON)
if(ZETA_BUILD_TESTS)
    enable_testing()
endif()

add_subdirectory(zeta)
