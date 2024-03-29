cmake_minimum_required(VERSION 3.29)
project(z)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_BUILD_TYPE "Debug")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(COMPILER_WARNINGS
    "-Wall"
    "-Wextra"
    "-Wunused"
)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    list(APPEND COMPILER_WARNINGS
        "-fsanitize=address"
        "-fno-omit-frame-pointer"
    )
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Release")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3")
endif()

add_compile_options(${COMPILER_WARNINGS})

add_subdirectory(Lib)
add_executable(z Main.cc)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_link_libraries(z PUBLIC Lib asan)
else()
    target_link_libraries(z PUBLIC Lib)
endif()
