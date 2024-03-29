cmake_minimum_required(VERSION 3.29)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_BUILD_TYPE "Debug")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_COMPILER clang)
set(CMAKE_CXX_COMPILER clang++)

project(z)

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

add_compile_options(${COMPILER_WARNINGS})

add_subdirectory(Lib)
add_subdirectory(Fuzzing)
add_executable(z Main.cc)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_link_libraries(z PUBLIC Lib asan)
else()
    target_link_libraries(z PUBLIC Lib)
endif()
