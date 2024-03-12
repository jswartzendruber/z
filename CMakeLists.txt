cmake_minimum_required(VERSION 3.10)
project(z)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_BUILD_TYPE "Debug")

set(COMPILER_WARNINGS
    "-Wall"
    "-Wextra"
)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    list(APPEND COMPILER_WARNINGS
        "-fsanitize=address"
        "-fno-omit-frame-pointer"
    )
endif()

add_executable(z src/main.cc src/lexer.cc)
target_compile_options(z PRIVATE ${COMPILER_WARNINGS})

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_link_libraries(z asan)
endif()
