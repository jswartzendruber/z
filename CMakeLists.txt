cmake_minimum_required(VERSION 3.10)
project(z)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(z src/main.cc src/lexer.cc)
