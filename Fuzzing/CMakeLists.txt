cmake_minimum_required(VERSION 3.29)
project(Fuzzing)

add_executable(Fuzz Fuzz.cc)

target_compile_options(Fuzz PRIVATE
    -g
    -O1
)

target_link_options(Fuzz PRIVATE
    -fsanitize=fuzzer,address
)

target_link_libraries(Fuzz Lib)
