project(cppfront)

cmake_minimum_required(VERSION 3.12)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED On)
set(CMAKE_CXX_EXTENSIONS Off)

add_executable(
    ${PROJECT_NAME}
    source/common.cpp
    source/cppfront.cpp
    source/io.cpp
    source/lex.cpp
    source/parse.cpp
    source/parse_tree.cpp
    source/parse_tree_print.cpp
)

target_compile_options(
    ${PROJECT_NAME}
    PRIVATE
    $<$<CXX_COMPILER_ID:MSVC>:/W4>
    $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-Wall -Wextra>
)
