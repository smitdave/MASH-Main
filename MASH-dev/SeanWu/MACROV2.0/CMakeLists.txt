cmake_minimum_required(VERSION 3.8)
project(MACROV2_0)

set(CMAKE_CXX_STANDARD 14)

set(SOURCE_FILES main.cpp Human.cpp Human.h)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/build")
add_executable(MACROV2_0 ${SOURCE_FILES})