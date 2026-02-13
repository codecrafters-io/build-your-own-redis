cmake_minimum_required(VERSION 3.13)

project(codecrafters-redis)

set(CMAKE_CXX_STANDARD 23) # Enable the C++23 standard

file(GLOB_RECURSE SOURCE_FILES src/*.cpp src/*.hpp)

add_executable(redis ${SOURCE_FILES})