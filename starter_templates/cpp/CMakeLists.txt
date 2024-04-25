cmake_minimum_required(VERSION 3.13)
project(redis-starter-cpp)
set(CMAKE_CXX_STANDARD 23) # Enable the C++23 standard

set(SOURCE_FILES src/Server.cpp)

find_package(fmt CONFIG REQUIRED)

add_executable(server ${SOURCE_FILES})

target_link_libraries(server PRIVATE fmt::fmt)