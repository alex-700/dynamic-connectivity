cmake_minimum_required(VERSION 3.9)

project(dynamic_connectivity)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wpedantic -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2")

find_package(GTest REQUIRED)
find_package(PythonLibs 3.6)

include_directories("include/")
link_libraries(fmt::fmt-header-only)
add_subdirectory(fmt EXCLUDE_FROM_ALL)

add_executable(benchmark benchmarks/main.cpp include/dynamic_connectivity.h)
target_include_directories(benchmark PRIVATE ${PYTHON_INCLUDE_DIRS} matplotlib-cpp)
target_link_libraries(benchmark PRIVATE ${PYTHON_LIBRARIES})

enable_testing()

add_executable(dynamic_connectivity_test tests/test.cpp
        include/dynamic_connectivity.h)
target_link_libraries(dynamic_connectivity_test PRIVATE gtest gtest_main)

add_test(NAME dc_test COMMAND dynamic_connectivity_test)