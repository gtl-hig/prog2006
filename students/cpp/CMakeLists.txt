cmake_minimum_required(VERSION 3.14)

project(students-cpp)

add_executable(students-cpp)

# Add source files
target_sources(
    students-cpp
    PRIVATE
    students.cpp
)

# Add compile features
target_compile_features(
    students-cpp
    PRIVATE
    cxx_std_17
)
