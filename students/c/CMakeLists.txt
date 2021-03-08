cmake_minimum_required(VERSION 3.15)

project(students-c)

add_executable(students-c)

target_sources(
    students-c
    PRIVATE
    main.c
    stud_db.c
    stud_db.h
)

target_include_directories(
    students-c
    PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}
)

target_compile_features(
    students-c
    PRIVATE
    c_std_11
)
