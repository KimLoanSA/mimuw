cmake_minimum_required(VERSION 3.10)

project(MIMUW-S5-BSK-Zadanie-3)

set(CMAKE_C_STANDARD 99)
set(CMAKE_VERBOSE_MAKEFILE on)
set(CMAKE_C_COMPILER gcc)

set(SOURCE_FILES
        src/template/main.c
        src/template/auth.c src/template/auth.h
        src/template/safe_memory.c src/template/safe_memory.h
        src/template/commands_handler.c src/template/commands_handler.h
        src/commands_to_handle.h
        src/example/example_function.h
        src/impl/classification_resolver.c src/impl/classification_resolver.h)

add_executable(zadanie_3 ${SOURCE_FILES})

include_directories(/usr/include)
target_link_libraries(zadanie_3 PRIVATE -lpam -lpam_misc)