cmake_minimum_required (VERSION 3.1)
project (MIMUW-FORK C)

#set(CMAKE_C_STANDARD ...)
set(CMAKE_C_FLAGS "-g -Wall")

# http://stackoverflow.com/questions/10555706/
macro (add_executable _name)
  # invoke built-in add_executable
  _add_executable(${ARGV})
  if (TARGET ${_name})
    target_link_libraries(${_name} err)
  endif()
endmacro()

add_library(err err.c)
add_executable(child-pipe child-pipe.c)
add_executable(parent-pipe parent-pipe.c)
add_executable(parent-dup parent-dup.c)
add_executable(child-fifo child-fifo.c)
add_executable(parent-fifo parent-fifo.c)

install(TARGETS DESTINATION .)
