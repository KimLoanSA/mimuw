cmake_minimum_required (VERSION 3.1)
project (ASYNC C)

enable_testing()

#set(CMAKE_C_STANDARD ...)
set(CMAKE_C_FLAGS "-g -Wall -Wextra -pthread")

# http://stackoverflow.com/questions/10555706/
macro (add_executable _name)
  # invoke built-in add_executable
  _add_executable(${ARGV})
  if (TARGET ${_name})
    target_link_libraries(${_name} asyncc)
  endif()
endmacro()

include_directories(include)
add_library(asyncc STATIC threadpool.c future.c)
add_executable(macierz macierz.c)
add_executable(silnia silnia.c)
add_subdirectory(test)

install(TARGETS asyncc DESTINATION .)
