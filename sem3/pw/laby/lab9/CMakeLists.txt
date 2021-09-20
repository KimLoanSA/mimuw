cmake_minimum_required (VERSION 3.1)
project (lab9 C)

#set(CMAKE_C_STANDARD 17)
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

add_executable(primes primes.c)
target_link_libraries(primes pthread)

add_executable(producer-consumer producer-consumer.c)
target_link_libraries(producer-consumer pthread)

add_executable(readers-writers-template readers-writers-template.c)
target_link_libraries(readers-writers-template pthread)

add_executable(xd xd.c)

install(TARGETS DESTINATION .)
