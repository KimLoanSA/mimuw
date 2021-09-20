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
add_executable(exchange exchange.c)
add_executable(more_info more_info.c)
add_executable(rt_sig rt_sig.c)
add_executable(signal_mask signal_mask.c)
add_executable(signals_in_terminal signals_in_terminal.c)
add_executable(bariera bariera.c)


install(TARGETS DESTINATION .)
