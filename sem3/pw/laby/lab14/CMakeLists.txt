cmake_minimum_required (VERSION 3.1)
project (MIMUW-CPCPP-lab4 CXX)

find_package ( Threads REQUIRED )

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -Wall")

# http://stackoverflow.com/questions/10555706/
macro (add_executable _name)
    # invoke built-in add_executable
    _add_executable(${ARGV})
    if (TARGET ${_name})
        target_link_libraries(${_name} ${CMAKE_THREAD_LIBS_INIT} )
    endif()
endmacro()

add_executable(mutual-excl mutual-excl.cpp)
#add_executable(peterson peterson.cpp)
add_executable(atomic-volatile atomic-volatile.cpp)
add_executable(lockfree-stack lockfree-stack.cpp)
add_executable(dekker dekker.cpp)

install(TARGETS DESTINATION .)
