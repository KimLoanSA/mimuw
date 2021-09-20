cmake_minimum_required (VERSION 3.1)
project (MIMUW-CPCPP-lab2 CXX)

find_package ( Threads REQUIRED )

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "-g -Wall")

# http://stackoverflow.com/questions/10555706/
macro (add_executable _name)
    # invoke built-in add_executable
    _add_executable(${ARGV})
    if (TARGET ${_name})
        target_link_libraries(${_name} ${CMAKE_THREAD_LIBS_INIT} )
    endif()
endmacro()

add_executable(mutex mutex.cpp)
add_executable(lock lock.cpp)
add_executable(condition condition.cpp)
add_executable(thread-local thread-local.cpp)
add_executable(thread-local-static thread-local-static.cpp)
add_executable(barrier barrier.cpp)

install(TARGETS DESTINATION .)
