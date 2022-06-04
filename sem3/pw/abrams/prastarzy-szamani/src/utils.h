#ifndef SRC_UTILS_H_
#define SRC_UTILS_H_

#include <chrono>
#include <iostream>

void assert_msg(bool condition, std::string const& msg) {
  if (!condition) {
    std::cerr << msg << std::endl;
    exit(1);
  }
}

void assert_eq_msg(uint64_t val1, uint64_t val2, std::string const& msg) {
  if (val1 != val2) {
    std::cerr << msg << ", " << val1 << " is NOT equal " << val2 << std::endl;
    exit(1);
  }
}

std::chrono::time_point<std::chrono::high_resolution_clock> getCurrentTime() {
  return std::chrono::high_resolution_clock::now();
}

double getTimeDifference(
    std::chrono::time_point<std::chrono::high_resolution_clock> const&
        startTime) {
  return std::chrono::duration<double, std::milli>(
             std::chrono::high_resolution_clock::now() - startTime)
      .count();
}

template <class F>
void runAndPrintDuration(F&& lambda) {
  auto startTime = getCurrentTime();
  lambda();
  std::cout << getTimeDifference(startTime) << std::endl;
}

#endif  // SRC_UTILS_H_
