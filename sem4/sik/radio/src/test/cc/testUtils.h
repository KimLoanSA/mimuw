#include <iostream>

void logTestFileName(const std::string &testName) {
  std::cout << "-- " << testName << " tests --" << std::endl;
}

void logAllTestsPassed() {
  std::cout << "-- all tests passed --" << std::endl << std::endl;
}

void logTest(const std::string &message) {
  std::cout << message + ": ";
}

void logPassedTest() {
  std::cout << "passed" << std::endl;
}