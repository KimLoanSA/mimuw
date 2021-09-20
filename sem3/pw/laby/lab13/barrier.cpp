#include <iostream>
#include <condition_variable>
#include <thread>
#include <chrono>
#include <array>
#include "log.h"

const int RES = 4;
const int TH_NO = 10;

class Barrier {
  std::condition_variable cv;
  std::mutex cv_m;
  int resistance;

public:
  Barrier(int resistance) : resistance(resistance) {}

  void reach() {
    std::unique_lock<std::mutex> lock(cv_m);

    if (resistance > 0) {
      resistance--;

      if (resistance == 0) {
        lock.unlock();
        cv.notify_all();
      } else {
        cv.wait(lock, [this] { return resistance == 0; });
      }
    }
  }
};

void test(Barrier &barrier) {

  log("JESTEM PRZED BARIERA");
  barrier.reach();

  log("\tJESTEM ZA BARIERA");
}

int main() {
  Barrier barrier(RES);
  std::thread threads[TH_NO];

  for (auto& t : threads) {
    t = std::thread{[&barrier]{test(barrier);}};
  }

  for (auto& t : threads)
    t.join();

  return 0;
}