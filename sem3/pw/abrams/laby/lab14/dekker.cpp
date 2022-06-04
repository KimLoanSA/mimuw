#include <thread>
#include <iostream>
#include <atomic>

const long counter = 5'000'000;

std::atomic<bool> flag1 {false};
std::atomic<bool> flag2 {false};

std::atomic<int> turn {1};

void thread1() {
  for(int i = 0; i < counter; i++) {
    flag1 = true;

    while (flag2) {
      if (turn != 1) {
        flag1 = false;
        while (turn != 1) {}
        flag1 = true;
      }
    }

    //sekcja krytyczna
    std::printf("\tTHREAD 1 - SEKCJA KRYTYCZNA\n");
    std::printf("\tTHREAD 1 - SEKCJA KRYTYCZNA\n");

    turn = 2;
    flag1 = false;
  }
}

void thread2() {
  for(int i = 0; i < counter; i++) {
    flag2 = true;

    while (flag1) {
      if (turn != 2) {
        flag2 = false;
        while (turn != 2) {}
        flag2 = true;
      }
    }

    //sekcja krytyczna
    std::printf("THREAD 2 - SEKCJA KRYTYCZNA\n");
    std::printf("THREAD 2 - SEKCJA KRYTYCZNA\n");

    turn = 1;
    flag2 = false;
  }
}

int main() {

  std::cout << "main() starts" << std::endl;
  std::thread t1{thread1};
  std::thread t2{thread2};

  t1.join();
  t2.join();

  return 0;
}