#include <thread>
#include <iostream>
#include <chrono>
#include "log.h"

thread_local int counter = 0;

void f() {
    log("f() starts");
    for (int i = 0; i < 1'000'000; i++) {
        // look, ma, no mutex!
        int local = counter;
        local += 1;
        counter = local;
    }
    log("f() completes: counter=", counter);
}

int main() {
    log("main() starts");
    std::thread t1{f};
    std::thread t2{f};
    t1.join();
    t2.join();
    log("main() completes");
}
