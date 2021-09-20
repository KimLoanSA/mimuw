#include <thread>
#include <iostream>
#include <chrono>
#include "log.h"

int g() {
    thread_local static int count = 0;
    count += 1;
    return count;
}

void f() {
    int local = 0;
    log("f() starts");
    for (int i = 0; i < 1'000'000; i++) {
        local = g();
    }
    log("f() completes: local=", local);
}

int main() {
    log("main() starts");
    std::thread t1{f};
    std::thread t2{f};
    t1.join();
    t2.join();
    log("main() completes");
}
