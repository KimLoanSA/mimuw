#include <thread>
#include <iostream>
#include <atomic>

std::atomic<int> a{0};
volatile int v{0};

void f() {
    for (int i = 0; i < 1000000; i++) {
        a++;
        v++;
    }
}

int main() {
    std::cout << "main() starts" << std::endl;
    std::thread t1{f};
    std::thread t2{f};
    t1.join();
    t2.join();
    std::cout << "main() completes: v=" << v <<" a="<<a<<std::endl;
}
