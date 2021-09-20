#include <thread>
#include <iostream>
#include <chrono>
#include <array>

const long counter = 5'000'000;

long x = 0;
int waits = 1;
std::array<int,2> wants{{0, 0}};

void critical_section(void) {
  long y;
  y = x;
  y = y + 1;
  x = y;
}

void local_section(void) {
}

void entry_protocol(int nr) {
}

void exit_protocol(int nr) {
}


void th(int nr) {
    for (long i  = 0; i < counter; i++) {
        local_section();
        entry_protocol(nr);
        critical_section();
        exit_protocol(nr);
    }
}

void monitor() {
    long prev = 0;
    for (;;) {
        prev = x;
        std::this_thread::sleep_for(std::chrono::seconds(2));
        if (prev==x)
            std::cout << "Deadlock! wants = "<< wants[0] <<"/"<< wants[1] <<
                " " << "waits "<< waits << std::endl;
        else
            std::cout << "monitor: "<< x << std::endl;
    }
}


int main() {
    std::cout << "main() starts" << std::endl;
    std::thread monitor_th{monitor};
    monitor_th.detach();
    std::thread t1{th, 0};
    std::thread t2{th, 1};
    t1.join(); // wait for t1 to complete
    t2.join(); // wait for t2 to complete
    std::cout << "main() completes: " << x << std::endl;
}

