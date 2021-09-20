// this code is adopted from http://en.cppreference.com/w/cpp/thread/condition_variable/wait
#include <iostream>
#include <condition_variable>
#include <thread>
#include <chrono>
#include <array>
#include "log.h"

std::condition_variable cv;
std::mutex cv_m; // This mutex is used for two purposes:
                 // 1) to synchronize accesses to i
                 // 2) for the condition variable cv
int i = 0;
 
void waits()
{
    std::unique_lock<std::mutex> lk(cv_m);
    log("Waiting...");
    cv.wait(lk, []{return i == 1;}); // we use a predicate: wait ends iff i == 1
    log("Finished waiting. i == ", i);
}
 
void signals()
{
    std::this_thread::sleep_for(std::chrono::seconds(1));
    {
        std::lock_guard<std::mutex> lk(cv_m);
        i = 5; // simulate processing that changes i
    }
    log("Notifying...");
    cv.notify_all(); // we don't have mutex here!
    std::this_thread::sleep_for(std::chrono::seconds(1));
    {
        std::lock_guard<std::mutex> lk(cv_m);
        i = 1; // simulate processing that changes i
    }
    log("Notifying again...");
    cv.notify_all(); // we don't have mutex here!
}
 
int main()
{
    std::array<std::thread, 4> threads = {std::thread{waits}, std::thread{waits},
                                          std::thread{waits}, std::thread{signals}};
    for (auto& t : threads)
        t.join();
}


