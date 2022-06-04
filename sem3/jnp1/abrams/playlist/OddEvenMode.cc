#include "OddEvenMode.h"

std::shared_ptr<std::vector<size_t>> OddEvenMode::get_order(size_t size) {
  auto order = std::make_shared<std::vector<size_t>>();
  for (size_t i = 0; 2 * i + 1 < size; i++) {
    order->push_back(2 * i + 1);
  }
  for (size_t i = 0; 2 * i < size; i++) {
    order->push_back(2 * i);
  }

  return order;
}

OddEvenMode::OddEvenMode() {}
