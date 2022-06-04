#include "SequenceMode.h"

std::shared_ptr<std::vector<size_t>> SequenceMode::get_order(size_t size) {
  auto order = std::make_shared<std::vector<size_t>>();
  for (size_t i = 0; i < size; i++) {
    order->push_back(i);
  }

  return order;
}

SequenceMode::SequenceMode() {}
