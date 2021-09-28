#include "ShuffleMode.h"
#include <algorithm>

std::shared_ptr<std::vector<size_t>> ShuffleMode::get_order(size_t size) {
  auto order = std::make_shared<std::vector<size_t>>();
  for (size_t i = 0; i < size; i++) {
    order->push_back(i);
  }

  std::shuffle(order->begin(), order->end(), engine);
  return order;
}

ShuffleMode::ShuffleMode(unsigned seed)
  : engine(std::default_random_engine(seed)) {}
