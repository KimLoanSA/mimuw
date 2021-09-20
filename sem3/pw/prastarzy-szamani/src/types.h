#ifndef SRC_TYPES_H_
#define SRC_TYPES_H_

#include <vector>

#include "./utils.h"

void burden(uint64_t left, uint64_t right) {
  volatile uint64_t a = 0;
  for (int i = 0; i < 100; ++i) {
    a += (3 * a - left * 44) + (right / (left == 0 ? 1 : left) + 8);
  }
}

class Egg {
 public:
  Egg(uint64_t sizeArg, uint64_t weightArg)
      : size(sizeArg), weight(weightArg) {}

  uint64_t getSize() { return this->size; }
  uint64_t getWeight() {
    burden(this->size, this->weight);
    return this->weight;
  }

 private:
  uint64_t size;
  uint64_t weight;
};

class GrainOfSand {
 public:
  GrainOfSand() : size(0) {}

  GrainOfSand(uint64_t sizeArg) : size(sizeArg) {}  //  NOLINT

  bool operator<(GrainOfSand const& other) const {
    burden(this->size, other.size);
    return this->size < other.size;
  }

  bool operator==(GrainOfSand const& other) const {
    return this->size == other.size;
  }

  GrainOfSand operator=(GrainOfSand const& other) {
    this->size = other.size;
    return *this;
  }

  // private:
  uint64_t size;
};

class Crystal {
 public:
  Crystal() : shininess(0) {}

  Crystal(uint64_t shininessArg) : shininess(shininessArg) {}  // NOLINT

  bool operator<(Crystal const& other) const {
    burden(this->shininess, other.shininess);
    return this->shininess < other.shininess;
  }

  bool operator==(Crystal const& other) {
    return this->shininess == other.shininess;
  }

  Crystal operator=(Crystal const& other) {
    this->shininess = other.shininess;
    return *this;
  }

 private:
  uint64_t shininess;
};

class BottomlessBag {
 public:
  explicit BottomlessBag(uint64_t capacityArg) : capacity(capacityArg) {}

  uint64_t getCapacity() { return this->capacity; }

  void addEgg(Egg const& egg) { this->eggs.push_back(egg); }

 private:
  std::vector<Egg> eggs;

  uint64_t capacity;
};

#endif  // SRC_TYPES_H_
