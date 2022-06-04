#ifndef SRC_ADVENTURE_H_
#define SRC_ADVENTURE_H_

#include <algorithm>
#include <atomic>
#include <vector>

#include "../third_party/threadpool/threadpool.h"
#include "./types.h"
#include "./utils.h"

class Adventure {
 public:
  virtual ~Adventure() = default;

  virtual uint64_t packEggs(std::vector<Egg> eggs, BottomlessBag &bag) = 0;

  virtual void arrangeSand(std::vector<GrainOfSand> &grains) = 0;

  virtual Crystal selectBestCrystal(std::vector<Crystal> &crystals) = 0;
};

class LonesomeAdventure : public Adventure {
 public:
  LonesomeAdventure() {}

  virtual uint64_t packEggs(std::vector<Egg> eggs, BottomlessBag &bag) {
    uint64_t capacity = bag.getCapacity();
    size_t numberOfEggs = eggs.size();

    std::vector<std::vector<uint64_t>> dp(numberOfEggs + 1);
    for (size_t i = 0; i < dp.size(); i++) {
      dp[i].resize(capacity + 1, 0);
    }

    for (size_t eggIndex = 1; eggIndex <= numberOfEggs; eggIndex++) {
      for (uint64_t actualCapacity = 0; actualCapacity <= capacity;
           actualCapacity++) {
        Egg actualEgg = eggs[eggIndex - 1];

        dp[eggIndex][actualCapacity] = dp[eggIndex - 1][actualCapacity];

        if (previousCapacityPredicate(dp, eggIndex, actualCapacity)) {
          dp[eggIndex][actualCapacity] = dp[eggIndex][actualCapacity - 1];
        }

        uint64_t actualEggSize = actualEgg.getSize();
        uint64_t actualEggWeight = actualEgg.getWeight();

        if (addEggPredicate(dp, eggIndex, actualCapacity, actualEggSize,
                            actualEggWeight)) {
          dp[eggIndex][actualCapacity] =
              dp[eggIndex - 1][actualCapacity - actualEggSize] +
              actualEggWeight;
        }
      }
    }

    recoverResult(eggs, bag, capacity, numberOfEggs, dp);

    return dp[numberOfEggs][capacity];
  }

  virtual void arrangeSand(std::vector<GrainOfSand> &grains) {
    mergeSort(grains);
  }

  virtual Crystal selectBestCrystal(std::vector<Crystal> &crystals) {
    return *std::max_element(crystals.begin(), crystals.end());
  }

 private:
  void mergeSort(std::vector<GrainOfSand> &grains) {
    if (grains.size() > 1) {
      std::size_t const splitPoint = grains.size() / 2;
      std::vector<GrainOfSand> left(grains.begin(),
                                    grains.begin() + splitPoint);
      std::vector<GrainOfSand> right(grains.begin() + splitPoint, grains.end());

      mergeSort(left);
      mergeSort(right);

      std::merge(left.begin(), left.end(), right.begin(), right.end(),
                 grains.begin());
    }
  }

  bool previousCapacityPredicate(std::vector<std::vector<uint64_t>> &dp,
                                 size_t eggIndex,
                                 uint64_t actualCapacity) const {
    return actualCapacity > 0 &&
           dp[eggIndex][actualCapacity - 1] > dp[eggIndex][actualCapacity];
  }

  bool addEggPredicate(std::vector<std::vector<uint64_t>> &dp, size_t eggIndex,
                       uint64_t actualCapacity, uint64_t actualEggSize,
                       uint64_t actualEggWeight) const {
    return actualCapacity >= actualEggSize &&
           dp[eggIndex - 1][actualCapacity - actualEggSize] + actualEggWeight >
               dp[eggIndex][actualCapacity];
  }

  void recoverResult(const std::vector<Egg> &eggs, BottomlessBag &bag,
                     uint64_t capacity, size_t numberOfEggs,
                     const std::vector<std::vector<uint64_t>> &dp) const {
    uint64_t actualCapacity = capacity;
    for (size_t eggIndex = numberOfEggs; eggIndex > 0 && actualCapacity >= 0;
         eggIndex--) {
      Egg actualEgg = eggs[eggIndex - 1];

      if (previousCapacityEquality(dp, actualCapacity, eggIndex)) {
        actualCapacity--;
      }

      uint64_t actualEggSize = actualEgg.getSize();
      uint64_t actualEggWeight = actualEgg.getWeight();

      if (addEggEquality(actualCapacity, eggIndex, actualEggSize,
                         actualEggWeight, dp)) {
        actualCapacity -= actualEggSize;
        bag.addEgg(eggs[eggIndex - 1]);
      }
    }
  }

  bool addEggEquality(uint64_t actualCapacity, size_t eggIndex,
                      uint64_t actualEggSize, uint64_t actualEggWeight,
                      const std::vector<std::vector<uint64_t>> &dp) const {
    return actualCapacity >= actualEggSize &&
           dp[eggIndex - 1][actualCapacity - actualEggSize] + actualEggWeight ==
               dp[eggIndex][actualCapacity];
  }

  bool previousCapacityEquality(const std::vector<std::vector<uint64_t>> &dp,
                                uint64_t actualCapacity,
                                size_t eggIndex) const {
    return actualCapacity > 0 &&
           dp[eggIndex][actualCapacity - 1] == dp[eggIndex][actualCapacity];
  }
};

class TeamAdventure : public Adventure {
 public:
  explicit TeamAdventure(uint64_t numberOfShamansArg)
      : numberOfShamans(numberOfShamansArg),
        councilOfShamans(numberOfShamansArg),
        numberOfWorkingThreadsMergeSort(0),
        cacheLineSize(8) {}

  uint64_t packEggs(std::vector<Egg> eggs, BottomlessBag &bag) { return 1; }

  virtual void arrangeSand(std::vector<GrainOfSand> &grains) {
    numberOfWorkingThreadsMergeSort = numberOfShamans - 1;
    auto result =
        councilOfShamans.enqueue([this, &grains]() { mergeSort(grains); });

    result.wait();
  }

  virtual Crystal selectBestCrystal(std::vector<Crystal> &crystals) {
    uint64_t crystalsSize = crystals.size();
    uint64_t threadsNumber = std::min(crystalsSize, numberOfShamans);
    uint64_t intervalLength = crystalsSize / threadsNumber;

    std::vector<std::future<Crystal>> results;

    for (uint64_t i = 0; i < threadsNumber; i++) {
      auto endIt = (i == threadsNumber - 1)
                       ? crystals.end()
                       : crystals.begin() + (i + 1) * intervalLength;

      std::vector<Crystal> crystalsInterval(
          crystals.begin() + i * intervalLength, endIt);

      results.push_back(councilOfShamans.enqueue(
          [](std::vector<Crystal> &crystals) {
            return *std::max_element(crystals.begin(), crystals.end());
          },
          crystalsInterval));
    }

    return getFinalResult(results);
  }

 private:
  uint64_t numberOfShamans;
  ThreadPool councilOfShamans;
  std::atomic<int64_t> numberOfWorkingThreadsMergeSort;
  size_t cacheLineSize;

  void mergeSort(std::vector<GrainOfSand> &grains) {
    if (grains.size() > 1) {
      std::size_t splitPoint = grains.size() / 2;
      std::vector<GrainOfSand> left(grains.begin(),
                                    grains.begin() + splitPoint);
      std::vector<GrainOfSand> right(grains.begin() + splitPoint, grains.end());
      std::future<void> result;

      if (grains.size() > cacheLineSize &&
          --numberOfWorkingThreadsMergeSort > 0) {
        result =
            councilOfShamans.enqueue([this, &right]() { mergeSort(right); });
      } else {
        mergeSort(right);
      }

      mergeSort(left);

      if (result.valid()) {
        result.wait();
      }

      std::merge(left.begin(), left.end(), right.begin(), right.end(),
                 grains.begin());
    }
  }

  Crystal getFinalResult(std::vector<std::future<Crystal>> &results) const {
    Crystal bestCrystal;
    for (auto &crystal : results) {
      crystal.wait();
      Crystal actualCrystal = crystal.get();

      if (bestCrystal < actualCrystal) {
        bestCrystal = actualCrystal;
      }
    }

    return bestCrystal;
  }
};

#endif  // SRC_ADVENTURE_H_
