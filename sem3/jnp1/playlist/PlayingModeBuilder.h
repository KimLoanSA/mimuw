#ifndef PLAYLIST_PLAYINGMODEBUILDER_H
#define PLAYLIST_PLAYINGMODEBUILDER_H

#include "PlayingMode.h"
#include "ShuffleMode.h"
#include "SequenceMode.h"
#include "OddEvenMode.h"

#include <memory>

std::shared_ptr<PlayingMode> createSequenceMode() {
  return std::make_shared<SequenceMode>();
}

std::shared_ptr<PlayingMode> createShuffleMode(const unsigned long seed) {
  return std::make_shared<ShuffleMode>(seed);
}

std::shared_ptr<PlayingMode> createOddEvenMode() {
  return std::make_shared<OddEvenMode>();
}

#endif //PLAYLIST_PLAYINGMODEBUILDER_H
