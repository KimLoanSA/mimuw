#ifndef JNPI_PLAYLIST_SHUFFLEMODE_H
#define JNPI_PLAYLIST_SHUFFLEMODE_H

#include <vector>
#include <random>
#include "PlayingMode.h"

class ShuffleMode : virtual public PlayingMode {
private:
  std::default_random_engine engine;

public:
  ShuffleMode(unsigned seed);

  std::shared_ptr<std::vector<size_t>> get_order(size_t size) override;

};


#endif //JNPI_PLAYLIST_SHUFFLEMODE_H
