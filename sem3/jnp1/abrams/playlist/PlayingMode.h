#ifndef JNPI_PLAYLIST_PLAYINGMODE_H
#define JNPI_PLAYLIST_PLAYINGMODE_H

#include <cstddef>
#include <memory>
#include <vector>

class PlayingMode {
public:
  virtual ~PlayingMode() {}

  virtual std::shared_ptr<std::vector<size_t>> get_order(size_t size) = 0;
};

#endif //JNPI_PLAYLIST_PLAYINGMODE_H
