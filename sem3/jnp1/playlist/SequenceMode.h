#ifndef JNPI_PLAYLIST_SEQUENCEMODE_H
#define JNPI_PLAYLIST_SEQUENCEMODE_H

#include "PlayingMode.h"

class SequenceMode : virtual public PlayingMode {
public:
  SequenceMode();

  std::shared_ptr<std::vector<size_t>> get_order(size_t size) override;

};


#endif //JNPI_PLAYLIST_SEQUENCEMODE_H
