#ifndef JNPI_PLAYLIST_ODDEVENMODE_H
#define JNPI_PLAYLIST_ODDEVENMODE_H

#include "PlayingMode.h"

class OddEvenMode : virtual public PlayingMode {
public:
  OddEvenMode();

  std::shared_ptr<std::vector<size_t>> get_order(size_t size) override;
};


#endif //JNPI_PLAYLIST_ODDEVENMODE_H
