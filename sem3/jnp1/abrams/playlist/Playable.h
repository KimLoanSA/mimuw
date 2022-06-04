#ifndef JNPI_PLAYLIST_PLAYABLE_H
#define JNPI_PLAYLIST_PLAYABLE_H

class Playable {
public:
  virtual ~Playable() {}

  virtual void play() = 0;
};

#endif //JNPI_PLAYLIST_PLAYABLE_H
