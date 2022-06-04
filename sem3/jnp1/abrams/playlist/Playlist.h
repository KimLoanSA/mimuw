#ifndef JNPI_PLAYLIST_PLAYLIST_H
#define JNPI_PLAYLIST_PLAYLIST_H

#include <cstddef>
#include <list>
#include <string>
#include <memory>
#include <set>

#include "Playable.h"
#include "PlayingMode.h"

class Playlist : virtual public Playable {
private:
  std::list<std::shared_ptr<Playable> > playlist;
  std::shared_ptr<PlayingMode> mode;
  const std::string name;
  std::multiset<std::shared_ptr<Playlist>> contains_lists;
  std::shared_ptr<Playlist> shared_this;

  void check_playlist(std::shared_ptr<Playable> element);

  void rm_if_playlist(std::shared_ptr<Playable> element);

public:
  Playlist(std::string name, std::shared_ptr<PlayingMode> mode);

  void play() override;

  void add(std::shared_ptr<Playable> element);

  void add(std::shared_ptr<Playable> element, size_t position);

  void remove();

  void remove(size_t position);

  void setMode(std::shared_ptr<PlayingMode> mode);

  //Sprawdza czy w bieżącej playliście i playlistach w niej zawartych jest
  //podana playlisty root
  bool check_if_cycle(std::shared_ptr<Playlist> root);
};


#endif //JNPI_PLAYLIST_PLAYLIST_H
