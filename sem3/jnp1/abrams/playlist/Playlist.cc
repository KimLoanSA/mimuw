#include "Playlist.h"
#include "PlayerExceptions.h"
#include <iostream>
#include <functional>
#include <iterator>

Playlist::Playlist(std::string name, std::shared_ptr<PlayingMode> mode)
  : playlist(std::list<std::shared_ptr<Playable> >()),
  mode(mode),
  name(name),
  shared_this(std::make_shared<Playlist>(*this)) {
  contains_lists.insert(shared_this);
}

void Playlist::play() {
  std::cout << "Playlist [" << name << "]\n";

  auto iterator = playlist.begin();
  auto order = mode->get_order(playlist.size());

  for (size_t i = 0; i < playlist.size(); i++) {
    size_t to_play = (*order)[i];
    iterator = playlist.begin();
    std::advance(iterator, to_play);

    (*iterator)->play();
  }
}

bool Playlist::check_if_cycle(std::shared_ptr<Playlist> root) {
  for (auto it = contains_lists.begin(); it != contains_lists.end(); ++it) {
    if (it->get() == root.get()) {
      return true;
    }
    if ((*it)->check_if_cycle(root)) {
      return true;
    }
  }

  return false;
}

void Playlist::check_playlist(std::shared_ptr<Playable> element) {
  auto to_add = std::dynamic_pointer_cast<Playlist>(element);
  if (to_add.get() != nullptr) {
    if (to_add->check_if_cycle(this->shared_this))
      throw AddPlaylistException();

    contains_lists.insert(to_add->shared_this);
  }
}

void Playlist::rm_if_playlist(std::shared_ptr<Playable> element) {
  auto to_rm = std::dynamic_pointer_cast<Playlist>(element);
  if (to_rm.get() != nullptr)
    contains_lists.erase(to_rm->shared_this);
}

void Playlist::add(std::shared_ptr<Playable> element) {
  check_playlist(element);

  playlist.push_back(element);
}

void Playlist::add(std::shared_ptr<Playable> element, size_t position) {
  check_playlist(element);

  auto iterator = playlist.begin();
  std::advance(iterator, position);

  playlist.insert(iterator, element);
}

void Playlist::remove() {
  rm_if_playlist(playlist.back());
  playlist.pop_back();
}

void Playlist::remove(size_t position) {
  auto iterator = playlist.begin();
  std::advance(iterator, position);

  rm_if_playlist(*iterator);
  playlist.erase(iterator);
}

void Playlist::setMode(std::shared_ptr<PlayingMode> mode) {
  this->mode = mode;
}
