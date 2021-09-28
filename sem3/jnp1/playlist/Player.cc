#include "Player.h"

std::shared_ptr<Playable> Player::openFile(const File &file) {
  if (file.get_type() == audio_type) {
    return PlayableFromFileBuilder::to_song(file);
  } else if (file.get_type() == video_type) {
    return PlayableFromFileBuilder::to_movie(file);
  } else {
    throw UnsuportedTypeException();
  }
}

std::shared_ptr<Playlist> Player::createPlaylist(const std::string &name) {
  return std::make_shared<Playlist>(name, std::make_shared<SequenceMode>());
}
