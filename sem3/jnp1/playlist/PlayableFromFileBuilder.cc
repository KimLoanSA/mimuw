#include "PlayableFromFileBuilder.h"

std::shared_ptr<Song> PlayableFromFileBuilder::to_song(const File &file) {
  return std::make_shared<Song>(file.get_metadata(), file.get_content());
}

std::shared_ptr<Movie> PlayableFromFileBuilder::to_movie(const File &file) {
  return std::make_shared<Movie>(file.get_metadata(), file.get_content());
}
