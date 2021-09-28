#ifndef JNPI_PLAYLIST_PLAYABLEFROMFILEBUILDER_H
#define JNPI_PLAYLIST_PLAYABLEFROMFILEBUILDER_H

#include "Song.h"
#include "Movie.h"
#include "File.h"

#include <memory>

class PlayableFromFileBuilder {

public:
  static std::shared_ptr<Song> to_song(const File &file);

  static std::shared_ptr<Movie> to_movie(const File &file);
};


#endif //JNPI_PLAYLIST_PLAYABLEFROMFILEBUILDER_H
