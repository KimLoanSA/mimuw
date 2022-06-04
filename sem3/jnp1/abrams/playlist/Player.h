#ifndef JNPI_PLAYLIST_PLAYER_H
#define JNPI_PLAYLIST_PLAYER_H

#include "Playable.h"
#include "File.h"
#include "Playlist.h"
#include "PlayableFromFileBuilder.h"
#include "PlayerExceptions.h"
#include "SequenceMode.h"

#include <string>

class Player {
private:

  inline static const std::string audio_type{"audio"};
  inline static const std::string video_type{"video"};

public:
  std::shared_ptr<Playable> openFile(const File &file);

  std::shared_ptr<Playlist> createPlaylist(const std::string &name);
};


#endif //JNPI_PLAYLIST_PLAYER_H
