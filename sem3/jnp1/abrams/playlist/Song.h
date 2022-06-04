#ifndef JNPI_PLAYLIST_SONG_H
#define JNPI_PLAYLIST_SONG_H

#include "Playable.h"
#include "MetadataResolver.h"

#include <iostream>
#include <regex>
#include <map>
#include <vector>

class Song : virtual public Playable {
private:
  std::map<std::string, std::string> metadata;
  std::string content;

  inline const static std::string play_type{"Song"};
  inline const static std::vector<std::string> required_metadata{
    "artist", "title"};

  inline const static std::regex content_available_characters{
    R"([\sa-zA-Z0-9,.!?':;-]*)"};

  void validate_data();

public:
  Song(const std::map<std::string, std::string> &metadata,
    const std::string &content)
    : metadata(metadata),
    content(content) {
    this->validate_data();
  }

  void play() override;
};


#endif //JNPI_PLAYLIST_SONG_H
