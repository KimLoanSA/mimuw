#ifndef JNPI_PLAYLIST_MOVIE_H
#define JNPI_PLAYLIST_MOVIE_H

#include "Playable.h"
#include "MetadataResolver.h"
#include "ROT13Decrypter.h"

#include <iostream>
#include <regex>
#include <map>
#include <vector>

class Movie : virtual public Playable {
private:
  std::map<std::string, std::string> metadata;
  std::string content;

  inline const static std::string play_type{"Movie"};
  inline const static std::vector<std::string> required_metadata{
    "title", "year"};

  inline const static std::vector<std::string> required_numeric{"year"};

  inline const static std::regex content_available_characters{
    R"([\sa-zA-Z0-9,.!?':;-]*)"};

  void validate_data();

public:
  Movie(const std::map<std::string, std::string> &metadata,
    const std::string &content)
    : metadata(metadata),
    content(content) {
    this->validate_data();
  }

  void play() override;
};


#endif //JNPI_PLAYLIST_MOVIE_H
