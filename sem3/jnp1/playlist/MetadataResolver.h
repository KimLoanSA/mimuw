#ifndef JNPI_PLAYLIST_METADATARESOLVER_H
#define JNPI_PLAYLIST_METADATARESOLVER_H

#include "PlayerExceptions.h"

#include <map>
#include <string>
#include <vector>
#include <regex>

class MetadataResolver {
public:
  static void validateMetadata(const std::map<std::string,
    std::string> &metadata, const std::vector<std::string> &required_metadata);

  static std::string build_string_from_metadata(const std::map<std::string,
    std::string> &metadata, const std::vector<std::string> &required_metadata);

  static void validate_is_number(const std::string &value);

};

#endif //JNPI_PLAYLIST_METADATARESOLVER_H
