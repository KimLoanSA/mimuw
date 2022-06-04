#ifndef JNPI_PLAYLIST_FILE_H
#define JNPI_PLAYLIST_FILE_H

#include "PlayerExceptions.h"
#include <string>
#include <vector>
#include <regex>
#include <map>

class File {
private:
  const std::string description;

  inline const static std::regex type_regex{R"(([^\|]*))"};
  inline const static std::regex content_regex{R"(([^\|]*)$)"};
  inline const static std::regex metadata_regex{R"(([^\|:]*):([^\|]*))"};
  inline const static std::regex description_validation_regex{
    R"((([^\|]+)((\|[^\|]*:[^\|]*)*)\|([^\|]+)))"};

  std::string apply_regex_or_throw(const std::string &desc,
    const std::regex &regex) const;

  void validate_data();

public:
  File(const std::string &desc)
    : description(desc) {
    this->validate_data();
  }

  std::string get_type() const;

  std::string get_content() const;

  std::map<std::string, std::string> get_metadata() const;
};

#endif //JNPI_PLAYLIST_FILE_H
