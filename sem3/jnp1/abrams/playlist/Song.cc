#include "Song.h"

void Song::play() {
  std::cout << play_type
            << " ["
            << MetadataResolver::build_string_from_metadata(metadata,
              required_metadata)
            << "]: "
            << content
            << std::endl;
}

void Song::validate_data() {
  MetadataResolver::validateMetadata(metadata, required_metadata);

  if (!std::regex_match(content, content_available_characters)) {
    throw CorruptContentException();
  }
}