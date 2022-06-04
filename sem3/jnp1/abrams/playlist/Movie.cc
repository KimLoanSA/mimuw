#include "Movie.h"

void Movie::play() {
  std::cout << play_type
            << " ["
            << MetadataResolver::build_string_from_metadata(metadata,
              required_metadata)
            << "]: "
            << ROT13Decrypter::decrypt(content)
            << std::endl;
}

void Movie::validate_data() {
  MetadataResolver::validateMetadata(metadata, required_metadata);

  for (const auto &value : required_numeric) {
    MetadataResolver::validate_is_number(metadata.at(value));
  }

  if (!std::regex_match(content, content_available_characters)) {
    throw CorruptContentException();
  }
}