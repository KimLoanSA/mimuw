#include "MetadataResolver.h"

void MetadataResolver::validateMetadata(
  const std::map<std::string, std::string> &metadata,
  const std::vector<std::string> &required_metadata) {

  for (const auto &name : required_metadata) {
    if (!metadata.count(name)) {
      throw CorruptMetadataException();
    }
  }
}

std::string MetadataResolver::build_string_from_metadata(
  const std::map<std::string, std::string> &metadata,
  const std::vector<std::string> &required_metadata) {

  std::string result;
  for (const auto &data : required_metadata) {
    if (!result.empty()) {
      result += ", ";
    }
    result += metadata.at(data);
  }

  return result;
}

void MetadataResolver::validate_is_number(const std::string &value) {
  static const std::regex regex{R"(\d*)"};

  if (!std::regex_match(value, regex)) {
    throw CorruptMetadataException();
  }
}
