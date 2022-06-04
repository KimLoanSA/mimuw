#include "File.h"

std::string File::get_type() const {
  return apply_regex_or_throw(description, type_regex);
}

std::string File::get_content() const {
  return apply_regex_or_throw(description, content_regex);
}

std::map<std::string, std::string> File::get_metadata() const {
  std::map<std::string, std::string> result;

  auto words_begin = std::sregex_iterator(description.begin(),
    description.end(), metadata_regex);
  auto words_end = std::sregex_iterator();

  for (std::sregex_iterator it = words_begin; it != words_end; it++) {
    result[it->str(1)] = it->str(2);
  }

  return result;
}

void File::validate_data() {
  if (!std::regex_match(description, description_validation_regex)) {
    throw CorruptFileException();
  }
}

std::string File::apply_regex_or_throw(const std::string &desc,
  const std::regex &regex) const {
  std::smatch result;
  if (!std::regex_search(desc, result, regex)) {
    throw CorruptFileException();
  }

  return result.str(0);
}






