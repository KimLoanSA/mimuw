#include "ROT13Decrypter.h"

std::string ROT13Decrypter::decrypt(const std::string &string) {
  std::string result;
  for (const char s : string) {
    if (isalpha(s)) {
      result +=
        (tolower(s)) < 'n' ? static_cast<char>(s + 13) : static_cast<char>(s -
          13);
    } else {
      result += s;
    }
  }
  return result;
}
