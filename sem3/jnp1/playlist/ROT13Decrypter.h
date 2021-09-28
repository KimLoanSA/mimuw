#ifndef JNPI_PLAYLIST_ROT13DECRYPTER_H
#define JNPI_PLAYLIST_ROT13DECRYPTER_H

#include <cctype>
#include <iostream>
#include <string>

class ROT13Decrypter {
public:
  static std::string decrypt(const std::string &string);

};


#endif //JNPI_PLAYLIST_ROT13DECRYPTER_H
