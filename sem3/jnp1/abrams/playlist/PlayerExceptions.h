#ifndef JNPI_PLAYLIST_PLAYEREXCEPTION_H
#define JNPI_PLAYLIST_PLAYEREXCEPTION_H

#include <exception>

class PlayerException : public std::exception {
public:
  virtual const char *what() const throw() {
    return "player internal error";
  }
};

class UnsuportedTypeException : public PlayerException {
public:
  virtual const char *what() const throw() {
    return "unsupported type";
  }
};

class CorruptFileException : public PlayerException {
public:
  virtual const char *what() const throw() {
    return "corrupt file";
  }
};

class CorruptContentException : public PlayerException {
public:
  virtual const char *what() const throw() {
    return "corrupt content";
  }
};

class CorruptMetadataException : public PlayerException {
public:
  virtual const char *what() const throw() {
    return "corrupt metadata";
  }
};

class AddPlaylistException : public PlayerException {
public:
  virtual const char *what() const throw() {
    return "cycle in a playlist";
  }
};

#endif //JNPI_PLAYLIST_PLAYEREXCEPTION_H
