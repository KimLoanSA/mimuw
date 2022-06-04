#include <iostream>

#include "outputAudioStreamSink.h"

void OutputAudioStreamSink::handleAudioData(const std::string &audioData) {
  std::cout << audioData;
}

void OutputAudioStreamSink::handleMetadata(const std::string &metadata) {
  std::cerr << metadata;
}
