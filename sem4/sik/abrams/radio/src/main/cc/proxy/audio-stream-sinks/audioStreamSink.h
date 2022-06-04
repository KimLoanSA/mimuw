#ifndef MIMUW_S4_SIK_RADIO_AUDIOSTREAMSINK_H
#define MIMUW_S4_SIK_RADIO_AUDIOSTREAMSINK_H

#include <string>

class AudioStreamSink {

public:
  virtual void handleAudioData(const std::string &audioData) = 0;
  virtual void handleMetadata(const std::string &metadata) = 0;

  virtual ~AudioStreamSink() = default;
};


#endif //MIMUW_S4_SIK_RADIO_AUDIOSTREAMSINK_H
