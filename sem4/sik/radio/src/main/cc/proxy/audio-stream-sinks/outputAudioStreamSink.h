#ifndef MIMUW_S4_SIK_RADIO_OUTPUTAUDIOSTREAMSINK_H
#define MIMUW_S4_SIK_RADIO_OUTPUTAUDIOSTREAMSINK_H

#include "audioStreamSink.h"

class OutputAudioStreamSink : public AudioStreamSink {

public:
  OutputAudioStreamSink() = default;

  void handleAudioData(const std::string &audioData) override;
  void handleMetadata(const std::string &metadata) override;

  ~OutputAudioStreamSink() override = default;
};


#endif //MIMUW_S4_SIK_RADIO_OUTPUTAUDIOSTREAMSINK_H
