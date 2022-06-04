#ifndef MIMUW_S4_SIK_RADIO_AUDIOSTREAMSINKFACTORY_H
#define MIMUW_S4_SIK_RADIO_AUDIOSTREAMSINKFACTORY_H

#include "audioStreamSink.h"
#include "../udp-client/udpClient.h"
#include "../udp-client/udpClientsStorage.h"

#include <memory>

class AudioStreamSinkFactory {

public:
  static std::shared_ptr<AudioStreamSink> outputAudioStreamSink();
  static std::shared_ptr<AudioStreamSink> udpAudioStreamSink(std::shared_ptr<UdpClient> udpClient, std::shared_ptr<UdpClientsStorage> udpClientsStorage);
};

#endif //MIMUW_S4_SIK_RADIO_AUDIOSTREAMSINKFACTORY_H
