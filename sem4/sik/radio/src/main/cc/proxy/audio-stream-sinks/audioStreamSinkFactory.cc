#include "audioStreamSinkFactory.h"

#include "outputAudioStreamSink.h"
#include "udpAudioStreamSink.h"
#include <memory>

std::shared_ptr<AudioStreamSink> AudioStreamSinkFactory::outputAudioStreamSink() {
  return std::make_shared<OutputAudioStreamSink>();
}

std::shared_ptr<AudioStreamSink> AudioStreamSinkFactory::udpAudioStreamSink(std::shared_ptr<UdpClient> udpClient, std::shared_ptr<UdpClientsStorage> udpClientsStorage) {
  return std::make_shared<UdpAudioStreamSink>(udpClient, udpClientsStorage);
}
