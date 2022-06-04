#ifndef MIMUW_S4_SIK_RADIO_UDPAUDIOSTREAMSINK_H
#define MIMUW_S4_SIK_RADIO_UDPAUDIOSTREAMSINK_H

#include <memory>

#include "audioStreamSink.h"
#include "../udp-client/udpClient.h"
#include "../udp-client/udpClientsStorage.h"
#include "../radio-client-communication-parser/radioClientCommunicationParser.h"

class UdpAudioStreamSink : public AudioStreamSink {
public:
  UdpAudioStreamSink(std::shared_ptr<UdpClient> udpClient, std::shared_ptr<UdpClientsStorage> udpClientsStorage);

  void handleAudioData(const std::string &audioData) override;
  void handleMetadata(const std::string &metadata) override;

  ~UdpAudioStreamSink() override = default;;


private:
  std::shared_ptr<UdpClient> udpClient;
  std::shared_ptr<UdpClientsStorage> udpClientsStorage;

  std::unique_ptr<RadioClientCommunicationParser> radioClientCommunicationParser;

  void
  sendMessageToClients(const std::string &message);
  void sendMessageToClient(const std::string &message, const std::pair<uint16_t, uint32_t> &client);
};


#endif //MIMUW_S4_SIK_RADIO_UDPAUDIOSTREAMSINK_H
