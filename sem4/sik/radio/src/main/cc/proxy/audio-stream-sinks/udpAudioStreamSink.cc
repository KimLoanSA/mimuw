#include "udpAudioStreamSink.h"

#include <utility>

UdpAudioStreamSink::UdpAudioStreamSink(std::shared_ptr<UdpClient> udpClient, std::shared_ptr<UdpClientsStorage> udpClientsStorage) :
  udpClient(std::move(udpClient)),
  udpClientsStorage(std::move(udpClientsStorage)),
  radioClientCommunicationParser(std::make_unique<RadioClientCommunicationParser>()) { }

void UdpAudioStreamSink::handleAudioData(const std::string &audioData) {
  auto messagesToSend = radioClientCommunicationParser->getMessageWithAudio(audioData);

  for (const auto &message : messagesToSend) {
    sendMessageToClients(message);
  }
}


void UdpAudioStreamSink::handleMetadata(const std::string &metadata) {
  if (!metadata.empty()) {
    std::string messageToSend = radioClientCommunicationParser->getMessageWithMetadata(metadata);

    sendMessageToClients(messageToSend);
  }
}


void UdpAudioStreamSink::sendMessageToClients(const std::string &message) {
  auto clients = udpClientsStorage->getActiveClients();

  for (auto & client : clients) {
    sendMessageToClient(message, client);
  }
}


void UdpAudioStreamSink::sendMessageToClient(const std::string &message, const std::pair<uint16_t, uint32_t> &client) {
  udpClient->sendMessage(message, client.first, client.second);
}
