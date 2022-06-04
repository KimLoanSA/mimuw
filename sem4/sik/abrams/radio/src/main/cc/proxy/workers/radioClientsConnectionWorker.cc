#include "radioClientsConnectionWorker.h"

#include <utility>

RadioClientsConnectionWorker::RadioClientsConnectionWorker(std::shared_ptr<UdpClient> udpClient, std::shared_ptr<UdpClientsStorage> udpClientsStorage) :
  udpClient(std::move(udpClient)),
  udpClientsStorage(std::move(udpClientsStorage)),
  radioClientCommunicationParser(std::make_unique<RadioClientCommunicationParser>()),
  interrupted(false) { }


void RadioClientsConnectionWorker::work(const std::string &host, const std::string &port, const std::string &resource) {
  while(!udpClient->hasPreviousOperationBeenInterrupted() && !interrupted) {
    workWhileNotInterrupted(host, port, resource);
  }
}

void RadioClientsConnectionWorker::workWhileNotInterrupted(const std::string &host, const std::string &port, const std::string &resource) {
  std::string header = udpClient->readMessage(4);

  if (!udpClient->hasPreviousOperationBeenInterrupted() && !udpClient->hasPreviousOperationTimeouted()) {
    sockaddr_in client = udpClient->getLatestClientAddress();
    CommunicationType communicationType = radioClientCommunicationParser->parseHeader(header);

    resolveCommunicationType(host, port, resource, client, communicationType);
  }
}

void RadioClientsConnectionWorker::resolveCommunicationType(const std::string &host, const std::string &port, const std::string &resource, const sockaddr_in &client, const CommunicationType &communicationType) {
  if (communicationType == DISCOVER) {
    resolveCommunicationTypeDiscover(host, port, resource, client);
  } else if (communicationType == KEEPALIVE) {
    resolveCommunicationTypeKeepalive(client);
  } else {
    exit(1);
  }
}

void RadioClientsConnectionWorker::resolveCommunicationTypeDiscover(const std::string &host, const std::string &port, const std::string &resource, const sockaddr_in &client) {
  auto clientAddress = getClientAddress(client);
  std::string message = radioClientCommunicationParser->getMessageWithIam(host, port, resource);

  udpClientsStorage->addNewClient(clientAddress);
  udpClient->sendMessage(message, client);
}

void RadioClientsConnectionWorker::resolveCommunicationTypeKeepalive(const sockaddr_in &client) {
  auto clientAddress = getClientAddress(client);

  if (udpClientsStorage->isClientSaved(clientAddress)) {
    udpClientsStorage->updateClientTimeoutAndRemoveIfExpired(clientAddress);
  }
}

std::pair<uint16_t, uint32_t> RadioClientsConnectionWorker::getClientAddress(sockaddr_in client) {
  uint16_t port = client.sin_port;
  uint32_t address = client.sin_addr.s_addr;

  return std::make_pair(port, address);
}


void RadioClientsConnectionWorker::interrupt() {
  interrupted = true;
}
