#ifndef MIMUW_S4_SIK_RADIO_RADIOCLIENTSCONNECTIONWORKER_H
#define MIMUW_S4_SIK_RADIO_RADIOCLIENTSCONNECTIONWORKER_H

#include <memory>
#include <atomic>

#include "../udp-client/udpClient.h"
#include "../udp-client/udpClientsStorage.h"
#include "../radio-client-communication-parser/radioClientCommunicationParser.h"

class RadioClientsConnectionWorker {
public:
  RadioClientsConnectionWorker(std::shared_ptr <UdpClient> udpClient, std::shared_ptr <UdpClientsStorage> udpClientsStorage);

  void work(const std::string &host, const std::string &port, const std::string &resource);
  void interrupt();

private:
  std::shared_ptr<UdpClient> udpClient;
  std::shared_ptr<UdpClientsStorage> udpClientsStorage;

  std::unique_ptr<RadioClientCommunicationParser> radioClientCommunicationParser;

  std::atomic_bool interrupted;

  void workWhileNotInterrupted(const std::string &host, const std::string &port, const std::string &resource);
  void resolveCommunicationType(const std::string &host, const std::string &port, const std::string &resource, const sockaddr_in &client, const CommunicationType &communicationType);
  void resolveCommunicationTypeDiscover(const std::string &host, const std::string &port, const std::string &resource, const sockaddr_in &client);
  void resolveCommunicationTypeKeepalive(const sockaddr_in &client);
  std::pair<uint16_t, uint32_t> getClientAddress(sockaddr_in client);
};


#endif //MIMUW_S4_SIK_RADIO_RADIOCLIENTSCONNECTIONWORKER_H
