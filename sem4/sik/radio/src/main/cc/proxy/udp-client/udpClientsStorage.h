#ifndef MIMUW_S4_SIK_RADIO_UDPCLIENTSSTORAGE_H
#define MIMUW_S4_SIK_RADIO_UDPCLIENTSSTORAGE_H

#include <utility>
#include <vector>
#include <map>
#include <ctime>
#include <mutex>

class UdpClientsStorage {
public:
  UdpClientsStorage(int timeout);

  void addNewClient(std::pair<uint16_t, uint32_t> clientInfo);
  bool updateClientTimeoutAndRemoveIfExpired(std::pair<uint16_t, uint32_t> clientInfo);
  std::vector<std::pair<uint16_t, uint32_t> > getActiveClients();
  bool isClientSaved(std::pair<uint16_t, uint32_t> clientInfo);

private:
  std::mutex mutex;
  std::map<std::pair<uint16_t, uint32_t>, std::time_t> clientsMap;

  int timeout;

  std::time_t getTimestamp();
  bool isTimeoutReached(std::time_t timestamp1, std::time_t timestamp2);
  void removeClientFromMapIfTimeouted(const std::pair<uint16_t, uint32_t> &clientInfo, bool isTimeouted);

  bool removeClientIfTimeouted(time_t actualTimestamp, const std::pair<uint16_t, uint32_t> &clientInfo);

  std::vector<std::pair<uint16_t, uint32_t> > getAllClients();

  void removeAllTimeoutedClients(time_t actualTimestamp,
    const std::vector<std::pair<uint16_t, uint32_t>> &allClients);
};


#endif //MIMUW_S4_SIK_RADIO_UDPCLIENTSSTORAGE_H
