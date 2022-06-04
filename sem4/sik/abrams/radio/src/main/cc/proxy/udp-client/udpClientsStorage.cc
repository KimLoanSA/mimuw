#include "udpClientsStorage.h"

#include <chrono>

UdpClientsStorage::UdpClientsStorage(int timeout) :
  timeout(timeout) { }

void UdpClientsStorage::addNewClient(std::pair<uint16_t, uint32_t> clientInfo) {
  mutex.lock();

  clientsMap[clientInfo] = getTimestamp();

  mutex.unlock();
}


bool UdpClientsStorage::isClientSaved(std::pair<uint16_t, uint32_t> clientInfo) {
  mutex.lock();

  bool result = clientsMap.count(clientInfo);

  mutex.unlock();

  return result;
}


bool UdpClientsStorage::updateClientTimeoutAndRemoveIfExpired(std::pair<uint16_t, uint32_t> clientInfo) {
  mutex.lock();

  time_t newTimestamp = getTimestamp();
  bool isTimeouted = removeClientIfTimeouted(newTimestamp, clientInfo);

  if (!isTimeouted) {
    clientsMap[clientInfo] = newTimestamp;
  }

  mutex.unlock();

  return !isTimeouted;
}

std::vector<std::pair<uint16_t, uint32_t> > UdpClientsStorage::getActiveClients() {
  mutex.lock();

  std::time_t actualTimestamp = getTimestamp();

  auto allClients = getAllClients();
  removeAllTimeoutedClients(actualTimestamp, allClients);
  auto validClients = getAllClients();

  mutex.unlock();

  return validClients;
}

void UdpClientsStorage::removeAllTimeoutedClients(time_t actualTimestamp, const std::vector<std::pair<uint16_t, uint32_t>> &allClients) {
  for (auto &client : allClients) {
    removeClientIfTimeouted(actualTimestamp, client);
  }
}

std::vector<std::pair<uint16_t, uint32_t> > UdpClientsStorage::getAllClients() {
  std::vector<std::pair<uint16_t, uint32_t>> clients;

  for (auto &client : clientsMap) {
    clients.push_back(client.first);
  }

  return clients;
}


bool UdpClientsStorage::removeClientIfTimeouted(time_t actualTimestamp, const std::pair<uint16_t, uint32_t> &clientInfo) {
  time_t oldTimestamp = clientsMap[clientInfo];
  bool isTimeouted = isTimeoutReached(oldTimestamp, actualTimestamp);

  removeClientFromMapIfTimeouted(clientInfo, isTimeouted);

  return isTimeouted;
}

bool UdpClientsStorage::isTimeoutReached(std::time_t oldTimestamp, std::time_t newTimestamp) {
  int timeDifference = (int) difftime(newTimestamp, oldTimestamp);

  return timeDifference > timeout + 1;
}

void UdpClientsStorage::removeClientFromMapIfTimeouted(const std::pair<uint16_t, uint32_t> &clientInfo, bool isTimeouted) {
  if (isTimeouted) {
    clientsMap.erase(clientInfo);
  }
}


std::time_t UdpClientsStorage::getTimestamp() {
  return std::time(nullptr);
}
