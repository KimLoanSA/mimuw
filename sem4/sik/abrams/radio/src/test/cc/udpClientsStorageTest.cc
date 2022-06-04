#include <cassert>
#include <memory>
#include <iostream>
#include <vector>
#include <unistd.h>

#include "testUtils.h"
#include "../../main/cc/proxy/udp-client/udpClientsStorage.h"

const static std::pair<uint16_t, uint32_t> CLIENT_1(18, 22);
const static std::pair<uint16_t, uint32_t> CLIENT_2(21, 37);

void shouldAddToStorageAndGetList();
void shouldAddToStorageAndGetListAfterTimeout();
void shouldAddToStorageAndGetListAfterUpdate();
void shouldRemoveIfTimeoutedAndNotUpdate();

int main() {

  logTestFileName("udpClientsStorageTest");

  shouldAddToStorageAndGetList();
  shouldAddToStorageAndGetListAfterTimeout();
  shouldAddToStorageAndGetListAfterUpdate();
  shouldRemoveIfTimeoutedAndNotUpdate();

  logAllTestsPassed();

  return 0;
}

void shouldAddToStorageAndGetList() {
  logTest("should add to storage and get list");

  UdpClientsStorage udpClientsStorage(2);
  udpClientsStorage.addNewClient(CLIENT_1);
  udpClientsStorage.addNewClient(CLIENT_2);

  std::vector<std::pair<uint16_t, uint32_t> > required;
  required.push_back(CLIENT_1);
  required.push_back(CLIENT_2);

  assert(udpClientsStorage.getActiveClients() == required);

  logPassedTest();
}

void shouldAddToStorageAndGetListAfterTimeout() {
  logTest("should add to storage and get list after timout");

  UdpClientsStorage udpClientsStorage(2);
  udpClientsStorage.addNewClient(CLIENT_1);

  std::vector<std::pair<uint16_t, uint32_t> > required;
  sleep(4);

  assert(udpClientsStorage.getActiveClients() == required);

  logPassedTest();
}

void shouldAddToStorageAndGetListAfterUpdate() {
  logTest("should add to storage and get list after update");

  UdpClientsStorage udpClientsStorage(3);
  udpClientsStorage.addNewClient(CLIENT_1);

  std::vector<std::pair<uint16_t, uint32_t> > required;
  required.push_back(CLIENT_1);

  sleep(2);
  udpClientsStorage.updateClientTimeoutAndRemoveIfExpired(CLIENT_1);
  sleep(2);

  assert(udpClientsStorage.getActiveClients() == required);

  logPassedTest();
}

void shouldRemoveIfTimeoutedAndNotUpdate() {
  logTest("should remove if timeouted and not update");

  UdpClientsStorage udpClientsStorage(2);
  udpClientsStorage.addNewClient(CLIENT_1);

  sleep(4);
  udpClientsStorage.updateClientTimeoutAndRemoveIfExpired(CLIENT_1);

  std::vector<std::pair<uint16_t, uint32_t> > required;

  assert(udpClientsStorage.getActiveClients() == required);

  logPassedTest();
}