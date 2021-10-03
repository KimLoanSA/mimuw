#include <cassert>
#include <memory>

#include "testUtils.h"
#include "../../main/cc/proxy/program-arguments-resolvers/udpProxyArgumentsResolver.h"

void shouldParseRequiredAndGiveDefaultForNonRequired();
void shouldParseRequiredAndMulticastAndDefaultTimeout();
void shouldParseRequiredAndTimeout();
void shouldParseRequiredAndMetadataAndTimeout();
void shouldParseRequiredAndTimeoutAndDefaultMetadata();

const char *PROGRAM_NAME = "program";

const char *PORT_FLAG = "-P";
const int PORT_VALUE = 2137;
const char *PORT_VALUE_STRING = "2137";

const char *MULTICAST_FLAG = "-B";
const char *MULTICAST_VALUE = "2137.2137.2137.2137";

const char *TIMEOUT_FLAG = "-T";
const int TIMEOUT_VALUE = 2137;
const char *TIMEOUT_STRING_VALUE = "2137";
const int TIMEOUT_VALUE_DEFAULT = 5;

int main() {

  logTestFileName("udpProxyArgumentsResolverTest");

  shouldParseRequiredAndGiveDefaultForNonRequired();
  shouldParseRequiredAndMulticastAndDefaultTimeout();
  shouldParseRequiredAndTimeout();

  logAllTestsPassed();

  return 0;
}

void shouldParseRequiredAndGiveDefaultForNonRequired() {
  logTest("should parse required and give default for non required");

  const char *TEST_PARAMS[] = {
    PROGRAM_NAME,
    PORT_FLAG, PORT_VALUE_STRING,
  };

  std::unique_ptr<UdpProxyArgumentsResolver> udpProxyArgumentsResolver =
    std::make_unique<UdpProxyArgumentsResolver>(3, const_cast<char **>(TEST_PARAMS));

  assert(udpProxyArgumentsResolver->getPort() == PORT_VALUE);
  assert(udpProxyArgumentsResolver->isMulticastAddressDefined() == false);
  assert(udpProxyArgumentsResolver->getTimeoutOrDefault() == TIMEOUT_VALUE_DEFAULT);

  logPassedTest();
}

void shouldParseRequiredAndMulticastAndDefaultTimeout() {
  logTest("should parse required and multicast and give default for timeout");

  const char *TEST_PARAMS[] = {
    PROGRAM_NAME,
    MULTICAST_FLAG, MULTICAST_VALUE,
    PORT_FLAG, PORT_VALUE_STRING,
  };

  std::unique_ptr<UdpProxyArgumentsResolver> udpProxyArgumentsResolver =
    std::make_unique<UdpProxyArgumentsResolver>(5, const_cast<char **>(TEST_PARAMS));

  assert(udpProxyArgumentsResolver->getPort() == PORT_VALUE);
  assert(udpProxyArgumentsResolver->isMulticastAddressDefined() == true);
  assert(udpProxyArgumentsResolver->getMulticastAddress() == MULTICAST_VALUE);
  assert(udpProxyArgumentsResolver->getTimeoutOrDefault() == TIMEOUT_VALUE_DEFAULT);

  logPassedTest();
}

void shouldParseRequiredAndTimeout() {
  logTest("should parse required and timout");

  const char *TEST_PARAMS[] = {
    PROGRAM_NAME,
    PORT_FLAG, PORT_VALUE_STRING,
    TIMEOUT_FLAG, TIMEOUT_STRING_VALUE
  };

  std::unique_ptr<UdpProxyArgumentsResolver> udpProxyArgumentsResolver =
    std::make_unique<UdpProxyArgumentsResolver>(5, const_cast<char **>(TEST_PARAMS));

  assert(udpProxyArgumentsResolver->getPort() == PORT_VALUE);
  assert(udpProxyArgumentsResolver->isMulticastAddressDefined() == false);
  assert(udpProxyArgumentsResolver->getTimeoutOrDefault() == TIMEOUT_VALUE);

  logPassedTest();
}