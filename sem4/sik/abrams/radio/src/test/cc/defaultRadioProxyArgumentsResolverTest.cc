#include <iostream>
#include <cassert>
#include <memory>

#include "testUtils.h"
#include "../../main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h"

void shouldParseRequiredAndGiveDefaultForNonRequired();
void shouldParseRequiredAndMulticastAndDefaultTimeout();
void shouldParseRequiredAndTimeout();
void shouldParseRequiredAndMetadataAndTimeout();
void shouldParseRequiredAndTimeoutAndDefaultMetadata();

const char *PROGRAM_NAME = "program";

const char *HOST_FLAG = "-h";
const char *HOST_VALUE = "TEST_HOST";

const char *RESOURCE_FLAG = "-r";
const char *RESOURCE_VALUE = "resource_test";

const char *PORT_FLAG = "-p";
const int PORT_VALUE = 2137;
const char *PORT_VALUE_STRING = "2137";

const char *METADATA_FLAG = "-m";
const char *METADATA_VALUE_STRING_YES = "yes";
const char *METADATA_VALUE_STRING_NO = "no";

const bool METADATA_VALUE_TRUE = true;
const bool METADATA_VALUE_FALSE = false;
const bool METADATA_VALUE_DEFAULT = METADATA_VALUE_FALSE;

const char *TIMEOUT_FLAG = "-t";
const int TIMEOUT_VALUE = 2137;
const char *TIMEOUT_STRING_VALUE = "2137";
const int TIMEOUT_VALUE_DEFAULT = 5;

int main() {

  logTestFileName("defaultRadioProxyArgumentsResolverTest");

  shouldParseRequiredAndGiveDefaultForNonRequired();
  shouldParseRequiredAndMulticastAndDefaultTimeout();
  shouldParseRequiredAndTimeout();
  shouldParseRequiredAndMetadataAndTimeout();
  shouldParseRequiredAndTimeoutAndDefaultMetadata();

  logAllTestsPassed();

  return 0;
}

void shouldParseRequiredAndGiveDefaultForNonRequired() {
  logTest("should parse required and give default for non required");

  const char *TEST_PARAMS[] = {
    PROGRAM_NAME,
    HOST_FLAG, HOST_VALUE,
    PORT_FLAG, PORT_VALUE_STRING,
    RESOURCE_FLAG, RESOURCE_VALUE
  };

  std::unique_ptr<DefaultRadioProxyArgumentsResolver> defaultRadioProxyArgumentsResolver =
    std::make_unique<DefaultRadioProxyArgumentsResolver>(7, const_cast<char **>(TEST_PARAMS));


  assert(defaultRadioProxyArgumentsResolver->getHost() == HOST_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getResource() == RESOURCE_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getPort() == PORT_VALUE);

  assert(defaultRadioProxyArgumentsResolver->getMetadataOrDefault() == METADATA_VALUE_DEFAULT);
  assert(defaultRadioProxyArgumentsResolver->getTimeoutOrDefault() == TIMEOUT_VALUE_DEFAULT);

  logPassedTest();
}

void shouldParseRequiredAndMulticastAndDefaultTimeout() {
  logTest("should parse required and metadata yes give default for timeout");

  const char *TEST_PARAMS[] = {
    PROGRAM_NAME,
    HOST_FLAG, HOST_VALUE,
    PORT_FLAG, PORT_VALUE_STRING,
    RESOURCE_FLAG, RESOURCE_VALUE,
    METADATA_FLAG, METADATA_VALUE_STRING_YES
  };

  std::unique_ptr<DefaultRadioProxyArgumentsResolver> defaultRadioProxyArgumentsResolver =
    std::make_unique<DefaultRadioProxyArgumentsResolver>(9, const_cast<char **>(TEST_PARAMS));


  assert(defaultRadioProxyArgumentsResolver->getHost() == HOST_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getResource() == RESOURCE_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getPort() == PORT_VALUE);

  assert(defaultRadioProxyArgumentsResolver->getMetadataOrDefault() == METADATA_VALUE_TRUE);
  assert(defaultRadioProxyArgumentsResolver->getTimeoutOrDefault() == TIMEOUT_VALUE_DEFAULT);

  logPassedTest();
}

void shouldParseRequiredAndTimeout() {
  logTest("should parse required and metadata no give default for timeout");

  const char *TEST_PARAMS[] = {
    PROGRAM_NAME,
    HOST_FLAG, HOST_VALUE,
    PORT_FLAG, PORT_VALUE_STRING,
    RESOURCE_FLAG, RESOURCE_VALUE,
    METADATA_FLAG, METADATA_VALUE_STRING_NO
  };

  std::unique_ptr<DefaultRadioProxyArgumentsResolver> defaultRadioProxyArgumentsResolver =
    std::make_unique<DefaultRadioProxyArgumentsResolver>(9, const_cast<char **>(TEST_PARAMS));


  assert(defaultRadioProxyArgumentsResolver->getHost() == HOST_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getResource() == RESOURCE_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getPort() == PORT_VALUE);

  assert(defaultRadioProxyArgumentsResolver->getMetadataOrDefault() == METADATA_VALUE_FALSE);
  assert(defaultRadioProxyArgumentsResolver->getTimeoutOrDefault() == TIMEOUT_VALUE_DEFAULT);

  logPassedTest();
}

void shouldParseRequiredAndMetadataAndTimeout() {
  logTest("should parse required and metadata yes and timeout");

  const char *TEST_PARAMS[] = {
    PROGRAM_NAME,
    HOST_FLAG, HOST_VALUE,
    PORT_FLAG, PORT_VALUE_STRING,
    RESOURCE_FLAG, RESOURCE_VALUE,
    TIMEOUT_FLAG, TIMEOUT_STRING_VALUE,
    METADATA_FLAG, METADATA_VALUE_STRING_YES
  };

  std::unique_ptr<DefaultRadioProxyArgumentsResolver> defaultRadioProxyArgumentsResolver =
    std::make_unique<DefaultRadioProxyArgumentsResolver>(11, const_cast<char **>(TEST_PARAMS));


  assert(defaultRadioProxyArgumentsResolver->getHost() == HOST_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getResource() == RESOURCE_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getPort() == PORT_VALUE);

  assert(defaultRadioProxyArgumentsResolver->getMetadataOrDefault() == METADATA_VALUE_TRUE);
  assert(defaultRadioProxyArgumentsResolver->getTimeoutOrDefault() == TIMEOUT_VALUE);

  logPassedTest();
}

void shouldParseRequiredAndTimeoutAndDefaultMetadata() {
  logTest("should parse required and timeout and give default metadata");

  const char *TEST_PARAMS[] = {
    PROGRAM_NAME,
    HOST_FLAG, HOST_VALUE,
    PORT_FLAG, PORT_VALUE_STRING,
    RESOURCE_FLAG, RESOURCE_VALUE,
    TIMEOUT_FLAG, TIMEOUT_STRING_VALUE,
  };

  std::unique_ptr<DefaultRadioProxyArgumentsResolver> defaultRadioProxyArgumentsResolver =
    std::make_unique<DefaultRadioProxyArgumentsResolver>(9, const_cast<char **>(TEST_PARAMS));


  assert(defaultRadioProxyArgumentsResolver->getHost() == HOST_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getResource() == RESOURCE_VALUE);
  assert(defaultRadioProxyArgumentsResolver->getPort() == PORT_VALUE);

  assert(defaultRadioProxyArgumentsResolver->getMetadataOrDefault() == METADATA_VALUE_DEFAULT);
  assert(defaultRadioProxyArgumentsResolver->getTimeoutOrDefault() == TIMEOUT_VALUE);

  logPassedTest();
}
