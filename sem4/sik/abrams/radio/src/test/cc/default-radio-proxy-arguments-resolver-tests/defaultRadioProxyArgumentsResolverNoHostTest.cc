#include <iostream>
#include <cassert>
#include <memory>

#include "../testUtils.h"
#include "../../../main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h"

void shouldParseRequiredAndGiveDefaultForNonRequired();

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
const int TIMEOUT_VALUE_DEFAULT = 5;

int main() {
  logTest("should exit with 1 - no host");

  const char *TEST_PARAMS[] = {
    PROGRAM_NAME,
    PORT_FLAG, PORT_VALUE_STRING,
    RESOURCE_FLAG, RESOURCE_VALUE
  };

  std::unique_ptr<DefaultRadioProxyArgumentsResolver> defaultRadioProxyArgumentsResolver =
    std::make_unique<DefaultRadioProxyArgumentsResolver>(5, const_cast<char **>(TEST_PARAMS));

  logPassedTest();

  return 0;
}