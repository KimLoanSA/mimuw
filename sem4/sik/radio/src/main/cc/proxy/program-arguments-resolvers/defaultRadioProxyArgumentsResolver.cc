#include "defaultRadioProxyArgumentsResolver.h"

DefaultRadioProxyArgumentsResolver::DefaultRadioProxyArgumentsResolver(int argc, char **argv) :
  programArgumentsParser(std::make_unique<ProgramArgumentsParser>(argc, argv)),
  programUsagePrinter(std::make_unique<ProgramUsagePrinter>(argv[0])) {

  parseRequiredArgumentsOrExit();
  parseOptionalArguments();
}

std::string DefaultRadioProxyArgumentsResolver::getHost() {
  return host;
}

std::string DefaultRadioProxyArgumentsResolver::getResource() {
  return resource;
}

int DefaultRadioProxyArgumentsResolver::getPort() {
  return port;
}

bool DefaultRadioProxyArgumentsResolver::getMetadataOrDefault() {
  return metadata;
}

int DefaultRadioProxyArgumentsResolver::getTimeoutOrDefault() {
  return timeout;
}

void DefaultRadioProxyArgumentsResolver::parseRequiredArgumentsOrExit() {
  parseHostOrExit();
  parseResourceOrExit();
  parsePortOrExit();
}

void DefaultRadioProxyArgumentsResolver::parseHostOrExit() {
  host = getStringArgumentOrExit(hostFlag);
}

void DefaultRadioProxyArgumentsResolver::parseResourceOrExit() {
  resource = getStringArgumentOrExit(resourceFlag);
}

void DefaultRadioProxyArgumentsResolver::parsePortOrExit() {
  port = getIntArgumentOrExit(portFlag);
}

std::string DefaultRadioProxyArgumentsResolver::getStringArgumentOrExit(const std::string &flag) {
  exitIfArgumentNotDefined(flag);

  return programArgumentsParser->getArgument(flag);
}

void DefaultRadioProxyArgumentsResolver::exitIfArgumentNotDefined(const std::string &flag) {
  if (!programArgumentsParser->isArgumentDefined(flag)) {
    programUsagePrinter->printUsageAndExitWith1();
  }
}

int DefaultRadioProxyArgumentsResolver::getIntArgumentOrExit(const std::string &flag) {
  exitIfIntArgumentNotDefined(flag);

  return programArgumentsParser->getIntArgument(flag);
}

void DefaultRadioProxyArgumentsResolver::exitIfIntArgumentNotDefined(const std::string &flag) {
  if (!programArgumentsParser->isIntArgumentDefined(flag)) {
    programUsagePrinter->printUsageAndExitWith1();
  }
}

void DefaultRadioProxyArgumentsResolver::parseOptionalArguments() {
  parseMetadata();
  parseTimeout();
}

void DefaultRadioProxyArgumentsResolver::parseMetadata() {
  if (programArgumentsParser->isArgumentDefined(metadataFlag)) {
    parseMetadataIfDefinedOrExit();
  }
}

void DefaultRadioProxyArgumentsResolver::parseMetadataIfDefinedOrExit() {
  std::string metadataValue = programArgumentsParser->getArgument(metadataFlag);

  if (isMetadataValueBoolean(metadataValue)) {
    metadata = mapValidMetadata(metadataValue);
  } else {
    programUsagePrinter->printUsageAndExitWith1();
  }
}

bool DefaultRadioProxyArgumentsResolver::isMetadataValueBoolean(const std::string &metadataValue) {
  return metadataValue == "yes"
    || metadataValue == "no";
}

bool DefaultRadioProxyArgumentsResolver::mapValidMetadata(const std::string &metadataValue) {
  return metadataValue == "yes";
}

void DefaultRadioProxyArgumentsResolver::parseTimeout() {
  if (programArgumentsParser->isIntArgumentDefined(timeoutFlag)) {
    parseTimeoutIfDefinedOrExit();
  } else if (programArgumentsParser->isArgumentDefined(timeoutFlag)) {
    programUsagePrinter->printUsageAndExitWith1();
  }
}

void DefaultRadioProxyArgumentsResolver::parseTimeoutIfDefinedOrExit() {
  timeout = programArgumentsParser->getIntArgument(timeoutFlag);

  if (timeout <= 0) {
    programUsagePrinter->printUsageAndExitWith1();
  }
}
