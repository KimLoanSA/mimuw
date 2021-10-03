#include "udpProxyArgumentsResolver.h"

UdpProxyArgumentsResolver::UdpProxyArgumentsResolver(int argc, char *argv[]) :
  programArgumentsParser(std::make_unique<ProgramArgumentsParser>(argc, argv)),
  programUsagePrinter(std::make_unique<ProgramUsagePrinter>(argv[0])) {

  parseRequiredArgumentsOrExit();
  parseOptionalArguments();
}


int UdpProxyArgumentsResolver::getPort() {
  return port;
}


bool UdpProxyArgumentsResolver::isMulticastAddressDefined() {
  return isMulticastDefined;
}


std::string UdpProxyArgumentsResolver::getMulticastAddress() {
  return multicast;
}


int UdpProxyArgumentsResolver::getTimeoutOrDefault() {
  return timeout;
}


void UdpProxyArgumentsResolver::parseRequiredArgumentsOrExit() {
  parsePortOrExit();
}

void UdpProxyArgumentsResolver::parsePortOrExit() {
  if (programArgumentsParser->isIntArgumentDefined(portFlag)) {
    port = programArgumentsParser->getIntArgument(portFlag);
    isPortDefinedFlag = true;
  } else if (programArgumentsParser->isArgumentDefined(portFlag)) {
    programUsagePrinter->printUsageAndExitWith1();
  }
}


void UdpProxyArgumentsResolver::parseOptionalArguments() {
  parseTimeout();
  parseMulticast();
}

void UdpProxyArgumentsResolver::parseTimeout() {
  if (isPortDefinedFlag && programArgumentsParser->isIntArgumentDefined(timeoutFlag)) {
    parseTimeoutIfDefinedOrExit();
  } else if (programArgumentsParser->isArgumentDefined(timeoutFlag)) {
    programUsagePrinter->printUsageAndExitWith1();
  }
}

void UdpProxyArgumentsResolver::parseTimeoutIfDefinedOrExit() {
  timeout = programArgumentsParser->getIntArgument(timeoutFlag);

  if (timeout <= 0) {
    programUsagePrinter->printUsageAndExitWith1();
  }
}

void UdpProxyArgumentsResolver::parseMulticast() {
  isMulticastDefined = programArgumentsParser->isArgumentDefined(multicastFlag);
  parseMulticastIfDefined();
}


void UdpProxyArgumentsResolver::parseMulticastIfDefined() {
  if (isMulticastDefined) {
    multicast = programArgumentsParser->getArgument(multicastFlag);
  }

  if (!isPortDefinedFlag && isMulticastDefined) {
    programUsagePrinter->printUsageAndExitWith1();
  }
}

bool UdpProxyArgumentsResolver::isPortDefined() {
  return isPortDefinedFlag;
}



