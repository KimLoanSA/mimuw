#ifndef MIMUW_S4_SIK_RADIO_UDPPROXYARGUMENTSRESOLVER_H
#define MIMUW_S4_SIK_RADIO_UDPPROXYARGUMENTSRESOLVER_H

#include <memory>
#include "../../utils/programArgumentsParser.h"
#include "../../utils/programUsagePrinter.h"

class UdpProxyArgumentsResolver {
public:
  UdpProxyArgumentsResolver(int argc, char *argv[]);

  int getPort();
  bool isPortDefined();

  bool isMulticastAddressDefined();
  std::string getMulticastAddress();

  int getTimeoutOrDefault();

private:
  const std::unique_ptr<ProgramArgumentsParser> programArgumentsParser;
  const std::unique_ptr<ProgramUsagePrinter> programUsagePrinter;

  const std::string portFlag = "-P";

  const std::string multicastFlag = "-B";
  const std::string timeoutFlag = "-T";

  bool isPortDefinedFlag = false;
  int port;

  bool isMulticastDefined;
  std::string multicast;
  int timeout = 5;

  void parseRequiredArgumentsOrExit();
  void parsePortOrExit();

  void parseOptionalArguments();

  void parseTimeout();
  void parseTimeoutIfDefinedOrExit();
  void parseMulticast();
  void parseMulticastIfDefined();

};


#endif //MIMUW_S4_SIK_RADIO_UDPPROXYARGUMENTSRESOLVER_H
