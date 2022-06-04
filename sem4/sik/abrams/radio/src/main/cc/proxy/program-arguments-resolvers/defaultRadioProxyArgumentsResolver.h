#ifndef MIMUW_S4_SIK_RADIO_DEFAULTRADIOPROXYARGUMENTSRESOLVER_H
#define MIMUW_S4_SIK_RADIO_DEFAULTRADIOPROXYARGUMENTSRESOLVER_H

#include <memory>

#include "../../utils/programArgumentsParser.h"
#include "../../utils/programUsagePrinter.h"

class DefaultRadioProxyArgumentsResolver {

public:
  DefaultRadioProxyArgumentsResolver(int argc, char *argv[]);

  std::string getHost();
  std::string getResource();
  int getPort();

  bool getMetadataOrDefault();
  int getTimeoutOrDefault();

private:
  const std::unique_ptr<ProgramArgumentsParser> programArgumentsParser;
  const std::unique_ptr<ProgramUsagePrinter> programUsagePrinter;

  const std::string hostFlag = "-h";
  const std::string resourceFlag = "-r";
  const std::string portFlag = "-p";

  const std::string metadataFlag = "-m";
  const std::string timeoutFlag = "-t";

  std::string host;
  std::string resource;
  int port;

  bool metadata = false;
  int timeout = 5;

  void parseRequiredArgumentsOrExit();
  void parseHostOrExit();
  void parseResourceOrExit();
  void parsePortOrExit();
  std::string getStringArgumentOrExit(const std::string &flag);
  void exitIfArgumentNotDefined(const std::string &flag);
  int getIntArgumentOrExit(const std::string &flag);
  void exitIfIntArgumentNotDefined(const std::string &flag);

  void parseOptionalArguments();
  void parseMetadata();
  void parseMetadataIfDefinedOrExit();

  static bool isMetadataValueBoolean(const std::string &metadataValue);
  static bool mapValidMetadata(const std::string &metadataValue);

  void parseTimeout();
  void parseTimeoutIfDefinedOrExit();

};


#endif //MIMUW_S4_SIK_RADIO_DEFAULTRADIOPROXYARGUMENTSRESOLVER_H
