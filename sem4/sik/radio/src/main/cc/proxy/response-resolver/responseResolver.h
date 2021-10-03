#ifndef MIMUW_S4_SIK_RADIO_RESPONSERESOLVER_H
#define MIMUW_S4_SIK_RADIO_RESPONSERESOLVER_H

#include <memory>

#include "../audio-stream-sinks/audioStreamSink.h"
#include "../../utils/programUsagePrinter.h"

class ResponseResolver {

public:
  ResponseResolver(bool metadataRequired, char *programName);

  void parseStatusLine(const std::string &statusLine);
  void parseHeader(const std::string &header);
  size_t getAudioBlockSize();
  size_t parseMetadataBlockSize(const std::string &blockSizeString);
  bool areMetadataParsing();
  bool hasHeadersEnded();

private:
  const static size_t possibleAcceptableStatusLinesSize = 3;
  std::string possibleAcceptableStatusLines[possibleAcceptableStatusLinesSize] = {
    "ICY 200 OK\r\n",
    "HTTP/1.0 200 OK\r\n",
    "HTTP/1.1 200 OK\r\n"
  };
  const std::string metadataHeaderRegexString = R"(icy-metaint:(\d*)\r\n)";
  const std::string headersEndString = "\r\n";
  const int metadataBlockMultiplicationFactor = 16;

  std::unique_ptr<ProgramUsagePrinter> programUsagePrinter;
  bool metadataRequired;

  bool hasHeadersEndedFlag = false;
  bool hasMetadataDetected = false;

  size_t metadataInterval = 8192;

  bool isStatusLineAcceptable(const std::string &statusLine);
  bool isMetadataHeader(const std::string &header);
  int getMetadataInterval(const std::string &header);
  void resolveMetadataHeader(const std::string &header);
  void updateFlagIfHeadersEnded(const std::string &header);
};


#endif //MIMUW_S4_SIK_RADIO_RESPONSERESOLVER_H
