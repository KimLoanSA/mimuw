#include <memory>
#include <regex>

#include "responseResolver.h"
#include "../../utils/programUsagePrinter.h"

ResponseResolver::ResponseResolver(bool metadataRequired, char *programName) :
  programUsagePrinter(std::make_unique<ProgramUsagePrinter>(programName)),
  metadataRequired(metadataRequired) { }


void ResponseResolver::parseStatusLine(const std::string &statusLine) {
  if (!isStatusLineAcceptable(statusLine)) {
    programUsagePrinter->printUsageAndExitWith1();
  }
}

bool ResponseResolver::isStatusLineAcceptable(const std::string &statusLine) {
  for (const auto &possibleAcceptableStatusLine : possibleAcceptableStatusLines) {
    if (statusLine == possibleAcceptableStatusLine) {
      return true;
    }
  }

  return false;
}


void ResponseResolver::parseHeader(const std::string &header) {
  if (isMetadataHeader(header)) {
    resolveMetadataHeader(header);
  }

  updateFlagIfHeadersEnded(header);
}

bool ResponseResolver::isMetadataHeader(const std::string &header) {
  std::regex metadataHeaderRegex(metadataHeaderRegexString);

  return std::regex_match(header, metadataHeaderRegex);
}

void ResponseResolver::resolveMetadataHeader(const std::string &header) {
  if (metadataRequired) {
    metadataInterval = getMetadataInterval(header);
    hasMetadataDetected = true;
  } else {
    programUsagePrinter->printUsageAndExitWith1();
  }
}

int ResponseResolver::getMetadataInterval(const std::string &header) {
  std::regex metadataHeaderRegex(metadataHeaderRegexString);
  std::smatch match;

  std::regex_search(header, match, metadataHeaderRegex);

  return std::stoi(match.str(1));
}

void ResponseResolver::updateFlagIfHeadersEnded(const std::string &header) {
  if (header == headersEndString) {
    hasHeadersEndedFlag = true;
  }
}


bool ResponseResolver::areMetadataParsing() {
  return hasMetadataDetected && metadataRequired;
}


bool ResponseResolver::hasHeadersEnded() {
  return hasHeadersEndedFlag;
}


size_t ResponseResolver::getAudioBlockSize() {
  return metadataInterval;
}


size_t ResponseResolver::parseMetadataBlockSize(const std::string &blockSizeString) {
  size_t metadataSize = blockSizeString[0];

  return metadataSize * metadataBlockMultiplicationFactor;
}




