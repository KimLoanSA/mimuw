#include "radioClientCommunicationParser.h"

#include <arpa/inet.h>
#include <cstring>

CommunicationType RadioClientCommunicationParser::parseHeader(const std::string &header) {
  uint16_t numberBeforeConversion = stringToInt16(header);
  uint16_t numberAfterConversion = ntohs(numberBeforeConversion);

  return getCommunicationTypeForNumber(numberAfterConversion);
}

uint16_t RadioClientCommunicationParser::stringToInt16(std::string string) {
  uint16_t result;
  memcpy(&result, &string[0], 2);

  return result;
}

CommunicationType RadioClientCommunicationParser::getCommunicationTypeForNumber(uint16_t numberAfterConversion) {
  if (numberAfterConversion == DISCOVER) {
    return DISCOVER;
  } else if (numberAfterConversion == KEEPALIVE) {
    return KEEPALIVE;
  } else {
    return ERROR;
  }
}


std::string RadioClientCommunicationParser::getMessageWithIam(const std::string &host, const std::string &port, const std::string &resource) {
  const uint16_t messageType = IAM;
  std::string message = createIamMessage(host, port, resource);
  const uint16_t messageSize = message.size();

  return convertHeaderAndCreateMessage(messageType, messageSize, message);
}

std::string RadioClientCommunicationParser::createIamMessage(const std::string &host, const std::string &port, const std::string &resource) {
  return "radio: " + host + ":" + port + resource + iamMessage;
}


std::vector<std::string> RadioClientCommunicationParser::getMessageWithAudio(const std::string& audioData) {
  std::vector<std::string> result;
  const uint16_t messageType = AUDIO;
  size_t audioDataSize = std::min(maxMessageDataSize, audioData.size());

  std::vector<std::string> chunkedAudioData = getMessageWithAudioIfTooBig(audioData, audioData.size());

  result.push_back(convertHeaderAndCreateMessage(messageType, audioDataSize, audioData.substr(0, audioDataSize)));
  result.insert(result.end(), chunkedAudioData.begin(), chunkedAudioData.end());

  return result;
}

std::vector<std::string> RadioClientCommunicationParser::getMessageWithAudioIfTooBig(const std::string &audioData, const size_t audioDataSize) {
  if (audioDataSize > maxMessageDataSize) {
    return getMessageWithAudio(audioData.substr(maxMessageDataSize));
  }

  return std::vector<std::string>();
}


std::string RadioClientCommunicationParser::getMessageWithMetadata(const std::string& metadata) {
  const uint16_t metadataSize = metadata.size();
  const uint16_t messageType = METADATA;

  return convertHeaderAndCreateMessage(messageType, metadataSize, metadata);
}

std::string RadioClientCommunicationParser::convertHeaderAndCreateMessage(const uint16_t type, const uint16_t size, const std::string& data) {
  const uint16_t convertedType = htons(type);
  const uint16_t convertedSize = htons(size);

  return createMessageForHeaderAndBody(convertedType, convertedSize, data);
}


std::string RadioClientCommunicationParser::createMessageForHeaderAndBody(const uint16_t convertedType, const uint16_t convertedSize, const std::string& data) {
  std::string convertedMessageTypeString = uint16ToString(convertedType);
  std::string convertedMetadataSizeString = uint16ToString(convertedSize);

  return convertedMessageTypeString + convertedMetadataSizeString + data;
}

std::string RadioClientCommunicationParser::uint16ToString(uint16_t number) {
  char result[2];
  memcpy(result, &number, 2);

  return std::string(result, 2);
}


