#ifndef MIMUW_S4_SIK_RADIO_RADIOCLIENTCOMMUNICATIONPARSER_H
#define MIMUW_S4_SIK_RADIO_RADIOCLIENTCOMMUNICATIONPARSER_H

#include <string>
#include <vector>
#include <cstdint>

enum CommunicationType {
  DISCOVER = 1,
  IAM = 2,
  KEEPALIVE = 3,
  AUDIO = 4,
  METADATA = 6,
  ERROR = 0,
};

class RadioClientCommunicationParser {
public:
  RadioClientCommunicationParser() = default;

  CommunicationType parseHeader(const std::string &header);
  std::vector<std::string> getMessageWithAudio(const std::string &audioData);
  std::string getMessageWithMetadata(const std::string &metadata);
  std::string getMessageWithIam(const std::string &host, const std::string &port, const std::string &resource);

private:
  const size_t maxMessageDataSize = 8192;
  const std::string iamMessage = " | HEY ; ) pozdro chillerka d-_-b |";

  uint16_t stringToInt16(std::string string);
  CommunicationType getCommunicationTypeForNumber(uint16_t numberAfterConversion);

  std::string createIamMessage(const std::string &host, const std::string &port, const std::string &resource);
  std::string createMessageForHeaderAndBody(uint16_t convertedType, uint16_t convertedSize, const std::string& data);
  std::string convertHeaderAndCreateMessage(uint16_t type, uint16_t size, const std::string& data);

  std::vector<std::string> getMessageWithAudioIfTooBig(const std::string &audioData,size_t audioDataSize);
  std::string uint16ToString(uint16_t number);
};


#endif //MIMUW_S4_SIK_RADIO_RADIOCLIENTCOMMUNICATIONPARSER_H
