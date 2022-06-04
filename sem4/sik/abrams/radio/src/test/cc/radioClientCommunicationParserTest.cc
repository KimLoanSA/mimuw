#include <cassert>
#include <memory>
#include <vector>
#include <string>
#include "testUtils.h"
#include "../../main/cc/proxy/radio-client-communication-parser/radioClientCommunicationParser.h"

void shouldParseDiscoverHeader();
void shouldParseDiscoverKeepalive();
void shouldParseInvalidHeader();
void shouldCreateMessageWithMetadata();
void shouldCreateMessageWithAudioData();
void shouldCreateMessageWithIam();

char ZERO_CHAR = 0;
char ONE_CHAR = 1;
char TWO_CHAR = 2;
char THREE_CHAR = 3;
char FOUR_CHAR = 4;
char SIX_CHAR = 6;
char THIRTEEN_CHAR = 13;
char FIFTY_FOUR_CHAR = 54;
char MAX_CHAR = 255;
char MAX_CHAR_MINUS_3 = 252;

const static size_t MAX_AUDIO_MESSAGE_SIZE = 65535;

const static std::string METADATA_TEST = "METADATA TEST";
const static std::string HOST_TEST = "HOST";
const static std::string PORT_TEST = "2137";
const static std::string RESOURCE_TEST = "/XD";

int main() {

  logTestFileName("radioClientCommunicationParserTest");

  shouldParseDiscoverHeader();
  shouldParseDiscoverKeepalive();
  shouldParseInvalidHeader();
  shouldCreateMessageWithMetadata();
  shouldCreateMessageWithAudioData();
  shouldCreateMessageWithIam();

  logAllTestsPassed();

  return 0;
}

void shouldParseDiscoverHeader() {
  logTest("should parse discover header");

  RadioClientCommunicationParser radioClientCommunicationParser;

  std::string header;
  header += ZERO_CHAR;
  header += ONE_CHAR;
  header += ZERO_CHAR;
  header += ZERO_CHAR;

  assert(radioClientCommunicationParser.parseHeader(header) == DISCOVER);

  logPassedTest();
}

void shouldParseDiscoverKeepalive() {
  logTest("should parse keepalive header");

  RadioClientCommunicationParser radioClientCommunicationParser;

  std::string header;
  header += ZERO_CHAR;
  header += THREE_CHAR;
  header += ZERO_CHAR;
  header += ZERO_CHAR;

  assert(radioClientCommunicationParser.parseHeader(header) == KEEPALIVE);

  logPassedTest();
}

void shouldParseInvalidHeader() {
  logTest("should parse invalid header");

  RadioClientCommunicationParser radioClientCommunicationParser;

  std::string header;
  header += THREE_CHAR;
  header += ONE_CHAR;
  header += ZERO_CHAR;
  header += ZERO_CHAR;

  assert(radioClientCommunicationParser.parseHeader(header) == ERROR);

  logPassedTest();
}

#include <iostream>
void shouldCreateMessageWithMetadata() {
  logTest("should create message with metadata");

  RadioClientCommunicationParser radioClientCommunicationParser;

  std::string required;
  required += ZERO_CHAR;
  required += SIX_CHAR;

  required += ZERO_CHAR;
  required += THIRTEEN_CHAR;

  required += METADATA_TEST;

  assert(radioClientCommunicationParser.getMessageWithMetadata(METADATA_TEST) == required);

  logPassedTest();
}

void shouldCreateMessageWithAudioData() {
  logTest("should create message with audio data");

  RadioClientCommunicationParser radioClientCommunicationParser;

  std::string audioData1;
  audioData1.resize(MAX_AUDIO_MESSAGE_SIZE, '1');

  std::string requiredAudioData1;

  requiredAudioData1 += ZERO_CHAR;
  requiredAudioData1 += FOUR_CHAR;

  requiredAudioData1 += MAX_CHAR;
  requiredAudioData1 += MAX_CHAR;

  requiredAudioData1 += audioData1;


  std::string audioData2;
  audioData2.resize(MAX_AUDIO_MESSAGE_SIZE - 3, '2');

  std::string requiredAudioData2;

  requiredAudioData2 += ZERO_CHAR;
  requiredAudioData2 += FOUR_CHAR;

  requiredAudioData2 += MAX_CHAR;
  requiredAudioData2 += MAX_CHAR_MINUS_3;

  requiredAudioData2 += audioData2;

  std::vector<std::string> required;
  required.push_back(requiredAudioData1);
  required.push_back(requiredAudioData2);

  std::string given = audioData1 + audioData2;

  assert(radioClientCommunicationParser.getMessageWithAudio(given) == required);

  logPassedTest();
}

void shouldCreateMessageWithIam() {
  logTest("should create message with iam");

  RadioClientCommunicationParser radioClientCommunicationParser;

  std::string required;
  required += ZERO_CHAR;
  required += TWO_CHAR;

  required += ZERO_CHAR;
  required += FIFTY_FOUR_CHAR;

  required += "radio: " + HOST_TEST + ":" + PORT_TEST + RESOURCE_TEST + " | HEY ; ) pozdro chillerka d-_-b |";

  assert(radioClientCommunicationParser.getMessageWithIam(HOST_TEST, PORT_TEST, RESOURCE_TEST) == required);

  logPassedTest();
}