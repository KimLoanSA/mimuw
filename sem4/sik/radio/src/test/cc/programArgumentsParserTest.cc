#include <iostream>
#include <cassert>

#include "testUtils.h"
#include "../../main/cc/utils/programArgumentsParser.h"

void shouldParseArgument();
void shouldParseIntArgument();
void shouldNotParseInvalidIntArgument();
void shouldNotParseFlagWithoutValue();
void shouldNotParseFlagIfNextIsFlag();

const char *PROGRAM_NAME = "program";

int main() {
  logTestFileName("programArgumentsParser");

  shouldParseArgument();
  shouldParseIntArgument();
  shouldNotParseInvalidIntArgument();
  shouldNotParseFlagWithoutValue();
  shouldNotParseFlagIfNextIsFlag();

  logAllTestsPassed();

  return 0;
}

void shouldParseArgument() {
  logTest("should parse argument");

  const char *FLAG = "-flag";
  const char *VALUE = "test";

  const char *TEST_ARGV[] = {PROGRAM_NAME, FLAG, VALUE};

  ProgramArgumentsParser *programArgumentsParser = new ProgramArgumentsParser(3, const_cast<char **>(TEST_ARGV));

  assert(programArgumentsParser->isArgumentDefined(FLAG) == true);
  assert(programArgumentsParser->getArgument(FLAG) == VALUE);

  assert(programArgumentsParser->isArgumentDefined("invalid") == false);

  logPassedTest();
}

void shouldParseIntArgument() {
  logTest("should parse int argument");

  const char *FLAG = "-flag";
  const char *VALUE = "123";

  const char *TEST_ARGV[] = {PROGRAM_NAME, FLAG, VALUE};

  ProgramArgumentsParser *programArgumentsParser = new ProgramArgumentsParser(3, const_cast<char **>(TEST_ARGV));

  assert(programArgumentsParser->isIntArgumentDefined(FLAG) == true);
  assert(programArgumentsParser->getIntArgument(FLAG) == 123);

  logPassedTest();
}

void shouldNotParseInvalidIntArgument() {
  logTest("should not parse invalid int argument");

  const char *FLAG = "-flag";
  const char *VALUE = "123test";

  const char *TEST_ARGV[] = {PROGRAM_NAME, FLAG, VALUE};

  ProgramArgumentsParser *programArgumentsParser = new ProgramArgumentsParser(3, const_cast<char **>(TEST_ARGV));

  assert(programArgumentsParser->isIntArgumentDefined(FLAG) == false);

  logPassedTest();
}

void shouldNotParseFlagWithoutValue() {
  logTest("should not parse flag without value");

  const char *FLAG = "-flag";

  const char *TEST_ARGV[] = {PROGRAM_NAME, FLAG};

  ProgramArgumentsParser *programArgumentsParser = new ProgramArgumentsParser(2, const_cast<char **>(TEST_ARGV));

  assert(programArgumentsParser->isArgumentDefined(FLAG) == false);

  logPassedTest();
}

void shouldNotParseFlagIfNextIsFlag() {
  logTest("should not parse flag if next value is flag");

  const char *FLAG = "-flag";
  const char *FLAG_2 = "-flag2";

  const char *TEST_ARGV[] = {PROGRAM_NAME, FLAG, FLAG_2};

  ProgramArgumentsParser *programArgumentsParser = new ProgramArgumentsParser(3, const_cast<char **>(TEST_ARGV));

  assert(programArgumentsParser->isArgumentDefined(FLAG) == false);

  logPassedTest();
}