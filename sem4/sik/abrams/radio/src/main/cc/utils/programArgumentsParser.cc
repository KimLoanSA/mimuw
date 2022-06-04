#include "programArgumentsParser.h"

ProgramArgumentsParser::ProgramArgumentsParser(int argc, char **argv) :
  argc(argc),
  argv(argv) { }

bool ProgramArgumentsParser::isArgumentDefined(const std::string &argumentName) {
  if (!isArgumentNameDefined(argumentName)) {
    return false;
  }

  const size_t argumentNameIndex = getArgumentIndex(argumentName);

  return isArgumentValueDefined(argumentNameIndex);
}


bool ProgramArgumentsParser::isIntArgumentDefined(const std::string &argumentName) {
  if (!isArgumentNameDefined(argumentName)) {
    return false;
  }

  const size_t argumentNameIndex = getArgumentIndex(argumentName);

  return isArgumentValueInt(argumentNameIndex);
}


std::string ProgramArgumentsParser::getArgument(const std::string &argumentName) {
  const size_t argumentNameIndex = getArgumentIndex(argumentName);

  return this->argv[argumentNameIndex + 1];
}

int ProgramArgumentsParser::getIntArgument(const std::string &argumentName) {
  const size_t argumentNameIndex = getArgumentIndex(argumentName);

  return std::stoi(this->argv[argumentNameIndex + 1]);
}

bool ProgramArgumentsParser::isArgumentNameDefined(const std::string &argumentName) {
  for (size_t i = 0; i < argc; i++) {
    if (isArgumentNameEqual(argv[i], argumentName)) {
      return true;
    }
  }

  return false;
}

size_t ProgramArgumentsParser::getArgumentIndex(const std::string &argumentName) {
  for (size_t i = 0; i < argc; i++) {
    if (isArgumentNameEqual(argv[i], argumentName)) {
      return i;
    }
  }

  return 0;
}

bool ProgramArgumentsParser::isArgumentValueInt(const size_t argumentNameIndex) {
  return isArgumentValueDefined(argumentNameIndex)
    && isInt(this->argv[argumentNameIndex + 1]);
}

bool ProgramArgumentsParser::isArgumentValueDefined(const size_t argumentNameIndex) {
  return argumentNameIndex + 1 < this->argc
    && !isFlag(this->argv[argumentNameIndex + 1]);
}

bool ProgramArgumentsParser::isFlag(const std::string &value) {
  return value.rfind('-', 0) == 0;
}

bool ProgramArgumentsParser::isArgumentNameEqual(const char *argument, const std::string &argumentName) {
  return toString(argument) == argumentName;
}

std::string ProgramArgumentsParser::toString(const char *cString) {
  return std::string(cString);
}

bool ProgramArgumentsParser::isInt(const std::string &string) {
  char *parsed;
  strtol(string.c_str(), &parsed, 10);

  return *parsed == 0;
}
