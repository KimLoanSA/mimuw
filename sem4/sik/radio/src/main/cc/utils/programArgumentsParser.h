#ifndef MIMUW_S4_SIK_RADIO_PROGRAM_ARGUMENTS_PARSER_H
#define MIMUW_S4_SIK_RADIO_PROGRAM_ARGUMENTS_PARSER_H

#include <string>

class ProgramArgumentsParser {
public:
  ProgramArgumentsParser(int argc, char *argv[]);

  bool isArgumentDefined(const std::string &argumentName);
  bool isIntArgumentDefined(const std::string &argumentName);

  std::string getArgument(const std::string &argumentName);
  int getIntArgument(const std::string &argumentName);

private:
  size_t argc;
  char **argv;

  size_t getArgumentIndex(const std::string &argumentName);

  bool isArgumentNameDefined(const std::string &argumentName);
  bool isArgumentValueInt(size_t argumentNameIndex);
  bool isArgumentValueDefined(size_t argumentNameIndex);

  static bool isFlag(const std::string &value);
  static bool isArgumentNameEqual(const char *argument, const std::string &argumentName);
  static std::string toString(const char *cString);
  static bool isInt(const std::string &string);
};

#endif //MIMUW_S4_SIK_RADIO_PROGRAM_ARGUMENTS_PARSER_H
