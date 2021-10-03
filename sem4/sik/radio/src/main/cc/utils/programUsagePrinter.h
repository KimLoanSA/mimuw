#ifndef MIMUW_S4_SIK_RADIO_PROGRAMUSAGEPRINTER_H
#define MIMUW_S4_SIK_RADIO_PROGRAMUSAGEPRINTER_H

#include <string>

class ProgramUsagePrinter {

public:
  ProgramUsagePrinter(char *programName);

  void printUsageAndExitWith1();

private:
  const std::string usageLiteral = "Usage:";
  const std::string usage = "<-h host> <-r resource> <-p port> [-m yes|no] [-t timeout] [<-P port> [-B multi] [-T timeout]]";

  char *programName;
};


#endif //MIMUW_S4_SIK_RADIO_PROGRAMUSAGEPRINTER_H
