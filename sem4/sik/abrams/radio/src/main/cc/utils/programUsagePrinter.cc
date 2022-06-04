#include <utility>

#include "programUsagePrinter.h"

ProgramUsagePrinter::ProgramUsagePrinter(char *programName) :
  programName(programName) {}

void ProgramUsagePrinter::printUsageAndExitWith1() {
  fprintf(stderr, "%s %s %s",
    this->usageLiteral.c_str(),
    this->programName,
    this->usage.c_str());

  exit(1);
}
