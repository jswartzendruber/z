#include "ErrorReporter.hh"
#include <iostream>

void ErrorReporter::setFileName(std::string name) { fileName = name; }

void ErrorReporter::report(std::string message, int line) {
  errors++;
  std::cerr << "\033[1;31merror: \033[0m" << message << "\n";
  if (line > 0) {
    std::cerr << "  --> " << fileName << ":" << line << "\n";
    std::cerr << "\n";
  }
}