#ifndef ERROR_REPORTER_HH
#define ERROR_REPORTER_HH

#include <string>

class ErrorReporter {
  std::string fileName;
  int errors;

public:
  ErrorReporter() : fileName(""), errors(0) {}
  ErrorReporter(std::string fileName) : fileName(fileName), errors(0) {}
  void report(std::string message, int line = 0);
  void setFileName(std::string fileName);
};

#endif