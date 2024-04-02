#ifndef ANALYZER_H
#define ANALYZER_H

#include "AST.hh"
#include "ErrorReporter.hh"

class Analyzer {
  ErrorReporter *errorReporter;
  Program *program;
  bool hadErrors;

public:
  Analyzer(ErrorReporter *errorReporter, Program *program)
      : errorReporter(errorReporter), program(program), hadErrors(false) {}
  void report(std::string msg);
  void annotateAST();
  bool anyErrors();
};

#endif