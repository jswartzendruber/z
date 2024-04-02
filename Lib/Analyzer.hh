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

class AnalyzerVisitor : public ASTVisitor {
  Analyzer *analyzer;
  FunctionDeclaration *currentFunctionDeclaration;
  Program *currentProgram;

public:
  void report(std::string msg);

  PrimitiveType determineTypeOfFunctionCall(FunctionCall *expr);
  PrimitiveType determineTypeOfVariable(Variable *expr);
  PrimitiveType determineTypeOfBinaryExpression(BinaryExpression *expr);
  PrimitiveType determineTypeOfPostfixExpression(PostfixExpression *expr);
  PrimitiveType determineTypeOfUnaryExpression(UnaryExpression *expr);
  PrimitiveType determineTypeOf(Expression *expr);

  void
  visitFunctionDeclaration(FunctionDeclaration *functionDeclaration) override;
  void visitFunctionParameter(Parameter *parameter) override;
  void visitLetStatement(LetStatement *letStatement) override;
  void visitProgram(Program *program) override;

  AnalyzerVisitor(Analyzer *analyzer)
      : analyzer(analyzer), currentFunctionDeclaration(nullptr),
        currentProgram(nullptr) {}
};

#endif