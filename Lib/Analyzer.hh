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
  void analyze();
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
  PrimitiveType determineTypeOfExpression(Expression *expr);

  void
  visitFunctionDeclaration(FunctionDeclaration *functionDeclaration) override;
  void visitWhileStatement(WhileStatement *whileStatement) override;
  void visitFunctionCall(FunctionCall *functionCall) override;
  void visitForStatement(ForStatement *forStatement) override;
  void visitFunctionParameter(Parameter *parameter) override;
  void visitLetStatement(LetStatement *letStatement) override;
  void visitIfStatement(IfStatement *ifStatement) override;
  void visitProgram(Program *program) override;

  AnalyzerVisitor(Analyzer *analyzer)
      : analyzer(analyzer), currentFunctionDeclaration(nullptr),
        currentProgram(nullptr) {}
};

#endif