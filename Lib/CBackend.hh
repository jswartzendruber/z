#ifndef CBACKEND_HH
#define CBACKEND_HH

#include "AST.hh"
#include <sstream>

class EmitterVisitor : public ASTVisitor {
  std::stringstream *genSrc;
  int depth;

  void printDepth();

public:
  void
  visitFunctionDeclaration(FunctionDeclaration *functionDeclaration) override;
  //   void visitWhileStatement(WhileStatement *whileStatement) override;
  void visitFunctionCall(FunctionCall *functionCall) override;
  //   void visitForStatement(ForStatement *forStatement) override;
  void visitReturnStatement(ReturnStatement *returnStatement) override;
  void visitFunctionParameter(Parameter *parameter) override;
  void visitLetStatement(LetStatement *letStatement) override;
  void visitIfStatement(IfStatement *ifStatement) override;
  void visitProgram(Program *program) override;
  void visitIntegerValue(IntegerValue *integerValue) override;
  void visitFloatValue(FloatValue *floatValue) override;
  void visitStringValue(StringValue *stringValue) override;
  void visitBinaryExpression(BinaryExpression *binaryExpression) override;
  void visitVariable(Variable *variable) override;
  void visitBooleanValue(BooleanValue *booleanValue) override;
  void visitPostfixExpression(PostfixExpression *postfixExpression) override;
  void visitUnaryExpression(UnaryExpression *unaryExpression) override;

  void statementBlock(StatementBlock *stmts);

  EmitterVisitor(std::stringstream *genSrc) : genSrc(genSrc), depth(0) {}
};

class CBackend {
  Program *ast;
  std::stringstream genSrc;

public:
  CBackend(Program *ast) : ast(ast), genSrc() {}
  std::string emit();
};

#endif