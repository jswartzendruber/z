#ifndef PARSER_HH
#define PARSER_HH

#include "AST.hh"
#include "BumpAllocator.hh"
#include "ErrorReporter.hh"
#include "Lexer.hh"

struct Parser {
private:
  BumpAllocator<> *allocator;
  Lexer *lexer;
  ErrorReporter *errorReporter;

  std::optional<FunctionDeclaration *> parseFunctionDeclaration();
  std::optional<FunctionCall *> parseFunctionCall(Token lhsToken);
  std::optional<LinkedList<Statement *>> parseStatementBlock();
  std::optional<Expression *> parseExpressionBp(int minbp);
  std::optional<ReturnStatement *> parseReturnStatement();
  std::optional<LetStatement *> parseLetStatement();
  std::optional<IfStatement *> parseIfStatement();
  std::optional<Expression *> parseExpression();
  std::optional<Statement *> parseStatement();

public:
  Parser(BumpAllocator<> *allocator, Lexer *lexer, ErrorReporter *errorReporter)
      : allocator(allocator), lexer(lexer), errorReporter(errorReporter) {}
  std::optional<Program *> parse();
};

#endif
