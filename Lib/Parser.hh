#ifndef PARSER_HH
#define PARSER_HH

#include "AST.hh"
#include "ErrorReporter.hh"
#include "Lexer.hh"
#include <memory>

struct Parser {
private:
  Lexer *lexer;
  ErrorReporter *errorReporter;
  /* When this is set, tokens will be consumed and discarded until a semicolon
   * is reached. This is to help report multiple errors. */
  bool recoveryMode;
  /* When this is set, compilation will stop after typechecking. */
  bool hadErrors;

  std::optional<std::unique_ptr<FunctionDeclaration>>
  parseFunctionDeclaration();
  std::optional<std::unique_ptr<FunctionCall>>
  parseFunctionCall(Token lhsToken);
  std::optional<std::unique_ptr<Expression>> parseExpressionBp(int minbp);
  std::optional<std::unique_ptr<ReturnStatement>> parseReturnStatement();
  std::optional<std::unique_ptr<StatementBlock>> parseStatementBlock();
  std::optional<std::unique_ptr<WhileStatement>> parseWhileStatement();
  std::optional<std::unique_ptr<ForStatement>> parseForStatement();
  std::optional<std::unique_ptr<LetStatement>> parseLetStatement();
  std::optional<std::unique_ptr<IfStatement>> parseIfStatement();
  std::optional<std::unique_ptr<Expression>> parseExpression();
  std::optional<std::unique_ptr<Statement>> parseStatement();
  void synchronize();

  void report(std::string error, int line = 0);

public:
  Parser(Lexer *lexer, ErrorReporter *errorReporter)
      : lexer(lexer), errorReporter(errorReporter), recoveryMode(false),
        hadErrors(false) {}

  std::optional<Program> parse();

  bool anyErrors();
};

#endif
