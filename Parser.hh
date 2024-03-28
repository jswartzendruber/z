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
  /* When this is set, tokens will be consumed and discarded until a semicolon
   * is reached. This is to help report multiple errors. */
  bool recoveryMode;
  /* When this is set, compilation will stop after typechecking. */
  bool hadErrors;

  std::optional<FunctionDeclaration *> parseFunctionDeclaration();
  std::optional<FunctionCall *> parseFunctionCall(Token lhsToken);
  std::optional<LinkedList<Statement *>> parseStatementBlock();
  std::optional<Expression *> parseExpressionBp(int minbp);
  std::optional<ReturnStatement *> parseReturnStatement();
  std::optional<LetStatement *> parseLetStatement();
  std::optional<IfStatement *> parseIfStatement();
  std::optional<Expression *> parseExpression();
  std::optional<Statement *> parseStatement();
  void synchronize();

  void report(std::string error, int line = 0);

public:
  Parser(BumpAllocator<> *allocator, Lexer *lexer, ErrorReporter *errorReporter)
      : allocator(allocator), lexer(lexer), errorReporter(errorReporter),
        recoveryMode(false), hadErrors(false) {}

  std::optional<Program *> parse();

  bool anyErrors();
};

#endif
