#ifndef LEXER_HH
#define LEXER_HH

#include "ErrorReporter.hh"
#include <cstdint>
#include <iostream>
#include <optional>
#include <ostream>
#include <string_view>
#include <unordered_map>

#define UNREACHABLE()                                                          \
  std::cerr << "Unreachable tripped: ";                                        \
  std::cerr << __FILE__ << ":";                                                \
  std::cerr << __LINE__ << "\n";                                               \
  exit(1);

// An ID that represents a string inside the StringTable.
struct StringId {
  uint32_t id;
};

// Used to intern strings for fast equality checking
struct StringTable {
  std::unordered_map<std::string_view, StringId> table;
  uint32_t counter;

  StringTable() : table(), counter(0) {}

  StringId intern(std::string_view s);
};

// BooleanLiteral must be the last token defined in this enum.
// This has to do with the assert in lexer.cc, keeping the
// token type names in sync with this enum.
enum class TokenType {
  LParen,
  RParen,
  LCurly,
  RCurly,
  Eq,
  Minus,
  Plus,
  Star,
  Slash,
  Semicolon,
  LAngleBracket,
  RAngleBracket,
  Bang,
  Comma,
  Colon,
  Identifier,
  IntegerLiteral,
  StringLiteral,
  FloatLiteral,
  ReturnKeyword,
  IfKeyword,
  ElseKeyword,
  WhileKeyword,
  ForKeyword,
  MatchKeyword,
  FnKeyword,
  TrueKeyword,
  FalseKeyword,
};

std::string tokenTypeName(TokenType type);

// Smallest unit of a program. The 'src' string_view must live as long as the
// source code passed into the lexer.
struct Token {
  TokenType type;
  std::string_view src;
  int srcLine;

  Token(TokenType type, const std::string_view &src, int srcLine)
      : type(type), src(src), srcLine(srcLine) {}

  friend std::ostream &operator<<(std::ostream &os, const Token &token) {
    os << "Token { type: " << tokenTypeName(token.type) << ", src: \""
       << token.src << "\" }";
    return os;
  }
};

struct UnclosedDelimiter {
  int line;

  UnclosedDelimiter(int line) : line(line) {}
};

// Keeps track of what we have lexed so far.
struct LexerInternal {
  const std::string src;
  unsigned long index;
  unsigned long currentLine;
  StringTable *stringTable;
  ErrorReporter *errorReporter;

  LexerInternal(const std::string src, StringTable *stringTable,
                ErrorReporter *errorReporter)
      : src(src), index(0), currentLine(1), stringTable(stringTable),
        errorReporter(errorReporter) {}

  // Advances the index within the source string, and returns the next token.
  // If there are no more tokens, returns none.
  std::optional<Token> nextToken();

private:
  // Helper to construct tokens and update their lengths when lexing. This also
  // updates the current index in the lexer with the tokenLen.
  Token makeToken(TokenType type, std::size_t tokenLen);

  // Helper to construct tokens. This method does NOT update the current lexer
  // index for you.
  Token makeToken(TokenType type, std::string_view src);

  Token makeIdentifierOrBoolean();
  Token makeNumber();
  Token makeString();

  // Consumes whitespace until it reaches any other token, then calls
  // Lexer::nextToken and returns the result.
  std::optional<Token> handleWhitespace();
};

struct Lexer {
private:
  LexerInternal lexerInternal;
  std::optional<Token> current;
  std::optional<Token> next;
  ErrorReporter *errorReporter;

public:
  Lexer(std::string code, StringTable *stringTable,
        ErrorReporter *errorReporter);

  // Returns the next token and consumes it.
  std::optional<Token> nextToken();

  // Returns the next token without consuming it.
  std::optional<Token> peekToken();

  size_t currentLine();
};

#endif
