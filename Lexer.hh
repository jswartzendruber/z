#ifndef LEXER_HH
#define LEXER_HH

#include <string>
#include <string_view>
#include <optional>
#include <ostream>
#include <iostream>
#include <unordered_map>
#include <cstdint>

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
  Identifier,
  IntegerLiteral,
  StringLiteral,
  FloatLiteral,
  IfKeyword,
  ElseKeyword,
  WhileKeyword,
  ForKeyword,
  MatchKeyword,
  TrueKeyword,
  FalseKeyword,
};

std::string tokenTypeName(TokenType type);

// Smallest unit of a program. The 'src' string_view must live as long as the
// source code passed into the lexer.
struct Token {
  TokenType type;
  std::string_view src;

  Token(TokenType type, const std::string_view& src): type(type), src(src) {}

  friend std::ostream& operator<<(std::ostream& os, const Token& token) {
    os << "Token { type: " << tokenTypeName(token.type) << ", src: \"" << token.src << "\" }";
    return os;
  }
};

struct UnclosedDelimiter {};

// Keeps track of what we have lexed so far.
struct LexerInternal {
  const std::string src;
  std::size_t index;
  std::size_t currentLine;
  StringTable *stringTable;

  LexerInternal(const std::string src, StringTable *stringTable) : src(src), index(0), currentLine(1), stringTable(stringTable) {}

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

public:
  Lexer(std::string code, StringTable *stringTable);

  // Returns the next token and consumes it.
  std::optional<Token> nextToken();

  // Returns the next token without consuming it.
  std::optional<Token> peekToken();

  size_t currentLine();
};

#endif
