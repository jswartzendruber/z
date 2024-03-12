#ifndef LEXER_HH
#define LEXER_HH

#include <string>
#include <string_view>
#include <optional>
#include <ostream>
#include <iostream>

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
  Identifier,
  IntegerLiteral,
  StringLiteral,
  FloatLiteral,
  CharacterLiteral,
  BooleanLiteral,
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

// Keeps track of what we have lexed so far.
struct Lexer {
  const std::string src;
  std::size_t index;

  Lexer(const std::string src) : src(src), index(0) {}

  // Advances the index within the source string, and returns the next token.
  // If there are no more tokens, returns none.
  std::optional<Token> nextToken();

private:
  // Helper to construct tokens and update their lengths when lexing.
  Token makeToken(TokenType type, std::size_t tokenLen);
};

#endif
