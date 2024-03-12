#include "lexer.hh"
#include <unordered_map>
#include <cassert>

static const std::unordered_map<TokenType, std::string> tokenTypeNames = {
  {TokenType::LParen, "LParen"},
  {TokenType::RParen, "RParen"},
  {TokenType::LCurly, "LCurly"},
  {TokenType::RCurly, "RCurly"},
  {TokenType::Eq, "Eq"},
  {TokenType::Minus, "Minus"},
  {TokenType::Plus, "Plus"},
  {TokenType::Star, "Star"},
  {TokenType::Slash, "Slash"},
  {TokenType::Semicolon, "Semicolon"},
  {TokenType::LAngleBracket, "LAngleBracket"},
  {TokenType::RAngleBracket, "RAngleBracket"},
  {TokenType::Bang, "Bang"},
  {TokenType::Identifier, "Identifier"},
  {TokenType::IntegerLiteral, "IntegerLiteral"},
  {TokenType::StringLiteral, "StringLiteral"},
  {TokenType::FloatLiteral, "FloatLiteral"},
  {TokenType::CharacterLiteral, "CharacterLiteral"},
  {TokenType::BooleanLiteral, "BooleanLiteral"},
};

std::string tokenTypeName(TokenType type) {
  assert(tokenTypeNames.size() - 1 == static_cast<std::size_t>(TokenType::BooleanLiteral) && "Token names table is out of sync.");

  auto it = tokenTypeNames.find(type);
  if (it != tokenTypeNames.end()) {
    return it->second;
  }

  return "Unknown";
}

Token Lexer::makeToken(TokenType type, std::size_t tokenLen) {
  auto token = Token(type, std::string_view(src.data() + index, tokenLen));
  index += tokenLen;
  return token;
}

std::optional<Token> Lexer::nextToken() {
  if (index >= src.length()) {
    return std::nullopt;
  }
  
  switch (src[index]) {
  case '(': return makeToken(TokenType::LParen, 1);
  case ')': return makeToken(TokenType::RParen, 1);
  case '{': return makeToken(TokenType::LCurly, 1);
  case '}': return makeToken(TokenType::RCurly, 1);
  case '=': return makeToken(TokenType::Eq, 1);
  case '-': return makeToken(TokenType::Minus, 1);
  case '+': return makeToken(TokenType::Plus, 1);
  case '*': return makeToken(TokenType::Star, 1);
  case '/': return makeToken(TokenType::Slash, 1);
  case ';': return makeToken(TokenType::Semicolon, 1);
  case '>': return makeToken(TokenType::LAngleBracket, 1);
  case '<': return makeToken(TokenType::RAngleBracket, 1);
  case '!': return makeToken(TokenType::Bang, 1);
  };

  return std::nullopt;
}
