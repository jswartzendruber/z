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
  {TokenType::BooleanTrue, "BooleanTrue"},
  {TokenType::BooleanFalse, "BooleanFalse"},
};

std::string tokenTypeName(TokenType type) {
  assert(tokenTypeNames.size() - 1 == static_cast<std::size_t>(TokenType::BooleanFalse) && "Token names table is out of sync.");

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

bool isAlphanumeric(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9');
}

Token Lexer::makeIdentifier() {
  int len = 1; // We already checked the first character to enter this function

  while (index + len < src.length() && isAlphanumeric(src[index + len])) {
    len++;
  }

  return makeToken(TokenType::Identifier, len);
}

std::optional<Token> Lexer::handleWhitespace() {
  index++; // Consume the whitespace character that caused us to enter this function

  while (index < src.length()) {
    switch (src[index]) {
    case '\n':
      currentLine++;
      [[fallthrough]];
    case ' ':
    case '\t':
    case '\r':
      index++; break;
    default:
      return nextToken();
    }
  }

  return std::nullopt;
}

Token Lexer::makeNumber() {
  int len = 1; // We already checked the first character to enter this function
  bool floating = false;

  while (index + len < src.length()) {
    switch (src[index + len]) {
    case '0': case '1':
    case '2': case '3':
    case '4': case '5':
    case '6': case '7':
    case '8': case '9':
      len++; break;

    case '.':
      if (floating) {
	// This is already a floating point number. Stop parsing and return what we have up to this point.
	return makeToken(TokenType::FloatLiteral, len);
      } else {
	floating = true;
	len++; break;
      }

    default:
      return makeToken((floating) ? TokenType::FloatLiteral : TokenType::IntegerLiteral, len);
    }
  }

  assert(0 && "Unreachable.");
}

Token Lexer::makeString() {
  int len = 1; // The opening quote

  while (index + len < src.length()) {
    switch (src[index + len]) {
    case '"':
      len++;
      return makeToken(TokenType::StringLiteral, len);
    default:
      len++;
    }
  }

  throw UnclosedDelimiter{};
}

std::optional<Token> Lexer::nextToken() {
  if (index >= src.length()) {
    return std::nullopt;
  }
  
  switch (src[index]) {
  case '\n':
    currentLine++;
    [[fallthrough]];
  case ' ':
  case '\t':
  case '\r':
    return handleWhitespace();

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

  case '"':
    return makeString();

  case '0': case '1':
  case '2': case '3':
  case '4': case '5':
  case '6': case '7':
  case '8': case '9':
    return makeNumber();

  case 't':
    if (index + 3 < src.length() &&
	src[index + 1] == 'r' &&
	src[index + 2] == 'u' &&
	src[index + 3] == 'e') {
      return makeToken(TokenType::BooleanTrue, 4); 
    }
    [[fallthrough]];
  case 'f':
    if (index + 4 < src.length() &&
	src[index + 1] == 'a' &&
	src[index + 2] == 'l' &&
	src[index + 3] == 's' &&
	src[index + 4] == 'e') {
      return makeToken(TokenType::BooleanFalse, 5); 
    }
    [[fallthrough]];
  case 'a': case 'A':
  case 'b': case 'B':
  case 'c': case 'C':
  case 'd': case 'D':
  case 'e': case 'E':
  case 'F':
  case 'g': case 'G':
  case 'h': case 'H':
  case 'i': case 'I':
  case 'j': case 'J':
  case 'k': case 'K':
  case 'l': case 'L':
  case 'm': case 'M':
  case 'n': case 'N':
  case 'o': case 'O':
  case 'p': case 'P':
  case 'q': case 'Q':
  case 'r': case 'R':
  case 's': case 'S':
  case 'T':
  case 'u': case 'U':
  case 'v': case 'V':
  case 'w': case 'W':
  case 'x': case 'X':
  case 'y': case 'Y':
  case 'z': case 'Z':
    return makeIdentifier();

  default:
    return std::nullopt;

  };

  assert(0 && "Unreachable.");
}
