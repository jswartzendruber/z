#include "Lexer.hh"
#include <cassert>

StringId StringTable::intern(std::string_view s) {
  auto id = table.find(s);

  if (id == table.end()) {
    auto strId = StringId{counter++};
    table[s] = strId;
    return strId;
  } else {
    return id->second;
  }
}

bool operator==(const StringId &lhs, const StringId &rhs) {
  return lhs.id == rhs.id;
}

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
    {TokenType::Comma, "Comma"},
    {TokenType::Colon, "Colon"},
    {TokenType::Identifier, "Identifier"},
    {TokenType::IntegerLiteral, "IntegerLiteral"},
    {TokenType::StringLiteral, "StringLiteral"},
    {TokenType::FloatLiteral, "FloatLiteral"},
    {TokenType::ReturnKeyword, "ReturnKeyword"},
    {TokenType::LetKeyword, "LetKeyword"},
    {TokenType::IfKeyword, "IfKeyword"},
    {TokenType::ElseKeyword, "ElseKeyword"},
    {TokenType::WhileKeyword, "WhileKeyword"},
    {TokenType::ForKeyword, "ForKeyword"},
    {TokenType::MatchKeyword, "MatchKeyword"},
    {TokenType::FnKeyword, "FnKeyword"},
    {TokenType::TrueKeyword, "TrueKeyword"},
    {TokenType::FalseKeyword, "FalseKeyword"},
};

std::string tokenTypeName(TokenType type) {
  assert(tokenTypeNames.size() - 1 ==
             static_cast<std::size_t>(TokenType::FalseKeyword) &&
         "Token names table is out of sync.");

  auto it = tokenTypeNames.find(type);
  if (it != tokenTypeNames.end()) {
    return it->second;
  }

  UNREACHABLE();
}

Token LexerInternal::makeToken(TokenType type, std::size_t tokenLen) {
  auto token =
      Token(type, std::string_view(src.data() + index, tokenLen), currentLine);
  index += tokenLen;
  return token;
}

Token LexerInternal::makeToken(TokenType type, std::string_view str) {
  return Token(type, str, currentLine);
}

bool isAlphanumeric(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9');
}

Token LexerInternal::makeIdentifierOrBoolean() {
  // Find full identifier and intern it
  int start = index;

  while (index < src.length() && isAlphanumeric(src[index])) {
    index++;
  }

  auto str = std::string_view(src.data() + start, index - start);
  auto strId = stringTable->intern(str);

  // Save true and false in our string table to quickly check if the
  // str is true or false. With this being a static const, true and false
  // are interned once here on the first call to this function. In future
  // calls, only the result of the function call is used, true and false
  // are not repeatedly interned.
  static const StringId trueStr = stringTable->intern("true");
  static const StringId falseStr = stringTable->intern("false");
  static const StringId fnStr = stringTable->intern("fn");
  static const StringId ifStr = stringTable->intern("if");
  static const StringId elseStr = stringTable->intern("else");
  static const StringId returnStr = stringTable->intern("return");
  static const StringId letStr = stringTable->intern("let");

  if (strId == trueStr) {
    return makeToken(TokenType::TrueKeyword, str);
  } else if (strId == falseStr) {
    return makeToken(TokenType::FalseKeyword, str);
  } else if (strId == fnStr) {
    return makeToken(TokenType::FnKeyword, str);
  } else if (strId == ifStr) {
    return makeToken(TokenType::IfKeyword, str);
  } else if (strId == elseStr) {
    return makeToken(TokenType::ElseKeyword, str);
  } else if (strId == returnStr) {
    return makeToken(TokenType::ReturnKeyword, str);
  } else if (strId == letStr) {
    return makeToken(TokenType::LetKeyword, str);
  } else {
    return makeToken(TokenType::Identifier, str);
  }
}

std::optional<Token> LexerInternal::handleWhitespace() {
  while (index < src.length()) {
    switch (src[index]) {
    case '\n':
      currentLine++;
      [[fallthrough]];
    case ' ':
    case '\t':
    case '\r':
      index++;
      break;
    default:
      return nextToken();
    }
  }

  return std::nullopt;
}

Token LexerInternal::makeNumber() {
  int len = 1; // We already checked the first character to enter this function
  bool floating = false;

  while (index + len < src.length()) {
    switch (src[index + len]) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      len++;
      break;

    case '.':
      if (floating) {
        // This is already a floating point number. Stop parsing and return what
        // we have up to this point.
        return makeToken(TokenType::FloatLiteral, len);
      } else {
        floating = true;
        len++;
        break;
      }

      // Ran out of number tokens, bail.
    default:
      goto end;
    }
  }

end:
  return makeToken(
      (floating) ? TokenType::FloatLiteral : TokenType::IntegerLiteral, len);
}

Token LexerInternal::makeString() {
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

  throw UnclosedDelimiter(currentLine);
}

std::optional<Token> LexerInternal::nextToken() {
  if (index >= src.length()) {
    return std::nullopt;
  }

  switch (src[index]) {
  case '\n':
  case '\r':
  case '\t':
  case ' ':
    return handleWhitespace();

  case '(':
    return makeToken(TokenType::LParen, 1);
  case ')':
    return makeToken(TokenType::RParen, 1);
  case '{':
    return makeToken(TokenType::LCurly, 1);
  case '}':
    return makeToken(TokenType::RCurly, 1);
  case '=':
    return makeToken(TokenType::Eq, 1);
  case '-':
    return makeToken(TokenType::Minus, 1);
  case '+':
    return makeToken(TokenType::Plus, 1);
  case '*':
    return makeToken(TokenType::Star, 1);
  case '/':
    return makeToken(TokenType::Slash, 1);
  case ';':
    return makeToken(TokenType::Semicolon, 1);
  case '>':
    return makeToken(TokenType::LAngleBracket, 1);
  case '<':
    return makeToken(TokenType::RAngleBracket, 1);
  case '!':
    return makeToken(TokenType::Bang, 1);
  case ',':
    return makeToken(TokenType::Comma, 1);
  case ':':
    return makeToken(TokenType::Colon, 1);

  case '"':
    return makeString();

  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    return makeNumber();

  case 'a':
  case 'A':
  case 'b':
  case 'B':
  case 'c':
  case 'C':
  case 'd':
  case 'D':
  case 'e':
  case 'E':
  case 'f':
  case 'F':
  case 'g':
  case 'G':
  case 'h':
  case 'H':
  case 'i':
  case 'I':
  case 'j':
  case 'J':
  case 'k':
  case 'K':
  case 'l':
  case 'L':
  case 'm':
  case 'M':
  case 'n':
  case 'N':
  case 'o':
  case 'O':
  case 'p':
  case 'P':
  case 'q':
  case 'Q':
  case 'r':
  case 'R':
  case 's':
  case 'S':
  case 't':
  case 'T':
  case 'u':
  case 'U':
  case 'v':
  case 'V':
  case 'w':
  case 'W':
  case 'x':
  case 'X':
  case 'y':
  case 'Y':
  case 'z':
  case 'Z':
    return makeIdentifierOrBoolean();

  default:
    return std::nullopt;
  };

  UNREACHABLE();
}

Lexer::Lexer(std::string code, StringTable *stringTable,
             ErrorReporter *errorReporter)
    : lexerInternal(code, stringTable, errorReporter) {
  current = lexerInternal.nextToken();
  next = lexerInternal.nextToken();
}

std::optional<Token> Lexer::nextToken() {
  std::optional<Token> res = current;
  current = next;
  next = lexerInternal.nextToken();
  return res;
}

std::optional<Token> Lexer::peekToken() { return current; }

size_t Lexer::currentLine() { return lexerInternal.currentLine; }
