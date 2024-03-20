#include "Parser.hh"
#include "Lexer.hh"
#include <charconv>
#include <sstream>

// Acts like the rust '?' operator. Returns early from the function
// if the optional is none, otherwise assigns the optional to 'name'
// and continues execution.
#define TRY(opt)                                                               \
  ({                                                                           \
    auto val = opt;                                                            \
    if (!val.has_value()) {                                                    \
      return std::nullopt;                                                     \
    }                                                                          \
    val.value();                                                               \
  })

// Looks at the next token. IF it is what is expected, consumes the
// token and returns the token. If it does not match the value,
// prints an error and returns early.
#define EXPECT(tokenType)                                                      \
  ({                                                                           \
    auto peekTok = lexer.peekToken();                                          \
    if (!peekTok.has_value()) {                                                \
      return std::nullopt;                                                     \
    }                                                                          \
    auto peekVal = peekTok.value();                                            \
    if (peekVal.type != tokenType) {                                           \
      std::cout << "Error: expected " << tokenTypeName(tokenType);             \
      std::cout << ", got " << tokenTypeName(peekVal.type) << " ";             \
      std::cout << "'" << peekVal.src << "'\n";                                \
      return std::nullopt;                                                     \
    }                                                                          \
    lexer.nextToken().value();                                                 \
  })

std::string operationToString(Operation op) {
  switch (op) {
  case Operation::Add:
    return "+";
  case Operation::Sub:
    return "-";
  case Operation::Mul:
    return "*";
  case Operation::Div:
    return "/";
  }

  UNREACHABLE();
}

std::ostream &operator<<(std::ostream &os, const Printable &node) {
  node.print(os);
  return os;
}

void StringValue::print(std::ostream &os) const {
  os << "StringValue(" << val << ")";
}

void IntegerValue::print(std::ostream &os) const {
  os << "IntegerValue(" << val << ")";
}

void FloatValue::print(std::ostream &os) const {
  os << "FloatValue(" << val << ")";
}

void BinaryExpression::print(std::ostream &os) const {
  os << "BinaryExpression(" << *lhs << " " << operationToString(op) << " "
     << *rhs << ")";
}

void FunctionCall::print(std::ostream &os) const {
  os << "FunctionCall(" << name << "(";
  auto curr = arguments;
  int i = 0;
  while (curr != nullptr) {
    if (i > 0) {
      os << " ";
    }
    os << *curr->elem;
    i++;
    curr = curr->next;
  }
  os << "))";
}

void Parameter::print(std::ostream &os) const {
  os << "Parameter(" << name << ": " << type << ")";
}

void FunctionDeclaration::print(std::ostream &os) const {
  os << "FunctionDeclaration(" << name << "(";
  auto curr = parameters;
  int i = 0;
  while (curr != nullptr) {
    os << *curr->elem << " ";
    i++;
    curr = curr->next;
  }
  if (returnType.has_value()) {
    os << "-> " << returnType.value();
  } else {
    os << "-> "
       << "Void";
  }
  os << ") {\n";
  auto currStmt = statements;
  i = 0;
  while (currStmt != nullptr) {
    os << "  " << *currStmt->elem << ";\n";
    i++;
    currStmt = currStmt->next;
  }
  os << "})";
}

std::optional<std::tuple<Operation, int, int>>
infixBindingPower(TokenType type) {
  Operation op;
  int bpl;
  int bpr;

  switch (type) {
  case TokenType::Plus:
    op = Operation::Add;
    bpl = 1;
    bpr = 2;
    break;
  case TokenType::Minus:
    op = Operation::Sub;
    bpl = 1;
    bpr = 2;
    break;
  case TokenType::Star:
    op = Operation::Mul;
    bpl = 3;
    bpr = 4;
    break;
  case TokenType::Slash:
    bpl = 3;
    bpr = 4;
    op = Operation::Div;
    break;

  default:
    return std::nullopt;
  }

  return std::make_tuple(op, bpl, bpr);
}

std::optional<FunctionCall *> Parser::parseFunctionCall(Token lhsToken) {
  EXPECT(TokenType::LParen);

  LinkedList<Expression *> *arguments = nullptr;
  auto curr = arguments;

  // Collect arguments until we hit closing ')'
  std::optional<Token> token;
  while ((token = lexer.peekToken()) &&
         token.value().type != TokenType::RParen) {
    std::optional<Expression *> arg = parseExpression();
    if (arg.has_value()) {
      auto next = allocator->allocate(LinkedList<Expression *>());
      next->elem = arg.value();

      if (arguments == nullptr) {
        arguments = next;
        curr = next;
      }

      curr->next = next;
      curr = next;
      auto peek = TRY(lexer.peekToken());
      if (peek.type == TokenType::RParen) {
        break;
      } else if (peek.type == TokenType::Comma) {
        EXPECT(TokenType::Comma);
      } else {
        std::stringstream ss;
        ss << "expected ',' or ')' when parsing function call, got: ";
        ss << "'" << peek.src << "'";
        errorReporter->report(ss.str(), peek.srcLine);
        return std::nullopt;
      }
    } else {
      errorReporter->report("expected function argument");
      return std::nullopt;
    }
  }
  curr->next = nullptr;
  EXPECT(TokenType::RParen);

  return allocator->allocate(FunctionCall(lhsToken.src, arguments));
}

std::optional<Expression *> Parser::parseExpressionBp(int minbp) {
  auto lhsToken = TRY(lexer.nextToken());

  // We may use this to check for a function call if the lhsToken is a string
  // literal.
  std::optional<Token> peekLparen;

  Expression *lhs;
  switch (lhsToken.type) {
  case TokenType::IntegerLiteral:
    uint64_t lhsIntValue;
    std::from_chars(lhsToken.src.data(),
                    lhsToken.src.data() + lhsToken.src.size(), lhsIntValue);
    lhs = allocator->allocate(IntegerValue(lhsIntValue));
    break;

  case TokenType::FloatLiteral:
    double lhsFloatValue;
    std::from_chars(lhsToken.src.data(),
                    lhsToken.src.data() + lhsToken.src.size(), lhsFloatValue);
    lhs = allocator->allocate(FloatValue(lhsFloatValue));
    break;

  case TokenType::Identifier:
    // Is this a function call, or just an identifier?
    // TODO: variables
    peekLparen = lexer.peekToken();
    if (peekLparen.has_value()) {
      if (peekLparen.value().type == TokenType::LParen) {
        // Parse function.
        return parseFunctionCall(lhsToken);
      } else {
        std::stringstream ss;
        ss << "expected '(' when parsing function call, got: ";
        ss << "'" << peekLparen.value().src << "'";
        errorReporter->report(ss.str(), peekLparen.value().srcLine);
        return std::nullopt;
      }
    }
    break;

  case TokenType::StringLiteral:
    lhs = allocator->allocate(StringValue(lhsToken.src));
    break;

  default:
    std::stringstream ss;
    ss << "expected expression, got: ";
    ss << "'" << lhsToken.src << "'";
    errorReporter->report(ss.str(), lhsToken.srcLine);
    return std::nullopt;
  }

  while (true) {
    std::optional<Token> optOpToken = lexer.peekToken();
    if (!optOpToken.has_value()) {
      return lhs;
    }
    auto opToken = TRY(optOpToken);

    auto optIbp = infixBindingPower(opToken.type);
    if (!optIbp.has_value()) {
      // Must not be any expression left to parse. Return what we have.
      return lhs;
    }
    auto ibp = TRY(optIbp);

    Operation op = std::get<0>(ibp);
    int lbp = std::get<1>(ibp);
    int rbp = std::get<2>(ibp);

    if (lbp < minbp) {
      return lhs;
    }

    lexer.nextToken(); // Consume operation token
    auto rhs = TRY(parseExpressionBp(rbp));

    lhs = allocator->allocate(BinaryExpression(op, lhs, rhs));
  }

  return lhs;
}

std::optional<Expression *> Parser::parseExpression() {
  return parseExpressionBp(0);
}

std::optional<Statement *> Parser::parseStatement() {
  auto lhsToken = TRY(lexer.nextToken());
  auto fn = (Statement *)TRY(parseFunctionCall(lhsToken));
  EXPECT(TokenType::Semicolon);
  return fn;
}

std::optional<FunctionDeclaration *> Parser::parseFunctionDeclaration() {
  EXPECT(TokenType::FnKeyword);
  auto name = EXPECT(TokenType::Identifier);
  EXPECT(TokenType::LParen);

  // collect parameters
  LinkedList<Parameter *> *parameters = nullptr;
  auto curr = parameters;

  // Collect parameters until we hit closing ')'
  std::optional<Token> token;
  while ((token = lexer.peekToken()) &&
         token.value().type != TokenType::RParen) {
    auto p_name = EXPECT(TokenType::Identifier);
    EXPECT(TokenType::Colon);
    auto p_type = EXPECT(TokenType::Identifier);

    auto next = allocator->allocate(LinkedList<Parameter *>());
    next->elem = allocator->allocate(Parameter(p_name.src, p_type.src));

    if (parameters == nullptr) {
      parameters = next;
      curr = next;
    }

    curr->next = next;
    curr = next;
    auto peek = TRY(lexer.peekToken());
    if (peek.type == TokenType::RParen) {
      break;
    } else if (peek.type == TokenType::Comma) {
      EXPECT(TokenType::Comma);
    } else {
      std::stringstream ss;
      ss << "expected ',' or ')' when parsing function declaration parameters, "
            "got: ";
      ss << "'" << peek.src << "'";
      errorReporter->report(ss.str(), peek.srcLine);
      return std::nullopt;
    }
  }
  curr->next = nullptr;
  EXPECT(TokenType::RParen);

  // return type
  std::optional<std::string_view> returnType = std::nullopt;
  if (TRY(lexer.peekToken()).type == TokenType::Minus) {
    EXPECT(TokenType::Minus);
    EXPECT(TokenType::LAngleBracket);
    auto ty = EXPECT(TokenType::Identifier);
    returnType = ty.src;
  }

  EXPECT(TokenType::LCurly);

  // collect body
  LinkedList<Statement *> *body = nullptr;
  auto currStmt = body;

  while ((token = lexer.peekToken()) &&
         token.value().type != TokenType::RCurly) {
    auto stmt = TRY(parseStatement());

    auto next = allocator->allocate(LinkedList<Statement *>());
    next->elem = stmt;

    if (body == nullptr) {
      body = next;
      currStmt = next;
    }

    currStmt->next = next;
    currStmt = next;
    auto peek = TRY(lexer.peekToken());
    if (peek.type == TokenType::RCurly) {
      break;
    }
  }
  currStmt->next = nullptr;
  EXPECT(TokenType::RCurly);

  return allocator->allocate(
      FunctionDeclaration(name.src, parameters, body, returnType));
}
