#include "Parser.hh"
#include "AST.hh"
#include "Lexer.hh"
#include <charconv>
#include <sstream>

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
  while ((token = lexer->peekToken()) &&
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
      auto peek = TRY(lexer->peekToken());
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
  if (curr) {
    curr->next = nullptr;
  }
  EXPECT(TokenType::RParen);

  return allocator->allocate(FunctionCall(lhsToken.src, arguments));
}

std::optional<Expression *> Parser::parseExpressionBp(int minbp) {
  auto lhsToken = TRY(lexer->nextToken());

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
    peekLparen = lexer->peekToken();
    if (peekLparen.has_value() &&
        peekLparen.value().type == TokenType::LParen) {
      return parseFunctionCall(lhsToken);
    } else {
      lhs = allocator->allocate(Variable(lhsToken.src));
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
    std::optional<Token> optOpToken = lexer->peekToken();
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

    lexer->nextToken(); // Consume operation token
    auto rhs = TRY(parseExpressionBp(rbp));

    lhs = allocator->allocate(BinaryExpression(op, lhs, rhs));
  }

  return lhs;
}

std::optional<Expression *> Parser::parseExpression() {
  return parseExpressionBp(0);
}

std::optional<LinkedList<Statement *> *> Parser::parseStatementBlock() {
  EXPECT(TokenType::LCurly);

  LinkedList<Statement *> *body = nullptr;
  auto currStmt = body;

  std::optional<Token> token;
  while ((token = lexer->peekToken()) &&
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
    auto peek = TRY(lexer->peekToken());
    if (peek.type == TokenType::RCurly) {
      break;
    }
  }
  currStmt->next = nullptr;

  EXPECT(TokenType::RCurly);
  return body;
}

std::optional<IfStatement *> Parser::parseIfStatement() {
  EXPECT(TokenType::IfKeyword);
  EXPECT(TokenType::LParen);
  auto condition = TRY(parseExpression());
  EXPECT(TokenType::RParen);

  auto stmtsIfTrue = TRY(parseStatementBlock());

  std::optional<LinkedList<Statement *> *> stmtsIfFalse = std::nullopt;
  auto peek = TRY(lexer->peekToken());
  if (peek.type == TokenType::ElseKeyword) {
    EXPECT(TokenType::ElseKeyword);
    stmtsIfFalse = TRY(parseStatementBlock());
  }

  return allocator->allocate(IfStatement(condition, stmtsIfTrue, stmtsIfFalse));
}

std::optional<ReturnStatement *> Parser::parseReturnStatement() {
  EXPECT(TokenType::ReturnKeyword);
  auto expr = TRY(parseExpression());
  EXPECT(TokenType::Semicolon);
  return allocator->allocate(ReturnStatement(expr));
}

std::optional<Statement *> Parser::parseStatement() {
  auto peekToken = TRY(lexer->peekToken());

  if (peekToken.type == TokenType::IfKeyword) {
    return TRY(parseIfStatement());
  } else if (peekToken.type == TokenType::ReturnKeyword) {
    return TRY(parseReturnStatement());
  } else if (peekToken.type == TokenType::Identifier) {
    auto lhsToken = EXPECT(TokenType::Identifier);
    auto fn = (Statement *)TRY(parseFunctionCall(lhsToken));
    EXPECT(TokenType::Semicolon);
    return fn;
  } else {
    return std::nullopt;
  }
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
  while ((token = lexer->peekToken()) &&
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
    auto peek = TRY(lexer->peekToken());
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
  if (curr) {
    curr->next = nullptr;
  }
  EXPECT(TokenType::RParen);

  // return type
  std::optional<std::string_view> returnType = std::nullopt;
  if (TRY(lexer->peekToken()).type == TokenType::Minus) {
    EXPECT(TokenType::Minus);
    EXPECT(TokenType::LAngleBracket);
    auto ty = EXPECT(TokenType::Identifier);
    returnType = ty.src;
  }

  auto body = TRY(parseStatementBlock());

  return allocator->allocate(
      FunctionDeclaration(name.src, parameters, body, returnType));
}
