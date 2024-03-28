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

  auto arguments = LinkedList<Expression *>(allocator);

  // Collect arguments until we hit closing ')'
  std::optional<Token> token;
  while ((token = lexer->peekToken()) &&
         token.value().type != TokenType::RParen) {
    std::optional<Expression *> arg = parseExpression();
    if (arg.has_value()) {
      arguments.push_back(arg.value());
      auto peek = TRY(lexer->peekToken());

      if (peek.type == TokenType::RParen) {
        break;
      } else if (peek.type == TokenType::Comma) {
        EXPECT(TokenType::Comma);
      } else {
        std::stringstream ss;
        ss << "expected ',' or ')' when parsing function call, got: ";
        ss << "'" << peek.src << "'";
        report(ss.str(), peek.srcLine);
        return std::nullopt;
      }
    } else {
      report("expected function argument");
      return std::nullopt;
    }
  }
  EXPECT(TokenType::RParen);

  return allocator->allocate(FunctionCall(lhsToken.src, arguments));
}

std::optional<Expression *> Parser::parseExpressionBp(int minbp) {
  auto lhsToken = TRY(lexer->nextToken());
  bool negative = false;
  if (lhsToken.type == TokenType::Minus) {
    negative = true;
    lhsToken = TRY(lexer->nextToken());
  }

  // We may use this to check for a function call if the lhsToken is a string
  // literal.
  std::optional<Token> peekLparen;

  Expression *lhs;
  switch (lhsToken.type) {
  case TokenType::TrueKeyword:
    lhs = allocator->allocate(BooleanValue(true));
    break;

  case TokenType::FalseKeyword:
    lhs = allocator->allocate(BooleanValue(false));
    break;

  case TokenType::IntegerLiteral:
    int64_t lhsIntValue;
    std::from_chars(lhsToken.src.data(),
                    lhsToken.src.data() + lhsToken.src.size(), lhsIntValue);
    if (negative) {
      lhsIntValue *= -1;
    }
    lhs = allocator->allocate(IntegerValue(lhsIntValue));
    break;

  case TokenType::FloatLiteral:
    double lhsFloatValue;
    std::from_chars(lhsToken.src.data(),
                    lhsToken.src.data() + lhsToken.src.size(), lhsFloatValue);
    if (negative) {
      lhsFloatValue *= -1;
    }
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
    report(ss.str(), lhsToken.srcLine);
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

std::optional<LinkedList<Statement *>> Parser::parseStatementBlock() {
  EXPECT(TokenType::LCurly);

  auto body = LinkedList<Statement *>(allocator);

  std::optional<Token> token;
  while ((token = lexer->peekToken()) &&
         token.value().type != TokenType::RCurly) {
    std::optional<Statement *> stmt = parseStatement();

    if (stmt.has_value()) {
      body.push_back(stmt.value());
    } else if (recoveryMode) {
      // Try to find a semicolon and continue parsing
      synchronize();
    } else {
      // Can't recover, bail.
      return std::nullopt;
    }

    auto peek = TRY(lexer->peekToken());
    if (peek.type == TokenType::RCurly) {
      break;
    }
  }

  EXPECT(TokenType::RCurly);
  return body;
}

std::optional<IfStatement *> Parser::parseIfStatement() {
  EXPECT(TokenType::IfKeyword);
  EXPECT(TokenType::LParen);
  auto condition = TRY(parseExpression());
  EXPECT(TokenType::RParen);

  auto stmtsIfTrue = TRY(parseStatementBlock());

  std::optional<LinkedList<Statement *>> stmtsIfFalse = std::nullopt;
  auto peek = TRY(lexer->peekToken());
  if (peek.type == TokenType::ElseKeyword) {
    EXPECT(TokenType::ElseKeyword);
    stmtsIfFalse = TRY(parseStatementBlock());
  }

  return allocator->allocate(IfStatement(condition, stmtsIfTrue, stmtsIfFalse));
}

std::optional<ReturnStatement *> Parser::parseReturnStatement() {
  EXPECT(TokenType::ReturnKeyword);

  std::optional<Expression *> expr;
  auto peek = TRY(lexer->peekToken());
  if (peek.type == TokenType::Semicolon) {
    expr = std::nullopt;
  } else {
    expr = TRY(parseExpression());
  }

  EXPECT(TokenType::Semicolon);

  return allocator->allocate(ReturnStatement(expr));
}

std::optional<LetStatement *> Parser::parseLetStatement() {
  EXPECT(TokenType::LetKeyword);
  auto name = EXPECT(TokenType::Identifier).src;

  std::optional<std::string_view> type = std::nullopt;
  auto peek = TRY(lexer->peekToken());
  if (peek.type == TokenType::Colon) {
    EXPECT(TokenType::Colon);
    type = EXPECT(TokenType::Identifier).src;
  }

  EXPECT(TokenType::Eq);
  auto initializer = TRY(parseExpression());
  EXPECT(TokenType::Semicolon);

  return allocator->allocate(LetStatement(name, type, initializer));
}

std::optional<WhileStatement *> Parser::parseWhileStatement() {
  EXPECT(TokenType::WhileKeyword);

  EXPECT(TokenType::LParen);
  auto condition = TRY(parseExpression());
  EXPECT(TokenType::RParen);

  auto body = TRY(parseStatementBlock());

  return allocator->allocate(WhileStatement(condition, body));
}

std::optional<Statement *> Parser::parseStatement() {
  auto peekToken = TRY(lexer->peekToken());

  if (peekToken.type == TokenType::IfKeyword) {
    return TRY(parseIfStatement());
  } else if (peekToken.type == TokenType::ReturnKeyword) {
    return TRY(parseReturnStatement());
  } else if (peekToken.type == TokenType::LetKeyword) {
    return TRY(parseLetStatement());
  } else if (peekToken.type == TokenType::WhileKeyword) {
    return TRY(parseWhileStatement());
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
  auto parameters = LinkedList<Parameter *>(allocator);

  // Collect parameters until we hit closing ')'
  std::optional<Token> token;
  while ((token = lexer->peekToken()) &&
         token.value().type != TokenType::RParen) {
    auto p_name = EXPECT(TokenType::Identifier);
    EXPECT(TokenType::Colon);
    auto p_type = EXPECT(TokenType::Identifier);

    parameters.push_back(
        allocator->allocate(Parameter(p_name.src, p_type.src)));

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
      report(ss.str(), peek.srcLine);
      return std::nullopt;
    }
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

std::optional<Program *> Parser::parse() {
  auto functions = LinkedList<FunctionDeclaration *>(allocator);

  while (lexer->peekToken().has_value()) {
    auto fn = TRY(parseFunctionDeclaration());
    functions.push_back(fn);
  }

  return allocator->allocate(Program(functions));
}

/* Consumes tokens until we after we reach a semicolon. Then we should be able
 * to continue parsing statements.*/
void Parser::synchronize() {
  while (true) {
    auto peek = lexer->nextToken();

    if (peek.has_value()) {
      if (peek.value().type == TokenType::Semicolon) {
        // We found a semicolon, continue parsing normally.
        recoveryMode = false;
        return;
      }
    } else {
      // No tokens left, bail.
      return;
    }
  }
}

void Parser::report(std::string error, int line) {
  // Don't report any errors while we are recovering from another error.
  if (recoveryMode) {
    return;
  }

  hadErrors = true;
  errorReporter->report(error, line);
}

bool Parser::anyErrors() { return hadErrors; }
