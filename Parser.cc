#include "Parser.hh"
#include "AST.hh"
#include "Lexer.hh"
#include <charconv>
#include <memory>
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

std::optional<std::unique_ptr<FunctionCall>>
Parser::parseFunctionCall(Token lhsToken) {
  EXPECT(TokenType::LParen);

  std::vector<std::unique_ptr<Expression>> arguments = {};

  // Collect arguments until we hit closing ')'
  std::optional<Token> token;
  while ((token = lexer->peekToken()) &&
         token.value().type != TokenType::RParen) {
    std::optional<std::unique_ptr<Expression>> arg = parseExpression();
    if (arg.has_value()) {
      arguments.push_back(std::move(arg.value()));
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

  return std::make_unique<FunctionCall>(lhsToken.src, std::move(arguments));
}

std::optional<std::unique_ptr<Expression>>
Parser::parseExpressionBp(int minbp) {
  auto lhsToken = TRY(lexer->nextToken());
  bool negative = false;
  if (lhsToken.type == TokenType::Minus) {
    negative = true;
    lhsToken = TRY(lexer->nextToken());
  }

  // We may use this to check for a function call if the lhsToken is a string
  // literal.
  std::optional<Token> peekLparen;

  std::unique_ptr<Expression> lhs;
  switch (lhsToken.type) {
  case TokenType::TrueKeyword:
    lhs = std::make_unique<BooleanValue>(true);
    break;

  case TokenType::FalseKeyword:
    lhs = std::make_unique<BooleanValue>(false);
    break;

  case TokenType::IntegerLiteral:
    int64_t lhsIntValue;
    std::from_chars(lhsToken.src.data(),
                    lhsToken.src.data() + lhsToken.src.size(), lhsIntValue);
    if (negative) {
      lhsIntValue *= -1;
    }
    lhs = std::make_unique<IntegerValue>(lhsIntValue);
    break;

  case TokenType::FloatLiteral:
    double lhsFloatValue;
    std::from_chars(lhsToken.src.data(),
                    lhsToken.src.data() + lhsToken.src.size(), lhsFloatValue);
    if (negative) {
      lhsFloatValue *= -1;
    }
    lhs = std::make_unique<FloatValue>(lhsFloatValue);
    break;

  case TokenType::Identifier:
    // Is this a function call, or just an identifier?
    // TODO: variables
    peekLparen = lexer->peekToken();
    if (peekLparen.has_value() &&
        peekLparen.value().type == TokenType::LParen) {
      return TRY(parseFunctionCall(lhsToken));
    } else {
      lhs = std::make_unique<Variable>(lhsToken.src);
    }
    break;

  case TokenType::StringLiteral:
    lhs = std::make_unique<StringValue>(lhsToken.src);
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

    lhs =
        std::make_unique<BinaryExpression>(op, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

std::optional<std::unique_ptr<Expression>> Parser::parseExpression() {
  return parseExpressionBp(0);
}

std::optional<std::vector<std::unique_ptr<Statement>>>
Parser::parseStatementBlock() {
  EXPECT(TokenType::LCurly);

  std::vector<std::unique_ptr<Statement>> body = {};

  std::optional<Token> token;
  while ((token = lexer->peekToken()) &&
         token.value().type != TokenType::RCurly) {
    std::optional<std::unique_ptr<Statement>> stmt = parseStatement();

    if (stmt.has_value()) {
      body.push_back(std::move(stmt.value()));
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

std::optional<std::unique_ptr<Statement>> Parser::parseIfStatement() {
  EXPECT(TokenType::IfKeyword);
  EXPECT(TokenType::LParen);
  auto condition = TRY(parseExpression());
  EXPECT(TokenType::RParen);

  auto stmtsIfTrue = TRY(parseStatementBlock());

  std::optional<std::vector<std::unique_ptr<Statement>>> stmtsIfFalse =
      std::nullopt;
  auto peek = TRY(lexer->peekToken());
  if (peek.type == TokenType::ElseKeyword) {
    EXPECT(TokenType::ElseKeyword);
    stmtsIfFalse = TRY(parseStatementBlock());
  }

  return std::make_unique<IfStatement>(
      std::move(condition), std::move(stmtsIfTrue), std::move(stmtsIfFalse));
}

std::optional<std::unique_ptr<Statement>> Parser::parseReturnStatement() {
  EXPECT(TokenType::ReturnKeyword);

  std::optional<std::unique_ptr<Expression>> expr;
  auto peek = TRY(lexer->peekToken());
  if (peek.type == TokenType::Semicolon) {
    expr = std::nullopt;
  } else {
    expr = TRY(parseExpression());
  }

  EXPECT(TokenType::Semicolon);

  return std::make_unique<ReturnStatement>(std::move(expr));
}

std::optional<std::unique_ptr<Statement>> Parser::parseLetStatement() {
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

  return std::make_unique<LetStatement>(name, type, std::move(initializer));
}

std::optional<std::unique_ptr<WhileStatement>> Parser::parseWhileStatement() {
  EXPECT(TokenType::WhileKeyword);

  EXPECT(TokenType::LParen);
  auto condition = TRY(parseExpression());
  EXPECT(TokenType::RParen);

  auto body = TRY(parseStatementBlock());

  return std::make_unique<WhileStatement>(std::move(condition),
                                          std::move(body));
}

std::optional<std::unique_ptr<Statement>> Parser::parseStatement() {
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
    auto fn = TRY(parseFunctionCall(lhsToken));
    EXPECT(TokenType::Semicolon);
    return fn;
  } else {
    return std::nullopt;
  }
}

std::optional<std::unique_ptr<FunctionDeclaration>>
Parser::parseFunctionDeclaration() {
  EXPECT(TokenType::FnKeyword);
  auto name = EXPECT(TokenType::Identifier);
  EXPECT(TokenType::LParen);

  // collect parameters
  std::vector<std::unique_ptr<Parameter>> parameters = {};

  // Collect parameters until we hit closing ')'
  std::optional<Token> token;
  while ((token = lexer->peekToken()) &&
         token.value().type != TokenType::RParen) {
    auto p_name = EXPECT(TokenType::Identifier);
    EXPECT(TokenType::Colon);
    auto p_type = EXPECT(TokenType::Identifier);

    parameters.push_back(std::move(
        std::make_unique<Parameter>(Parameter(p_name.src, p_type.src))));

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

  return std::make_unique<FunctionDeclaration>(name.src, std::move(parameters),
                                               std::move(body), returnType);
}

std::optional<Program> Parser::parse() {
  std::vector<std::unique_ptr<FunctionDeclaration>> functions;

  while (lexer->peekToken().has_value()) {
    auto fn = TRY(parseFunctionDeclaration());
    functions.push_back(std::move(fn));
  }

  return std::make_optional<Program>(std::move(functions));
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
