#include "Parser.hh"
#include "Lexer.hh"
#include <charconv>

// Acts like the rust '?' operator. Returns early from the function
// if the optional is none, otherwise assigns the optional to 'name'
// and continues execution. 
#define TRY(opt)				\
  ({						\
    auto val = opt;				\
    if (!val.has_value()) {			\
      return std::nullopt;			\
    }						\
    val.value();				\
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
  return "operationToString Unreachable.";
}

std::ostream& operator<<(std::ostream& os, const Expression& node) {
  node.print(os);
  return os;
}

void IntegerValue::print(std::ostream& os) const {
  os << "IntegerValue(" << val << ")";
}

void FloatValue::print(std::ostream& os) const {
  os << "FloatValue(" << val << ")";
}

void BinaryExpression::print(std::ostream& os) const {
  os << "BinaryExpression(" << *lhs << " " << operationToString(op) << " " << *rhs << ")";
}

std::optional<std::tuple<Operation, int, int>> infixBindingPower(TokenType type) {
  Operation op;
  int bpl;
  int bpr;

  switch (type) {
  case TokenType::Plus:
    op = Operation::Add;
    bpl = 1; bpr = 2;
    break;
  case TokenType::Minus:
    op = Operation::Sub;
    bpl = 1; bpr = 2;
    break;
  case TokenType::Star:
    op = Operation::Mul;
    bpl = 3; bpr = 4;
    break;
  case TokenType::Slash:
    bpl = 3; bpr = 4;
    op = Operation::Div;
    break;

  default:
    return std::nullopt;
  }

  return std::make_tuple(op, bpl, bpr);
}

std::optional<Expression *> Parser::parseExpressionBp(int minbp) {
  auto lhsToken = TRY(lexer.nextToken());

  Expression *lhs;
  switch (lhsToken.type) {
  case TokenType::IntegerLiteral:
    uint64_t lhsIntValue;
    std::from_chars(lhsToken.src.data(), lhsToken.src.data() + lhsToken.src.size(), lhsIntValue);
    lhs = allocator->allocate(IntegerValue(lhsIntValue));
    break;

  case TokenType::FloatLiteral:
    double lhsFloatValue;
    std::from_chars(lhsToken.src.data(), lhsToken.src.data() + lhsToken.src.size(), lhsFloatValue);
    lhs = allocator->allocate(FloatValue(lhsIntValue));
    break;

  default:
    std::cerr << "Error: Expected number, got " << lhsToken << "\n";
    return std::nullopt;
  }

  while (true) {
    std::optional<Token> optOpToken = lexer.peekToken();
    if (!optOpToken.has_value()) {
      goto end;
    }
    auto opToken = TRY(optOpToken);

    auto optIbp = infixBindingPower(opToken.type);
    if (!optIbp.has_value()) {
      std::cerr << "Error: Expected operation, got " << opToken << "\n";
      return std::nullopt;
    }
    auto ibp = TRY(optIbp);

    Operation op = std::get<0>(ibp);
    int lbp = std::get<1>(ibp);
    int rbp = std::get<2>(ibp);

    if (lbp < minbp) {
      goto end;
    }

    lexer.nextToken(); // Consume operation token
    auto rhs = TRY(parseExpressionBp(rbp));

    lhs = allocator->allocate(BinaryExpression(op, lhs, rhs));
  }

 end:
  return lhs;
}

std::optional<Expression *> Parser::parseExpression() {
  return parseExpressionBp(0);
}
