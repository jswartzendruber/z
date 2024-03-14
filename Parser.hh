#ifndef PARSER_HH
#define PARSER_HH

#include "Lexer.hh"

enum class Operation {
  Add,
  Sub,
  Mul,
  Div,
};

class Expression {
public:
  enum class Type {
    IntegerValue,
    BinaryExpression,
  } type;

  Expression(Expression::Type type) : type(type) {}
  virtual ~Expression() = default;
  virtual std::string toString() = 0;
};

class IntegerValue : public Expression {
public:
  size_t val;

  IntegerValue(size_t val) : Expression(Expression::Type::IntegerValue), val(val) {}
  std::string toString();
  virtual ~IntegerValue() = default;
};

class BinaryExpression : public Expression {
public:
  Expression *lhs;
  Expression *rhs;
  Operation op;

  BinaryExpression(Operation op, Expression *lhs, Expression *rhs) : Expression(Expression::Type::BinaryExpression), lhs(lhs), rhs(rhs), op(op) {}
  std::string toString();
  virtual ~BinaryExpression() = default;
};

class Stmt {

  class IfStmt {
    
  };

  union {
    
  };
};

#endif
