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
  virtual void print(std::ostream& os) const = 0;
  friend std::ostream& operator<<(std::ostream& os, const Expression& node);
};

class IntegerValue : public Expression {
public:
  size_t val;

  IntegerValue(size_t val) : Expression(Expression::Type::IntegerValue), val(val) {}
  void print(std::ostream& os) const;
};

class BinaryExpression : public Expression {
public:
  Expression *lhs;
  Expression *rhs;
  Operation op;

  BinaryExpression(Operation op, Expression *lhs, Expression *rhs) : Expression(Expression::Type::BinaryExpression), lhs(lhs), rhs(rhs), op(op) {}
  void print(std::ostream& os) const;
  virtual ~BinaryExpression() = default;
};

class Stmt {

  class IfStmt {
    
  };

  union {
    
  };
};

#endif
