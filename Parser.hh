#ifndef PARSER_HH
#define PARSER_HH

#include "Lexer.hh"
#include "BumpAllocator.hh"

template <typename T>
class TinyVector {
private:
  int size;
  int capacity;
  T* data;

public:
  TinyVector(int capacity, BumpAllocator *allocator) : size(0), capacity(capacity), data(allocator->allocateBytes<T>(capacity * sizeof(T))) {}

  void push(const T& val) {
    // TODO: do better
    if (size < capacity) {
      data[size] = val;
      size++;
    } else {
      std::cout << "TinyVector full!\n";
    }
  }

  int len() const {
    return size;
  }
  
  T& operator[](std::size_t index) {
    return data[index];
  }

  const T& operator[](std::size_t index) const {
    return data[index];
  }
};

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
    FloatValue,
    StringValue,
    BinaryExpression,
    FunctionCall,
  } type;

  Expression(Expression::Type type) : type(type) {}
  virtual void print(std::ostream& os) const = 0;
  friend std::ostream& operator<<(std::ostream& os, const Expression& node);
};

class StringValue : public Expression {
public:
  std::string_view val;

  StringValue(std::string_view val) : Expression(Expression::Type::StringValue), val(val) {}
  void print(std::ostream& os) const;
};

class IntegerValue : public Expression {
public:
  uint64_t val;

  IntegerValue(uint64_t val) : Expression(Expression::Type::IntegerValue), val(val) {}
  void print(std::ostream& os) const;
};

class FloatValue : public Expression {
public:
  double val;

  FloatValue(double val) : Expression(Expression::Type::FloatValue), val(val) {}
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

class FunctionCall : public Expression {
public:
  std::string_view name;
  TinyVector<Expression *> arguments;

  FunctionCall(std::string_view name, TinyVector<Expression *> arguments) : Expression(Expression::Type::FunctionCall), name(name), arguments(arguments) {}
  void print(std::ostream& os) const;
};

class Statement {
public:
  enum class Type {
    FunctionCall,
  } type;

  Statement(Statement::Type type) : type(type) {}
  virtual void print(std::ostream& os) const = 0;
  friend std::ostream& operator<<(std::ostream& os, const Statement& node);
};

struct Parser {
private:
  BumpAllocator *allocator;
  Lexer lexer;

  std::optional<Expression *> parseExpressionBp(int minbp);

public:
  Parser(BumpAllocator *allocator, Lexer lexer) : allocator(allocator), lexer(lexer) {}
  std::optional<Expression *> parseExpression();
};

#endif
