#ifndef PARSER_HH
#define PARSER_HH

#include "Lexer.hh"
#include "BumpAllocator.hh"

class Printable {
public:
  virtual void print(std::ostream& os) const = 0;
  friend std::ostream& operator<<(std::ostream& os, const Printable& node);

  Printable() {}
};

template <typename T>
struct LinkedList {
  T elem;
  LinkedList<T>* next;

  LinkedList() : elem(0), next(nullptr) {}
};

enum class Operation {
  Add,
  Sub,
  Mul,
  Div,
};


class Statement : public Printable {
public:
  enum class Type {
    FunctionCall,
  } type;

  Statement(Statement::Type type) : type(type) {}
};

class Expression : public Printable {
public:
  enum class Type {
    IntegerValue,
    FloatValue,
    StringValue,
    BinaryExpression,
    FunctionCall,
  } type;

  Expression(Expression::Type type) : type(type) {}
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

class FunctionCall : public Expression, public Statement {
  std::string_view name;
  LinkedList<Expression *> *arguments;

public:
  FunctionCall(std::string_view name, LinkedList<Expression *> *arguments) : Expression(Expression::Type::FunctionCall), Statement(Statement::Type::FunctionCall), name(name), arguments(arguments) {}
  void print(std::ostream& os) const;
};

struct Parameter : public Printable {
public:
  std::string_view name;
  std::string_view type;

  Parameter(std::string_view name, std::string_view type) : name(name), type(type) {}
  void print(std::ostream& os) const;
};

class FunctionDeclaration : public Printable {
public:
  std::string_view name;
  LinkedList<Parameter *> *parameters;
  LinkedList<Statement *> *statements;
  std::optional<std::string_view> returnType;

  FunctionDeclaration(std::string_view name, LinkedList<Parameter *> *parameters, LinkedList<Statement *> *statements, std::optional<std::string_view> returnType) : name(name), parameters(parameters), statements(statements), returnType(returnType) {}
  void print(std::ostream& os) const;
};

struct Parser {
private:
  BumpAllocator *allocator;
  Lexer lexer;

  std::optional<FunctionCall *> parseFunctionCall(Token lhsToken);
  std::optional<Expression *> parseExpressionBp(int minbp);
  std::optional<Expression *> parseExpression();
  std::optional<Statement *> parseStatement();

public:
  Parser(BumpAllocator *allocator, Lexer lexer) : allocator(allocator), lexer(lexer) {}
  std::optional<FunctionDeclaration *> parseFunctionDeclaration();
};

#endif
