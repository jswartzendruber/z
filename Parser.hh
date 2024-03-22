#ifndef PARSER_HH
#define PARSER_HH

#include "BumpAllocator.hh"
#include "ErrorReporter.hh"
#include "Lexer.hh"

template <typename T> class LinkedList;
class Statement;

class Printable {
public:
  static int depth;
  virtual void print(std::ostream &os) = 0;
  friend std::ostream &operator<<(std::ostream &os, Printable &node);

  void printStmtBlock(std::ostream &os, LinkedList<Statement *> *stmts);

  Printable() {}
};

template <typename T> struct LinkedList {
  T elem;
  LinkedList<T> *next;

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
    IfStatement,
    ReturnStatement,
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
    Variable,
  } type;

  Expression(Expression::Type type) : type(type) {}
};

class Variable : public Expression {
public:
  std::string_view name;

  Variable(std::string_view name)
      : Expression(Expression::Type::Variable), name(name) {}
  void print(std::ostream &os);
};

class StringValue : public Expression {
public:
  std::string_view val;

  StringValue(std::string_view val)
      : Expression(Expression::Type::StringValue), val(val) {}
  void print(std::ostream &os);
};

class IntegerValue : public Expression {
public:
  uint64_t val;

  IntegerValue(uint64_t val)
      : Expression(Expression::Type::IntegerValue), val(val) {}
  void print(std::ostream &os);
};

class FloatValue : public Expression {
public:
  double val;

  FloatValue(double val) : Expression(Expression::Type::FloatValue), val(val) {}
  void print(std::ostream &os);
};

class BinaryExpression : public Expression {
public:
  Expression *lhs;
  Expression *rhs;
  Operation op;

  BinaryExpression(Operation op, Expression *lhs, Expression *rhs)
      : Expression(Expression::Type::BinaryExpression), lhs(lhs), rhs(rhs),
        op(op) {}
  void print(std::ostream &os);
  virtual ~BinaryExpression() = default;
};

class FunctionCall : public Expression, public Statement {
  std::string_view name;
  LinkedList<Expression *> *arguments;

public:
  FunctionCall(std::string_view name, LinkedList<Expression *> *arguments)
      : Expression(Expression::Type::FunctionCall),
        Statement(Statement::Type::FunctionCall), name(name),
        arguments(arguments) {}
  void print(std::ostream &os);
};

class IfStatement : public Statement {
  Expression *condition;
  LinkedList<Statement *> *ifTrueStmts;
  std::optional<LinkedList<Statement *> *> ifFalseStmts;

public:
  IfStatement(Expression *condition, LinkedList<Statement *> *ifTrueStmts,
              std::optional<LinkedList<Statement *> *> ifFalseStmts)
      : Statement(Statement::Type::IfStatement), condition(condition),
        ifTrueStmts(ifTrueStmts), ifFalseStmts(ifFalseStmts) {}
  void print(std::ostream &os);
};

class ReturnStatement : public Statement {
  Expression *val;

public:
  ReturnStatement(Expression *val)
      : Statement(Statement::Type::ReturnStatement), val(val) {}
  void print(std::ostream &os);
};

struct Parameter : public Printable {
public:
  std::string_view name;
  std::string_view type;

  Parameter(std::string_view name, std::string_view type)
      : name(name), type(type) {}
  void print(std::ostream &os);
};

class FunctionDeclaration : public Printable {
public:
  std::string_view name;
  LinkedList<Parameter *> *parameters;
  LinkedList<Statement *> *statements;
  std::optional<std::string_view> returnType;

  FunctionDeclaration(std::string_view name,
                      LinkedList<Parameter *> *parameters,
                      LinkedList<Statement *> *statements,
                      std::optional<std::string_view> returnType)
      : name(name), parameters(parameters), statements(statements),
        returnType(returnType) {}
  void print(std::ostream &os);
};

struct Parser {
private:
  BumpAllocator<> *allocator;
  Lexer *lexer;
  ErrorReporter *errorReporter;

  std::optional<FunctionCall *> parseFunctionCall(Token lhsToken);
  std::optional<LinkedList<Statement *> *> parseStatementBlock();
  std::optional<Expression *> parseExpressionBp(int minbp);
  std::optional<ReturnStatement *> parseReturnStatement();
  std::optional<IfStatement *> parseIfStatement();
  std::optional<Expression *> parseExpression();
  std::optional<Statement *> parseStatement();

public:
  Parser(BumpAllocator<> *allocator, Lexer *lexer, ErrorReporter *errorReporter)
      : allocator(allocator), lexer(lexer), errorReporter(errorReporter) {}
  std::optional<FunctionDeclaration *> parseFunctionDeclaration();
};

#endif
