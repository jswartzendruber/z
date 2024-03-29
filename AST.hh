#ifndef AST_HH
#define AST_HH

#include <cstdint>
#include <iostream>
#include <memory>
#include <optional>
#include <string_view>
#include <vector>

// Acts like the rust '?' operator. Returns early from the function
// if the optional is none, otherwise assigns the optional to 'name'
// and continues execution.
#define TRY(opt)                                                               \
  ({                                                                           \
    auto val = opt;                                                            \
    if (!val.has_value()) {                                                    \
      return std::nullopt;                                                     \
    }                                                                          \
    std::move(val.value());                                                    \
  })

// Looks at the next token. IF it is what is expected, consumes the
// token and returns the token. If it does not match the value,
// prints an error and returns early.
#define EXPECT(tokenType)                                                      \
  ({                                                                           \
    auto peekTok = lexer->peekToken();                                         \
    if (!peekTok.has_value()) {                                                \
      return std::nullopt;                                                     \
    }                                                                          \
    auto peekVal = peekTok.value();                                            \
    if (peekVal.type != tokenType) {                                           \
      std::stringstream ss;                                                    \
      ss << "expected " << tokenTypeName(tokenType) << ", got ";               \
      ss << tokenTypeName(peekVal.type) << " ";                                \
      ss << "'" << peekVal.src << "'";                                         \
      report(ss.str(), peekVal.srcLine);                                       \
      recoveryMode = true;                                                     \
      return std::nullopt;                                                     \
    }                                                                          \
    lexer->nextToken().value();                                                \
  })

class Statement;

class Printable {
public:
  static int depth;
  virtual void print(std::ostream &os) = 0;
  friend std::ostream &operator<<(std::ostream &os, Printable &node);

  void printStmtBlock(std::ostream &os,
                      std::vector<std::unique_ptr<Statement>> &stmts);
  virtual ~Printable() = default;
  Printable() {}
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
    LetStatement,
    WhileStatement,
  } type;

  Statement(Statement::Type type) : type(type) {}
  virtual ~Statement() = default;
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
    BooleanValue,
  } type;

  Expression(Expression::Type type) : type(type) {}
  virtual ~Expression() = default;
};

class Variable : public Expression {
public:
  std::string_view name;

  Variable(std::string_view name)
      : Expression(Expression::Type::Variable), name(name) {}
  void print(std::ostream &os);
};

class BooleanValue : public Expression {
public:
  bool val;

  BooleanValue(bool val)
      : Expression(Expression::Type::BooleanValue), val(val) {}
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
  int64_t val;

  IntegerValue(int64_t val)
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
  std::unique_ptr<Expression> lhs;
  std::unique_ptr<Expression> rhs;
  Operation op;

  BinaryExpression(Operation op, std::unique_ptr<Expression> lhs,
                   std::unique_ptr<Expression> rhs)
      : Expression(Expression::Type::BinaryExpression), lhs(std::move(lhs)),
        rhs(std::move(rhs)), op(op) {}
  void print(std::ostream &os);
};

class FunctionCall : public Expression, public Statement {
  std::string_view name;
  std::vector<std::unique_ptr<Expression>> arguments;

public:
  FunctionCall(std::string_view name,
               std::vector<std::unique_ptr<Expression>> arguments)
      : Expression(Expression::Type::FunctionCall),
        Statement(Statement::Type::FunctionCall), name(name),
        arguments(std::move(arguments)) {}
  void print(std::ostream &os);
};

class IfStatement : public Statement {
  std::unique_ptr<Expression> condition;
  std::vector<std::unique_ptr<Statement>> ifTrueStmts;
  std::optional<std::vector<std::unique_ptr<Statement>>> ifFalseStmts;

public:
  IfStatement(
      std::unique_ptr<Expression> condition,
      std::vector<std::unique_ptr<Statement>> ifTrueStmts,
      std::optional<std::vector<std::unique_ptr<Statement>>> ifFalseStmts)
      : Statement(Statement::Type::IfStatement),
        condition(std::move(condition)), ifTrueStmts(std::move(ifTrueStmts)),
        ifFalseStmts(std::move(ifFalseStmts)) {}
  void print(std::ostream &os);
};

class LetStatement : public Statement {
  std::string_view name;
  std::optional<std::string_view> type;
  std::unique_ptr<Expression> initializer;

public:
  LetStatement(std::string_view name, std::optional<std::string_view> type,
               std::unique_ptr<Expression> initializer)
      : Statement(Statement::Type::LetStatement), name(name), type(type),
        initializer(std::move(initializer)) {}
  void print(std::ostream &os);
};

class WhileStatement : public Statement {
  std::unique_ptr<Expression> condition;
  std::vector<std::unique_ptr<Statement>> body;

public:
  WhileStatement(std::unique_ptr<Expression> condition,
                 std::vector<std::unique_ptr<Statement>> body)
      : Statement(Statement::Type::WhileStatement),
        condition(std::move(condition)), body(std::move(body)) {}
  void print(std::ostream &os);
};

class ReturnStatement : public Statement {
  std::optional<std::unique_ptr<Expression>> val;

public:
  ReturnStatement(std::optional<std::unique_ptr<Expression>> val)
      : Statement(Statement::Type::ReturnStatement), val(std::move(val)) {}
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
  std::vector<std::unique_ptr<Parameter>> parameters;
  std::vector<std::unique_ptr<Statement>> statements;
  std::optional<std::string_view> returnType;

  FunctionDeclaration(std::string_view name,
                      std::vector<std::unique_ptr<Parameter>> parameters,
                      std::vector<std::unique_ptr<Statement>> statements,
                      std::optional<std::string_view> returnType)
      : name(name), parameters(std::move(parameters)),
        statements(std::move(statements)), returnType(returnType) {}
  void print(std::ostream &os);
};

class Program : public Printable {
public:
  std::vector<std::unique_ptr<FunctionDeclaration>> functions;

  Program(std::vector<std::unique_ptr<FunctionDeclaration>> functions)
      : functions(std::move(functions)) {}
  void print(std::ostream &os);
};

#endif
