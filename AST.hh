#ifndef AST_HH
#define AST_HH

#include "BumpAllocator.hh"
#include <cstdint>
#include <iostream>
#include <optional>
#include <string_view>

// Acts like the rust '?' operator. Returns early from the function
// if the optional is none, otherwise assigns the optional to 'name'
// and continues execution.
#define TRY(opt)                                                               \
  ({                                                                           \
    auto val = opt;                                                            \
    if (!val.has_value()) {                                                    \
      return std::nullopt;                                                     \
    }                                                                          \
    val.value();                                                               \
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
      errorReporter->report(ss.str(), peekVal.srcLine);                        \
      return std::nullopt;                                                     \
    }                                                                          \
    lexer->nextToken().value();                                                \
  })

template <typename T> class LinkedList;
class Statement;

class Printable {
public:
  static int depth;
  virtual void print(std::ostream &os) = 0;
  friend std::ostream &operator<<(std::ostream &os, Printable &node);

  void printStmtBlock(std::ostream &os, LinkedList<Statement *> stmts);

  Printable() {}
};

template <typename T> class LinkedList {
public:
  struct Node {
    T data;
    Node *nxt;

    Node(T data, Node *nxt = nullptr) : data(data), nxt(nxt) {}

    T value() { return data; }
    Node *next() { return nxt; }
  };

private:
  BumpAllocator<> *allocator;
  Node *head;
  Node *tail;
  int size;

public:
  void push_back(T val) {
    Node *node = allocator->allocate(Node(val));
    if (tail) {
      tail->nxt = node;
      tail = node;
    } else {
      head = node;
      tail = node;
    }
    size++;
  }

  LinkedList(BumpAllocator<> *allocator)
      : allocator(allocator), head(nullptr), tail(nullptr), size(0) {}

  LinkedList(BumpAllocator<> *allocator, T val)
      : allocator(allocator), head(nullptr), tail(nullptr), size(0) {
    push_back(val);
  }

  Node *front() { return head; }
  Node *back() { return tail; }
  Node *next() { return head->next(); }
  int len() { return size; }
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
    BooleanValue,
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
  LinkedList<Expression *> arguments;

public:
  FunctionCall(std::string_view name, LinkedList<Expression *> arguments)
      : Expression(Expression::Type::FunctionCall),
        Statement(Statement::Type::FunctionCall), name(name),
        arguments(arguments) {}
  void print(std::ostream &os);
};

class IfStatement : public Statement {
  Expression *condition;
  LinkedList<Statement *> ifTrueStmts;
  std::optional<LinkedList<Statement *>> ifFalseStmts;

public:
  IfStatement(Expression *condition, LinkedList<Statement *> ifTrueStmts,
              std::optional<LinkedList<Statement *>> ifFalseStmts)
      : Statement(Statement::Type::IfStatement), condition(condition),
        ifTrueStmts(ifTrueStmts), ifFalseStmts(ifFalseStmts) {}
  void print(std::ostream &os);
};

class LetStatement : public Statement {
  std::string_view name;
  std::optional<std::string_view> type;
  Expression *initializer;

public:
  LetStatement(std::string_view name, std::optional<std::string_view> type,
               Expression *initializer)
      : Statement(Statement::Type::LetStatement), name(name), type(type),
        initializer(initializer) {}
  void print(std::ostream &os);
};

class ReturnStatement : public Statement {
  std::optional<Expression *> val;

public:
  ReturnStatement(std::optional<Expression *> val)
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
  LinkedList<Parameter *> parameters;
  LinkedList<Statement *> statements;
  std::optional<std::string_view> returnType;

  FunctionDeclaration(std::string_view name, LinkedList<Parameter *> parameters,
                      LinkedList<Statement *> statements,
                      std::optional<std::string_view> returnType)
      : name(name), parameters(parameters), statements(statements),
        returnType(returnType) {}
  void print(std::ostream &os);
};

class Program : public Printable {
public:
  LinkedList<FunctionDeclaration *> functions;

  Program(LinkedList<FunctionDeclaration *> functions) : functions(functions) {}
  void print(std::ostream &os);
};

#endif