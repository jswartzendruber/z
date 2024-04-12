#ifndef AST_HH
#define AST_HH

#include <cstdint>
#include <iostream>
#include <memory>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <vector>

// Acts like the rust '?' operator. Returns early from the function
// if the optional is none, otherwise assigns the optional to 'name'
// and continues execution.
#define TRY(opt)                                                               \
  ({                                                                           \
    auto val = opt;                                                            \
    if (!val.has_value()) {                                                    \
      std::stringstream ss;                                                    \
      ss << __FILE__ << ":" << __LINE__ << " expected a value for " << #opt;   \
      report(ss.str());                                                        \
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

class Printable {
public:
  static int depth;
  virtual void print(std::ostream &os) = 0;
  friend std::ostream &operator<<(std::ostream &os, Printable &node);
  virtual ~Printable() = default;
  Printable() {}
};

class PrimitiveType : public Printable {
public:
  enum Type {
    Boolean,
    String,
    Void,
    I64,
    I32,
    F64,
    F32,
  } type;

  PrimitiveType() = delete;
  PrimitiveType(PrimitiveType::Type type) : type(type) {}
  void print(std::ostream &os);
  bool operator==(const PrimitiveType &other) const;
  bool matchesOrCoercesTo(PrimitiveType ty);
};

std::optional<PrimitiveType>
stringToPrimitiveType(std::optional<std::string_view> in);

enum class PostfixOperation {
  Increment,
};

std::string postfixOperationToString(PostfixOperation op);

enum class UnaryOperation {
  Positive,
  Negative,
};

std::string unaryOperationToString(UnaryOperation op);

enum class Operation {
  Add,
  Sub,
  Mul,
  Div,
  LessThan,
  GreaterThan,
};

std::string operationToString(Operation op);

class Statement : public Printable {
public:
  enum class Type {
    FunctionCall,
    IfStatement,
    ReturnStatement,
    LetStatement,
    WhileStatement,
    ForStatement,
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
    PostfixExpression,
    UnaryExpression,
  } type;

  Expression(Expression::Type type) : type(type) {}
  virtual ~Expression() = default;
};

class Variable : public Expression {
public:
  std::string_view name;

  std::optional<PrimitiveType> annotatedType;

  Variable(std::string_view name)
      : Expression(Expression::Type::Variable), name(name),
        annotatedType(std::nullopt) {}
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

  std::optional<PrimitiveType> annotatedType;

  BinaryExpression(Operation op, std::unique_ptr<Expression> lhs,
                   std::unique_ptr<Expression> rhs)
      : Expression(Expression::Type::BinaryExpression), lhs(std::move(lhs)),
        rhs(std::move(rhs)), op(op), annotatedType(std::nullopt) {}
  void print(std::ostream &os);
};

class PostfixExpression : public Expression {
public:
  std::unique_ptr<Expression> expr;
  PostfixOperation op;

  std::optional<PrimitiveType> annotatedType;

  PostfixExpression(std::unique_ptr<Expression> expr, PostfixOperation op)
      : Expression(Expression::Type::PostfixExpression), expr(std::move(expr)),
        op(op), annotatedType(std::nullopt) {}
  void print(std::ostream &os);
};

class UnaryExpression : public Expression {
public:
  std::unique_ptr<Expression> expr;
  UnaryOperation op;

  std::optional<PrimitiveType> annotatedType;

  UnaryExpression(std::unique_ptr<Expression> expr, UnaryOperation op)
      : Expression(Expression::Type::UnaryExpression), expr(std::move(expr)),
        op(op), annotatedType(std::nullopt) {}
  void print(std::ostream &os);
};

class FunctionCall : public Expression, public Statement {
public:
  std::string_view name;
  std::vector<std::unique_ptr<Expression>> arguments;

  std::optional<PrimitiveType> annotatedType;

  FunctionCall(std::string_view name,
               std::vector<std::unique_ptr<Expression>> arguments)
      : Expression(Expression::Type::FunctionCall),
        Statement(Statement::Type::FunctionCall), name(name),
        arguments(std::move(arguments)), annotatedType(std::nullopt) {}
  void print(std::ostream &os);
};

class StatementBlock : public Printable {
public:
  std::vector<std::unique_ptr<Statement>> statements;

  StatementBlock(std::vector<std::unique_ptr<Statement>> statements)
      : statements(std::move(statements)) {}
  void print(std::ostream &os);
};

class IfStatement : public Statement {
public:
  std::unique_ptr<Expression> condition;
  std::unique_ptr<StatementBlock> ifTrueStmts;
  std::optional<std::unique_ptr<StatementBlock>> ifFalseStmts;

  IfStatement(std::unique_ptr<Expression> condition,
              std::unique_ptr<StatementBlock> ifTrueStmts,
              std::optional<std::unique_ptr<StatementBlock>> ifFalseStmts)
      : Statement(Statement::Type::IfStatement),
        condition(std::move(condition)), ifTrueStmts(std::move(ifTrueStmts)),
        ifFalseStmts(std::move(ifFalseStmts)) {}
  void print(std::ostream &os);
};

class LetStatement : public Statement {
public:
  std::string_view name;
  std::optional<std::string_view> type;
  std::unique_ptr<Expression> initializer;

  std::optional<PrimitiveType> annotatedType;

  LetStatement(std::string_view name, std::optional<std::string_view> type,
               std::unique_ptr<Expression> initializer)
      : Statement(Statement::Type::LetStatement), name(name), type(type),
        initializer(std::move(initializer)), annotatedType(std::nullopt) {}
  void print(std::ostream &os);
};

class WhileStatement : public Statement {
public:
  std::unique_ptr<Expression> condition;
  std::unique_ptr<StatementBlock> body;

  std::optional<PrimitiveType> conditionAnnotatedType;

  WhileStatement(std::unique_ptr<Expression> condition,
                 std::unique_ptr<StatementBlock> body)
      : Statement(Statement::Type::WhileStatement),
        condition(std::move(condition)), body(std::move(body)),
        conditionAnnotatedType(std::nullopt) {}
  void print(std::ostream &os);
};

class ForStatement : public Statement {
public:
  std::unique_ptr<LetStatement> declaration;
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Expression> updater;
  std::unique_ptr<StatementBlock> body;

  ForStatement(std::unique_ptr<LetStatement> declaration,
               std::unique_ptr<Expression> condition,
               std::unique_ptr<Expression> updater,
               std::unique_ptr<StatementBlock> body)
      : Statement(Statement::Type::ForStatement),
        declaration(std::move(declaration)), condition(std::move(condition)),
        updater(std::move(updater)), body(std::move(body)) {}
  void print(std::ostream &os);
};

class ReturnStatement : public Statement {
public:
  std::optional<std::unique_ptr<Expression>> val;

  std::optional<PrimitiveType> annotatedType;

  ReturnStatement(std::optional<std::unique_ptr<Expression>> val)
      : Statement(Statement::Type::ReturnStatement), val(std::move(val)),
        annotatedType(std::nullopt) {}
  void print(std::ostream &os);
};

struct Parameter : public Printable {
public:
  std::string_view name;
  std::string_view type;

  std::optional<PrimitiveType> annotatedType;

  Parameter(std::string_view name, std::string_view type)
      : name(name), type(type), annotatedType(std::nullopt) {}
  void print(std::ostream &os);
};

class FunctionHeader {
public:
  std::string_view name;
  std::optional<std::string_view> type;
  std::vector<std::unique_ptr<Parameter>> parameters;

  std::optional<PrimitiveType> annotatedType;

  FunctionHeader(std::string_view name, std::optional<std::string_view> type,
                 std::vector<std::unique_ptr<Parameter>> parameters)
      : name(name), type(type), parameters(std::move(parameters)),
        annotatedType(std::nullopt) {}
};

class FunctionDeclaration : public Printable {
public:
  FunctionHeader header;
  std::unique_ptr<StatementBlock> body;

  // Contains variable type declarations, parameter declarations.
  std::unordered_map<std::string_view, std::optional<std::string_view>>
      symbolTable;

  std::unordered_map<std::string_view, PrimitiveType> annotatedTypes;

  FunctionDeclaration(
      FunctionHeader header, std::unique_ptr<StatementBlock> body,
      std::unordered_map<std::string_view, std::optional<std::string_view>>
          symbolTable)
      : header(std::move(header)), body(std::move(body)),
        symbolTable(std::move(symbolTable)), annotatedTypes() {}
  void print(std::ostream &os);
};

class Program : public Printable {
public:
  std::vector<std::unique_ptr<FunctionDeclaration>> functions;

  // Contains function type declarations
  std::unordered_map<std::string_view, FunctionHeader *> symbolTable;

  Program(std::vector<std::unique_ptr<FunctionDeclaration>> functions,
          std::unordered_map<std::string_view, FunctionHeader *> symbolTable)
      : functions(std::move(functions)), symbolTable(std::move(symbolTable)) {}
  void print(std::ostream &os);
};

class ASTVisitor {
public:
  virtual void visitProgram(Program *program);
  virtual void
  visitFunctionDeclaration(FunctionDeclaration *functionDeclaration);
  virtual void visitFunctionParameter(Parameter *parameter);
  virtual void visitStatement(Statement *statement);
  virtual void visitLetStatement(LetStatement *letStatement);
  virtual void visitExpression(Expression *expression);
  virtual void visitFunctionCall(FunctionCall *functionCall);
  virtual void visitIfStatement(IfStatement *ifStatement);
  virtual void visitReturnStatement(ReturnStatement *returnStatement);
  virtual void visitWhileStatement(WhileStatement *whileStatement);
  virtual void visitForStatement(ForStatement *forStatement);
  virtual void visitIntegerValue(IntegerValue *integerValue);
  virtual void visitFloatValue(FloatValue *floatValue);
  virtual void visitStringValue(StringValue *stringValue);
  virtual void visitBinaryExpression(BinaryExpression *binaryExpression);
  virtual void visitVariable(Variable *variable);
  virtual void visitBooleanValue(BooleanValue *booleanValue);
  virtual void visitPostfixExpression(PostfixExpression *postfixExpression);
  virtual void visitUnaryExpression(UnaryExpression *unaryExpression);
};

#endif
