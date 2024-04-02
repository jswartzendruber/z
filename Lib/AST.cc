#include "AST.hh"
#include "Lexer.hh"

std::string postfixOperationToString(PostfixOperation op) {
  switch (op) {
  case PostfixOperation::Increment:
    return "++";
  }

  UNREACHABLE();
}

std::string unaryOperationToString(UnaryOperation op) {
  switch (op) {
  case UnaryOperation::Positive:
    return "+";
  case UnaryOperation::Negative:
    return "-";
  }

  UNREACHABLE();
}

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
  case Operation::LessThan:
    return "<";
  case Operation::GreaterThan:
    return ">";
  }

  UNREACHABLE();
}

bool PrimitiveType::operator==(const PrimitiveType &other) const {
  return type == other.type;
}

std::optional<PrimitiveType>
stringToPrimitiveType(std::optional<std::string_view> in) {
  if (in.has_value()) {
    auto v = in.value();
    if (v == "bool") {
      return PrimitiveType(PrimitiveType::Type::Boolean);
    } else if (v == "String") {
      return PrimitiveType(PrimitiveType::Type::String);
    } else if (v == "") {
      return PrimitiveType(PrimitiveType::Type::Void);
    } else if (v == "i64") {
      return PrimitiveType(PrimitiveType::Type::I64);
    } else if (v == "f64") {
      return PrimitiveType(PrimitiveType::Type::F64);
    } else {
      // If the input type does not match anything else, we don't know what it
      // is.
      return std::nullopt;
    }
  } else {
    // IF the input optional is empty, the type is assumed to be void.
    return PrimitiveType(PrimitiveType::Type::Void);
  }
}

int Printable::depth = 0;

void printDepth(std::ostream &os, int n) {
  for (int i = 0; i < n; i++) {
    os << " ";
  }
}

void StatementBlock::print(std::ostream &os) {
  os << "{\n";
  depth += 2;
  for (auto &stmt : statements) {
    printDepth(os, depth);
    os << *stmt << ";\n";
  }
  depth -= 2;
  printDepth(os, depth);
  os << "}";
}

std::ostream &operator<<(std::ostream &os, Printable &node) {
  node.print(os);
  return os;
}

std::string primitiveTypeToString(PrimitiveType::Type type) {
  switch (type) {
  case PrimitiveType::Boolean:
    return "Boolean";
  case PrimitiveType::String:
    return "String";
  case PrimitiveType::Void:
    return "Void";
  case PrimitiveType::I64:
    return "i64";
  case PrimitiveType::F64:
    return "f64";
  }

  UNREACHABLE();
}

void PrimitiveType::print(std::ostream &os) {
  os << primitiveTypeToString(type);
}

void Variable::print(std::ostream &os) {
  os << "Variable(" << name;
  if (annotatedType.has_value()) {
    os << ": " << annotatedType.value();
  }
  os << ")";
}

void BooleanValue::print(std::ostream &os) {
  os << "BooleanValue(" << val << ")";
}

void StringValue::print(std::ostream &os) {
  os << "StringValue(" << val << ")";
}

void IntegerValue::print(std::ostream &os) {
  os << "IntegerValue(" << val << ")";
}

void FloatValue::print(std::ostream &os) { os << "FloatValue(" << val << ")"; }

void BinaryExpression::print(std::ostream &os) {
  os << "BinaryExpression(" << *lhs << " " << operationToString(op) << " "
     << *rhs;
  if (annotatedType.has_value()) {
    os << " type: " << annotatedType.value();
  }
  os << ")";
}

void PostfixExpression::print(std::ostream &os) {
  os << "PostfixExpression(" << *expr << postfixOperationToString(op);
  if (annotatedType.has_value()) {
    os << " type: " << annotatedType.value();
  }
  os << ")";
}

void UnaryExpression::print(std::ostream &os) {
  os << "UnaryExpression(" << unaryOperationToString(op) << *expr;
  if (annotatedType.has_value()) {
    os << " type: " << annotatedType.value();
  }
  os << ")";
}

void ReturnStatement::print(std::ostream &os) {
  os << "ReturnStatement(";
  if (val.has_value()) {
    os << *val.value();

    if (annotatedType.has_value()) {
      os << " type: " << annotatedType.value();
    }
  }
  os << ")";
}

void WhileStatement::print(std::ostream &os) {
  os << "While((" << *condition;
  if (conditionAnnotatedType.has_value()) {
    os << " type: " << conditionAnnotatedType.value();
  }
  os << ") " << *body << ")";
}

void IfStatement::print(std::ostream &os) {
  os << "IfStatement(" << *condition << ") " << *ifTrueStmts;
  if (ifFalseStmts.has_value()) {
    os << " else " << *ifFalseStmts.value();
  }
  os << "))";
}

void LetStatement::print(std::ostream &os) {
  os << "LetStatement(" << name;
  if (annotatedType.has_value()) {
    os << " type: " << annotatedType.value();
  }
  os << " = " << *initializer << ")";
}

void ForStatement::print(std::ostream &os) {
  os << "ForStatement(" << *declaration << "; " << *condition;
  os << "; " << *updater << ") {" << *body << "})";
}

void FunctionCall::print(std::ostream &os) {
  os << "FunctionCall(" << name << "(";
  int i = 0;
  for (auto &arg : arguments) {
    if (i > 0) {
      os << " ";
    }
    os << *arg;
    i++;
  }
  if (annotatedType.has_value()) {
    os << " type: " << annotatedType.value();
  }
  os << "))";
}

void Parameter::print(std::ostream &os) {
  os << "Parameter(" << name << ": " << type;
  if (annotatedType.has_value()) {
    os << " type: " << annotatedType.value();
  }
  os << ")";
}

void FunctionDeclaration::print(std::ostream &os) {
  printDepth(os, depth);
  os << "FunctionDeclaration(" << header.name << " ";
  for (auto &param : header.parameters) {
    os << *param << " ";
  }

  if (header.type.has_value()) {
    os << "-> " << header.type.value();
  } else {
    os << "-> Void";
  }
  os << ") ";

  os << *body;
  os << ")";
}

void Program::print(std::ostream &os) {
  os << "Program(\n";
  depth += 2;
  for (auto &fn : functions) {
    os << *fn << "\n";
  }
  os << ")\n";
  depth -= 2;
}

void ASTVisitor::visitProgram(Program *program) {
  for (const auto &fn : program->functions) {
    visitFunctionDeclaration(fn.get());
  }
}

void ASTVisitor::visitFunctionDeclaration(
    FunctionDeclaration *functionDeclaration) {
  for (auto &param : functionDeclaration->header.parameters) {
    visitFunctionParameter(param.get());
  }

  for (auto &statement : functionDeclaration->body.get()->statements) {
    visitStatement(statement.get());
  }
}

void ASTVisitor::visitFunctionCall(FunctionCall *functionCall) {
  (void)functionCall;
}

void ASTVisitor::visitIfStatement(IfStatement *ifStatement) {
  for (auto &statement : ifStatement->ifTrueStmts.get()->statements) {
    visitStatement(statement.get());
  }

  if (ifStatement->ifFalseStmts.has_value()) {
    for (auto &statement :
         ifStatement->ifFalseStmts.value().get()->statements) {
      visitStatement(statement.get());
    }
  }
}

void ASTVisitor::visitReturnStatement(ReturnStatement *returnStatement) {
  (void)returnStatement;
}

void ASTVisitor::visitLetStatement(LetStatement *letStatement) {
  (void)letStatement;
}

void ASTVisitor::visitWhileStatement(WhileStatement *whileStatement) {
  for (auto &statement : whileStatement->body.get()->statements) {
    visitStatement(statement.get());
  }
}

void ASTVisitor::visitForStatement(ForStatement *forStatement) {
  visitLetStatement(forStatement->declaration.get());
  for (auto &statement : forStatement->body.get()->statements) {
    visitStatement(statement.get());
  }
}

void ASTVisitor::visitStatement(Statement *statement) {
  switch (statement->type) {
  case Statement::Type::FunctionCall:
    return visitFunctionCall((FunctionCall *)statement);
  case Statement::Type::IfStatement:
    return visitIfStatement((IfStatement *)statement);
  case Statement::Type::ReturnStatement:
    return visitReturnStatement((ReturnStatement *)statement);
  case Statement::Type::LetStatement:
    return visitLetStatement((LetStatement *)statement);
  case Statement::Type::WhileStatement:
    return visitWhileStatement((WhileStatement *)statement);
  case Statement::Type::ForStatement:
    return visitForStatement((ForStatement *)statement);
  }

  UNREACHABLE();
}

void ASTVisitor::visitFunctionParameter(Parameter *parameter) {
  (void)parameter;
}
