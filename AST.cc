#include "AST.hh"
#include "Lexer.hh"
#include <memory>

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

  UNREACHABLE();
}

int Printable::depth = 0;

void printDepth(std::ostream &os, int n) {
  for (int i = 0; i < n; i++) {
    os << " ";
  }
}

void Printable::printStmtBlock(
    std::ostream &os, std::vector<std::unique_ptr<Statement>>& stmts) {
  os << "{\n";
  depth += 2;
  for (auto &stmt : stmts) {
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

void Variable::print(std::ostream &os) { os << "Variable(" << name << ")"; }

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
     << *rhs << ")";
}

void ReturnStatement::print(std::ostream &os) {
  os << "ReturnStatement(";
  if (val.has_value()) {
    os << *val.value();
  }
  os << ")";
}

void WhileStatement::print(std::ostream &os) {
  os << "While((" << *condition << ") ";
  printStmtBlock(os, body);
  os << ")";
}

void IfStatement::print(std::ostream &os) {
  os << "IfStatement(" << *condition << ") ";
  printStmtBlock(os, ifTrueStmts);

  if (ifFalseStmts.has_value()) {
    os << " else ";
    printStmtBlock(os, ifFalseStmts.value());
  }
  os << "))";
}

void LetStatement::print(std::ostream &os) {
  os << "LetStatement(" << name << " = " << *initializer << ")";
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
  os << "))";
}

void Parameter::print(std::ostream &os) {
  os << "Parameter(" << name << ": " << type << ")";
}

void FunctionDeclaration::print(std::ostream &os) {
  printDepth(os, depth);
  os << "FunctionDeclaration(" << name << " ";
  int i = 0;
  for (auto &param : parameters) {
    os << *param << " ";
    i++;
  }

  if (returnType.has_value()) {
    os << "-> " << returnType.value();
  } else {
    os << "-> Void";
  }
  os << ") ";

  printStmtBlock(os, statements);

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
