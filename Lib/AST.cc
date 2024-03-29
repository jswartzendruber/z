#include "AST.hh"
#include "Lexer.hh"
#include <memory>

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

void PostfixExpression::print(std::ostream &os) {
  os << "PostfixExpression(" << *expr << postfixOperationToString(op) << ")";
}

void UnaryExpression::print(std::ostream &os) {
  os << "UnaryExpression(" << unaryOperationToString(op) << *expr << ")";
}

void ReturnStatement::print(std::ostream &os) {
  os << "ReturnStatement(";
  if (val.has_value()) {
    os << *val.value();
  }
  os << ")";
}

void WhileStatement::print(std::ostream &os) {
  os << "While((" << *condition << ") " << *body << ")";
}

void IfStatement::print(std::ostream &os) {
  os << "IfStatement(" << *condition << ") " << *ifTrueStmts;

  if (ifFalseStmts.has_value()) {
    os << " else " << *ifFalseStmts.value();
  }
  os << "))";
}

void LetStatement::print(std::ostream &os) {
  os << "LetStatement(" << name << " = " << *initializer << ")";
}

void ForStatement::print(std::ostream &os) {
  os << "ForStatement(" << *declaration << "; " << *condition << "; "
     << *updater << ") {" << *body << "})";
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
  for (auto &param : parameters) {
    os << *param << " ";
  }

  if (returnType.has_value()) {
    os << "-> " << returnType.value();
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
