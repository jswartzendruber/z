#include "AST.hh"
#include "Lexer.hh"

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

void Printable::printStmtBlock(std::ostream &os,
                               LinkedList<Statement *> stmts) {
  os << "{\n";
  auto currStmt = stmts.front();
  depth += 2;
  while (currStmt != nullptr) {
    printDepth(os, depth);
    os << *currStmt->value() << ";\n";
    currStmt = currStmt->next();
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
  os << "ReturnStatement(" << *val << ")";
}

void IfStatement::print(std::ostream &os) {
  os << "IfStatement(" << *condition << ") ";
  printStmtBlock(os, ifTrueStmts);

  if (ifFalseStmts.has_value()) {
    os << " else ";
    auto stmts = ifFalseStmts.value();
    printStmtBlock(os, stmts);
  }
  os << "))";
}

void FunctionCall::print(std::ostream &os) {
  os << "FunctionCall(" << name << "(";
  auto curr = arguments.front();
  int i = 0;
  while (curr != nullptr) {
    if (i > 0) {
      os << " ";
    }
    os << *curr->value();
    i++;
    curr = curr->next();
  }
  os << "))";
}

void Parameter::print(std::ostream &os) {
  os << "Parameter(" << name << ": " << type << ")";
}

void FunctionDeclaration::print(std::ostream &os) {
  printDepth(os, depth);
  os << "FunctionDeclaration(" << name << " ";
  auto curr = parameters.front();
  int i = 0;
  while (curr != nullptr) {
    os << *curr->value() << " ";
    i++;
    curr = curr->next();
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
  auto curr = functions.front();
  while (curr != nullptr) {
    os << *curr->value() << "\n";
    curr = curr->next();
  }
  os << ")\n";
  depth -= 2;
}