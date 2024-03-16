#include "Parser.hh"

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
  return "Unreachable.";
}

std::ostream& operator<<(std::ostream& os, const Expression& node) {
  node.print(os);
  return os;
}

void IntegerValue::print(std::ostream& os) const {
  os << "IntegerValue(" << val << ")";
}

void BinaryExpression::print(std::ostream& os) const {
  os << "BinaryExpression(" << *lhs << " " << operationToString(op) << " " << *rhs << ")";
}

