#include "Parser.hh"
#include <sstream>

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

std::string BinaryExpression::toString() {
  std::stringstream ss;
  ss << "BinaryExpression( " << lhs->toString() << " " << operationToString(op) << " " << rhs->toString() << ")";
  return ss.str();
}
