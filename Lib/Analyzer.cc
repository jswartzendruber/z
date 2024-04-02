#include "Analyzer.hh"
#include "AST.hh"
#include "Lexer.hh"
#include <sstream>

// TODO: check whiles, fors, ifs function call headers. We need to add more information
// to the ast for the call headers.

void Analyzer::report(std::string msg) {
  errorReporter->report(msg);
  hadErrors = true;
}

bool Analyzer::anyErrors() { return hadErrors; }

void AnalyzerVisitor::report(std::string msg) { analyzer->report(msg); }

PrimitiveType AnalyzerVisitor::determineTypeOfFunctionCall(FunctionCall *expr) {
  // Check the return type
  std::optional<std::string_view> fnReturnTypeStrOpt;
  if (currentProgram->symbolTable.find(expr->name) ==
      currentProgram->symbolTable.end()) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->name << "', ";
    ss << "variable '" << expr->name << "' does not have an associated type.";
    analyzer->report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  } else {
    fnReturnTypeStrOpt = currentProgram->symbolTable[expr->name];
  }

  auto fnReturnTyOpt = stringToPrimitiveType(fnReturnTypeStrOpt);

  if (!fnReturnTyOpt.has_value()) {
    std::stringstream ss;
    ss << "function '" << expr->name;
    ss << "'"
       << " has type '" << fnReturnTypeStrOpt.value()
       << "' which does not exist.";
    analyzer->report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }

  // Also check that the parameters match the function header

  return fnReturnTyOpt.value();
}

PrimitiveType AnalyzerVisitor::determineTypeOfVariable(Variable *expr) {
  std::optional<std::string_view> strTypeOpt;
  if (currentFunctionDeclaration->symbolTable.find(expr->name) ==
      currentFunctionDeclaration->symbolTable.end()) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->name << "', ";
    ss << "variable '" << expr->name << "' does not have an associated type.";
    analyzer->report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  } else {
    strTypeOpt = currentFunctionDeclaration->symbolTable[expr->name];
  }

  if (!strTypeOpt.has_value()) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->name << "', ";
    ss << "variable '" << expr->name << "' does not have an associated type.";
    analyzer->report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }

  auto strType = strTypeOpt.value();
  auto tyOpt = stringToPrimitiveType(strType);
  if (!tyOpt.has_value()) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->name << "', ";
    ss << "variable '" << expr->name << "' has type '" << strType
       << "' which does not exist.";
    analyzer->report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }

  return tyOpt.value();
}

PrimitiveType
AnalyzerVisitor::determineTypeOfBinaryExpression(BinaryExpression *expr) {
  auto lhs = determineTypeOf(expr->lhs.get());
  auto rhs = determineTypeOf(expr->rhs.get());

  if (lhs == rhs) {
    return lhs;
  } else {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->name << "', ";
    ss << "expression's left side has type '" << lhs
       << "' which does not match it's right side type of '" << rhs << "'";
    analyzer->report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }
}

PrimitiveType
AnalyzerVisitor::determineTypeOfPostfixExpression(PostfixExpression *expr) {
  // These operations only make sense on integers/floats
  auto tyOpt = determineTypeOf(expr->expr.get());

  // ty opt should always have a value here, even if void because of an
  // error from the previous determinetypeof.
  if (tyOpt.type == PrimitiveType::Type::I64 ||
      tyOpt.type == PrimitiveType::Type::F64) {
    return tyOpt;
  } else {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->name << "', ";
    ss << "expression '" << *expr << "' has type '" << tyOpt
       << "' which does not make sense. It should have a type of i64 or "
          "f64.";
    analyzer->report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }
}

PrimitiveType
AnalyzerVisitor::determineTypeOfUnaryExpression(UnaryExpression *expr) {
  // These operations only make sense on integers/floats
  auto tyOpt = determineTypeOf(expr->expr.get());

  // ty opt should always have a value here, even if void because of an
  // error from the previous determinetypeof.
  if (tyOpt.type == PrimitiveType::Type::I64 ||
      tyOpt.type == PrimitiveType::Type::F64) {
    return tyOpt;
  } else {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->name << "', ";
    ss << "expression '" << *expr << "' has type '" << tyOpt
       << "' which does not make sense. It should have a type of i64 or "
          "f64.";
    analyzer->report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }
}

PrimitiveType AnalyzerVisitor::determineTypeOf(Expression *expr) {
  switch (expr->type) {
  case Expression::Type::IntegerValue:
    return PrimitiveType(PrimitiveType::Type::I64);

  case Expression::Type::FloatValue:
    return PrimitiveType(PrimitiveType::Type::F64);

  case Expression::Type::StringValue:
    return PrimitiveType(PrimitiveType::Type::String);

  case Expression::Type::BooleanValue:
    return PrimitiveType(PrimitiveType::Type::Boolean);

  case Expression::Type::BinaryExpression:
    return determineTypeOfBinaryExpression((BinaryExpression *)expr);

  case Expression::Type::FunctionCall:
    return determineTypeOfFunctionCall((FunctionCall *)expr);

  case Expression::Type::Variable:
    return determineTypeOfVariable((Variable *)expr);

  case Expression::Type::PostfixExpression:
    return determineTypeOfPostfixExpression((PostfixExpression *)expr);

  case Expression::Type::UnaryExpression:
    return determineTypeOfUnaryExpression((UnaryExpression *)expr);
  }

  UNREACHABLE();
}

void AnalyzerVisitor::visitFunctionDeclaration(
    FunctionDeclaration *functionDeclaration) {
  currentFunctionDeclaration = functionDeclaration;
  for (auto &param : functionDeclaration->parameters) {
    visitFunctionParameter(param.get());
  }

  for (auto &statement : functionDeclaration->body.get()->statements) {
    visitStatement(statement.get());
  }
}

void AnalyzerVisitor::visitFunctionParameter(Parameter *parameter) {
  auto param_ty = stringToPrimitiveType(parameter->type);

  if (param_ty.has_value()) {
    parameter->annotatedType = param_ty.value();
  } else {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->name << "', ";
    ss << "parameter '" << parameter->name << "' has type '" << parameter->type
       << "' which does not exist.";
    report(ss.str());
  }
}

void AnalyzerVisitor::visitLetStatement(LetStatement *letStatement) {
  std::cout << *letStatement << "\n";
  auto ty = determineTypeOf(letStatement->initializer.get());

  if (letStatement->type.has_value()) {
    auto expectedTy = stringToPrimitiveType(letStatement->type);

    if (!expectedTy.has_value()) {
      std::stringstream ss;
      ss << "in function '" << currentFunctionDeclaration->name << "', ";
      ss << "let statement for '" << letStatement->name << "' has type '"
         << letStatement->type.value() << "' which does not exist.";
      report(ss.str());
    } else {
      if (ty != expectedTy.value()) {
        std::stringstream ss;
        ss << "in function '" << currentFunctionDeclaration->name << "', ";
        ss << "let statement for '" << letStatement->name
           << "' is declared with type '" << expectedTy.value()
           << "' which does not match actual type of '" << ty << "'.";
        report(ss.str());
      }
    }
  }
}

void AnalyzerVisitor::visitProgram(Program *program) {
  currentProgram = program;
  for (const auto &fn : program->functions) {
    visitFunctionDeclaration(fn.get());
  }
}

void Analyzer::annotateAST() {
  AnalyzerVisitor av = AnalyzerVisitor(this);
  av.visitProgram(program);
}