#include "Analyzer.hh"
#include "AST.hh"
#include "Lexer.hh"
#include <sstream>

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
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "function call '" << expr->name
       << "' does not have an associated type.";
    report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  } else {
    fnReturnTypeStrOpt = currentProgram->symbolTable[expr->name]->type;
  }

  auto fnReturnTyOpt = stringToPrimitiveType(fnReturnTypeStrOpt);

  if (!fnReturnTyOpt.has_value()) {
    std::stringstream ss;
    ss << "function '" << expr->name;
    ss << "'"
       << " has type '" << fnReturnTypeStrOpt.value()
       << "' which does not exist.";
    report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }

  // Also check that the parameters match the function header
  auto parameters = &currentProgram->symbolTable[expr->name]->parameters;
  auto arguments = &expr->arguments;
  for (unsigned long i = 0; i < parameters->size() && i < arguments->size();
       i++) {
    auto param = (*parameters)[i].get();
    auto argument = (*arguments)[i].get();

    auto argType = determineTypeOfExpression(argument);
    if (!param->annotatedType.value().matchesOrCoercesTo(argType)) {
      std::stringstream ss;
      ss << "function '" << expr->name;
      ss << "'"
         << " has expression with type '" << argType << "' but type '"
         << *param->annotatedType << "' was expected.";
      report(ss.str());
    }
  }

  if (parameters->size() > arguments->size()) {
    std::stringstream ss;
    ss << "function '" << currentFunctionDeclaration->header.name;
    ss << "'"
       << " has function call '" << expr->name
       << "' which has too few arguments.";
    report(ss.str());
  } else if (arguments->size() > parameters->size()) {
    std::stringstream ss;
    ss << "function '" << currentFunctionDeclaration->header.name;
    ss << "'"
       << " has function call '" << expr->name
       << "' which has too many arguments.";
    report(ss.str());
  }

  return fnReturnTyOpt.value();
}

PrimitiveType AnalyzerVisitor::determineTypeOfVariable(Variable *expr) {
  // Check the annotated types first to see if we already found this value
  auto it = currentFunctionDeclaration->annotatedTypes.find(expr->name);
  if (it != currentFunctionDeclaration->annotatedTypes.end()) {
    return it->second;
  }

  std::optional<std::string_view> strTypeOpt;
  if (currentFunctionDeclaration->symbolTable.find(expr->name) ==
      currentFunctionDeclaration->symbolTable.end()) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "variable '" << expr->name << "' does not have an associated type.";
    report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  } else {
    strTypeOpt = currentFunctionDeclaration->symbolTable[expr->name];
  }

  if (!strTypeOpt.has_value()) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "variable '" << expr->name << "' does not have an associated type.";
    report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }

  auto strType = strTypeOpt.value();
  auto tyOpt = stringToPrimitiveType(strType);
  if (!tyOpt.has_value()) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "variable '" << expr->name << "' has type '" << strType
       << "' which does not exist.";
    report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }

  return tyOpt.value();
}

PrimitiveType
AnalyzerVisitor::determineTypeOfBinaryExpression(BinaryExpression *expr) {
  auto lhsTy = determineTypeOfExpression(expr->lhs.get());
  auto rhsTy = determineTypeOfExpression(expr->rhs.get());

  auto ty = lhsTy;

  switch (expr->op) {
  case Operation::LessThan:
  case Operation::GreaterThan:
    ty = PrimitiveType(PrimitiveType::Type::Boolean);
    break;

  default:
    break;
  }

  if (lhsTy.matchesOrCoercesTo(rhsTy)) {
    return ty;
  } else {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "expression's left side has type '" << lhsTy
       << "' which does not match it's right side type of '" << rhsTy << "'";
    report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }
}

PrimitiveType
AnalyzerVisitor::determineTypeOfPostfixExpression(PostfixExpression *expr) {
  // These operations only make sense on integers/floats
  auto tyOpt = determineTypeOfExpression(expr->expr.get());

  // ty opt should always have a value here, even if void because of an
  // error from the previous determinetypeof.
  if (tyOpt.type == PrimitiveType::Type::I64 ||
      tyOpt.type == PrimitiveType::Type::I32 ||
      tyOpt.type == PrimitiveType::Type::F64 ||
      tyOpt.type == PrimitiveType::Type::F32) {
    return tyOpt;
  } else {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "expression '" << *expr << "' has type '" << tyOpt
       << "' which does not make sense. It should have a type of i64 or "
          "f64.";
    report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }
}

PrimitiveType
AnalyzerVisitor::determineTypeOfUnaryExpression(UnaryExpression *expr) {
  // These operations only make sense on integers/floats
  auto tyOpt = determineTypeOfExpression(expr->expr.get());

  // ty opt should always have a value here, even if void because of an
  // error from the previous determinetypeof.
  if (tyOpt.type == PrimitiveType::Type::I64 ||
      tyOpt.type == PrimitiveType::Type::I32 ||
      tyOpt.type == PrimitiveType::Type::F64 ||
      tyOpt.type == PrimitiveType::Type::F32) {
    return tyOpt;
  } else {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "expression '" << *expr << "' has type '" << tyOpt
       << "' which does not make sense. It should have a type of i64 or "
          "f64.";
    report(ss.str());
    return PrimitiveType(PrimitiveType::Type::Void);
  }
}

PrimitiveType AnalyzerVisitor::determineTypeOfExpression(Expression *expr) {
  switch (expr->type) {
  case Expression::Type::IntegerValue:
    return PrimitiveType(PrimitiveType::Type::I32);

  case Expression::Type::FloatValue:
    return PrimitiveType(PrimitiveType::Type::F32);

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

  auto retTy = stringToPrimitiveType(functionDeclaration->header.type);

  if (retTy.has_value()) {
    functionDeclaration->header.annotatedType = retTy.value();
  } else {
    std::stringstream ss;
    ss << "function '" << currentFunctionDeclaration->header.name
       << "' has invalid return type '";
    ss << functionDeclaration->header.type.value() << "'.";
    report(ss.str());
  }

  for (auto &param : functionDeclaration->header.parameters) {
    visitFunctionParameter(param.get());
  }

  for (auto &statement : functionDeclaration->body.get()->statements) {
    visitStatement(statement.get());
  }
}

void AnalyzerVisitor::visitFunctionParameter(Parameter *parameter) {
  auto paramTy = stringToPrimitiveType(parameter->type);

  if (paramTy.has_value()) {
    parameter->annotatedType = paramTy.value();
  } else {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "parameter '" << parameter->name << "' has type '" << parameter->type
       << "' which does not exist.";
    report(ss.str());
  }
}

void AnalyzerVisitor::visitReturnStatement(ReturnStatement *returnStatement) {
  if (returnStatement->val.has_value()) {
    auto ty = determineTypeOfExpression(returnStatement->val.value().get());

    if (ty != currentFunctionDeclaration->header.annotatedType) {
      std::stringstream ss;
      ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
      ss << "return statement has type '" << ty << "' but expected type '"
         << currentFunctionDeclaration->header.annotatedType.value() << "'.";
      report(ss.str());
    }
  }
}

void AnalyzerVisitor::visitLetStatement(LetStatement *letStatement) {
  auto ty = determineTypeOfExpression(letStatement->initializer.get());

  if (letStatement->type.has_value()) {
    // IF the let statement was declared with a value, check that it
    // exists and matches the found type.
    auto expectedTy = stringToPrimitiveType(letStatement->type);

    if (!expectedTy.has_value()) {
      std::stringstream ss;
      ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
      ss << "let statement for '" << letStatement->name << "' has type '"
         << letStatement->type.value() << "' which does not exist.";
      report(ss.str());
    } else {
      if (!ty.matchesOrCoercesTo(expectedTy.value())) {
        letStatement->annotatedType = ty;
      } else {
        std::stringstream ss;
        ss << "in function '" << currentFunctionDeclaration->header.name
           << "', ";
        ss << "let statement for '" << letStatement->name
           << "' is declared with type '" << expectedTy.value()
           << "' which does not match actual type of '" << ty << "'.";
        report(ss.str());
      }
    }
  } else {
    // Annotate the let statement with the type we found. We will also
    // need to add it to the symbol table, since we know the type now.
    letStatement->annotatedType = ty;
    currentFunctionDeclaration->annotatedTypes.emplace(letStatement->name, ty);
  }
}

void AnalyzerVisitor::visitIfStatement(IfStatement *ifStatement) {
  // Check condition results in a boolean type
  auto conditionTy = determineTypeOfExpression(ifStatement->condition.get());

  if (conditionTy.type != PrimitiveType::Type::Boolean) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "if statement condition '" << *ifStatement->condition
       << "' has type '" << conditionTy << "' but should have type 'Boolean'.";
    report(ss.str());
  }

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

void AnalyzerVisitor::visitForStatement(ForStatement *forStatement) {
  visitLetStatement(forStatement->declaration.get());

  // Check condition results in a boolean type
  auto conditionTy = determineTypeOfExpression(forStatement->condition.get());

  if (conditionTy.type != PrimitiveType::Type::Boolean) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "for statement condition '" << *forStatement->condition
       << "' has type '" << conditionTy << "' but should have type 'Boolean'.";
    report(ss.str());
  }

  for (auto &statement : forStatement->body.get()->statements) {
    visitStatement(statement.get());
  }
}

void AnalyzerVisitor::visitWhileStatement(WhileStatement *whileStatement) {
  // Check condition results in a boolean type
  auto conditionTy = determineTypeOfExpression(whileStatement->condition.get());

  if (conditionTy.type != PrimitiveType::Type::Boolean) {
    std::stringstream ss;
    ss << "in function '" << currentFunctionDeclaration->header.name << "', ";
    ss << "while statement condition '" << *whileStatement->condition
       << "' has type '" << conditionTy << "' but should have type 'Boolean'.";
    report(ss.str());
  }

  for (auto &statement : whileStatement->body.get()->statements) {
    visitStatement(statement.get());
  }
}

void AnalyzerVisitor::visitFunctionCall(FunctionCall *functionCall) {
  determineTypeOfFunctionCall(functionCall);
}

void AnalyzerVisitor::visitProgram(Program *program) {
  currentProgram = program;
  auto hadMainFunction = false;
  for (const auto &fn : program->functions) {
    visitFunctionDeclaration(fn.get());

    if (fn.get()->header.name == "main") {
      hadMainFunction = true;

      auto intTy = PrimitiveType(PrimitiveType::Type::I32);
      if (fn.get()->header.annotatedType != intTy) {
        std::stringstream ss;
        ss << "main function has type '"
           << fn.get()->header.annotatedType.value()
           << "' but should have type " << intTy;
        report(ss.str());
      }
    }
  }

  if (!hadMainFunction) {
    report("no main function");
  }
}

void Analyzer::analyze() {
  AnalyzerVisitor av = AnalyzerVisitor(this);
  av.visitProgram(program);
}