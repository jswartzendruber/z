#include "CBackend.hh"
#include "AST.hh"
#include "Lexer.hh"

std::string primitiveTypeToString(PrimitiveType type) {
  switch (type.type) {
  case PrimitiveType::Boolean:
    return "bool";
  case PrimitiveType::String:
    return "char*";
  case PrimitiveType::Void:
    return "void";
  case PrimitiveType::I64:
    return "int64_t";
  case PrimitiveType::F64:
    return "double";
  case PrimitiveType::I32:
    return "int32_t";
  case PrimitiveType::F32:
    return "float";
  }

  UNREACHABLE();
}

void EmitterVisitor::printDepth() {
  for (int i = 0; i < depth; i++) {
    *genSrc << " ";
  }
}

void EmitterVisitor::visitIntegerValue(IntegerValue *integerValue) {
  *genSrc << integerValue->val;
}

void EmitterVisitor::visitFloatValue(FloatValue *floatValue) {
  *genSrc << floatValue->val;
}

void EmitterVisitor::visitStringValue(StringValue *stringValue) {
  *genSrc << stringValue->val;
}

void EmitterVisitor::visitBinaryExpression(BinaryExpression *binaryExpression) {
  *genSrc << "(";
  visitExpression(binaryExpression->lhs.get());
  *genSrc << " ";
  *genSrc << operationToString(binaryExpression->op);
  *genSrc << " ";
  visitExpression(binaryExpression->rhs.get());
  *genSrc << ")";
}

void EmitterVisitor::visitVariable(Variable *variable) {
  *genSrc << variable->name;
}

void EmitterVisitor::visitBooleanValue(BooleanValue *booleanValue) {
  if (booleanValue->val) {
    *genSrc << "true";
  } else {
    *genSrc << "false";
  }
}

void EmitterVisitor::visitPostfixExpression(
    PostfixExpression *postfixExpression) {
  *genSrc << "(";
  visitExpression(postfixExpression->expr.get());
  *genSrc << postfixOperationToString(postfixExpression->op);
  *genSrc << ")";
}

void EmitterVisitor::visitFunctionCall(FunctionCall *functionCall) {
  *genSrc << functionCall->name << "(";
  for (auto &arg : functionCall->arguments) {
    visitExpression(arg.get());
  }
  *genSrc << ")";
}

void EmitterVisitor::visitUnaryExpression(UnaryExpression *unaryExpression) {
  *genSrc << "(";
  *genSrc << unaryOperationToString(unaryExpression->op);
  visitExpression(unaryExpression->expr.get());
  *genSrc << ")";
}

void EmitterVisitor::statementBlock(StatementBlock *stmts) {
  *genSrc << "{\n";
  depth += 2;
  for (auto &stmt : stmts->statements) {
    printDepth();
    visitStatement(stmt.get());
    if (stmt->type != Statement::Type::IfStatement) {
      *genSrc << ";\n";
    }
  }
  depth -= 2;
  printDepth();
  *genSrc << "}";
}

void EmitterVisitor::visitReturnStatement(ReturnStatement *returnStatement) {
  *genSrc << "return";
  if (returnStatement->val.has_value()) {
    *genSrc << " ";
    visitExpression(returnStatement->val.value().get());
  }
}

void EmitterVisitor::visitIfStatement(IfStatement *ifStatement) {
  *genSrc << "if (";

  visitExpression(ifStatement->condition.get());

  *genSrc << ") ";

  statementBlock(ifStatement->ifTrueStmts.get());

  if (ifStatement->ifFalseStmts.has_value()) {
    *genSrc << " else ";
    statementBlock(ifStatement->ifFalseStmts.value().get());
  }

  *genSrc << "\n";
}

void EmitterVisitor::visitLetStatement(LetStatement *letStatement) {
  *genSrc << primitiveTypeToString(letStatement->annotatedType.value()) << " "
          << letStatement->name << " = ";
  visitExpression(letStatement->initializer.get());
}

void EmitterVisitor::visitFunctionParameter(Parameter *parameter) {
  *genSrc << primitiveTypeToString(parameter->annotatedType.value()) << " "
          << parameter->name;
}

void EmitterVisitor::visitFunctionDeclaration(
    FunctionDeclaration *functionDeclaration) {
  *genSrc << primitiveTypeToString(
                 functionDeclaration->header.annotatedType.value())
          << " " << functionDeclaration->header.name << "(";

  for (auto &param : functionDeclaration->header.parameters) {
    visitFunctionParameter(param.get());
  }

  *genSrc << ") ";
  statementBlock(functionDeclaration->body.get());
  *genSrc << "\n";
}

void EmitterVisitor::visitProgram(Program *program) {
  for (const auto &fn : program->functions) {
    visitFunctionDeclaration(fn.get());
  }
  *genSrc << "\n";
}

std::string CBackend::emit() {
  genSrc << std::fixed; // Always print floats like 0.0 instead of 0
  genSrc << "#include <stdint.h>\n"; // Include int64_t type
  genSrc << "#include <stdio.h>\n"; // printf

  auto visitor = EmitterVisitor(&genSrc);
  visitor.visitProgram(ast);
  return genSrc.str();
}