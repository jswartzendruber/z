#include "Analyzer.hh"
#include "AST.hh"
#include <sstream>

void Analyzer::report(std::string msg) {
  errorReporter->report(msg);
  hadErrors = true;
}

bool Analyzer::anyErrors() { return hadErrors; }

class AnalyzerVisitor : public ASTVisitor {
  Analyzer *analyzer;
  FunctionDeclaration *currentFunctionDeclaration;

  void report(std::string msg) { analyzer->report(msg); }

  void visitFunctionDeclaration(FunctionDeclaration *functionDeclaration) {
    currentFunctionDeclaration = functionDeclaration;
    for (auto &param : functionDeclaration->parameters) {
      visitFunctionParameter(param.get());
    }
  }

  void visitFunctionParameter(Parameter *parameter) {
    auto param_ty = stringToPrimitiveType(parameter->type);

    if (param_ty.has_value()) {
      parameter->annotatedType = param_ty.value();
    } else {
      std::stringstream ss;
      ss << "in function '" << currentFunctionDeclaration->name << "', ";
      ss << "parameter '" << parameter->name << "' has type '"
         << parameter->type << "' which does not exist.";
      report(ss.str());
    }
  }

public:
  AnalyzerVisitor(Analyzer *analyzer)
      : analyzer(analyzer), currentFunctionDeclaration(nullptr) {}
};

void Analyzer::annotateAST() {
  AnalyzerVisitor av = AnalyzerVisitor(this);
  av.visitProgram(program);
}