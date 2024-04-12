#include "Lib/Analyzer.hh"
#include "Lib/CBackend.hh"
#include "Lib/Parser.hh"
#include <fstream>
#include <sstream>

std::optional<std::string> readFile(const std::string &filename,
                                    ErrorReporter *errorReporter) {
  std::ifstream file(filename, std::ios::binary);
  if (!file.is_open()) {
    std::stringstream ss;
    ss << "could not open file '" << filename << "'";
    errorReporter->report(ss.str());
    return std::nullopt;
  }

  std::stringstream buffer;
  buffer << file.rdbuf();
  std::string res = buffer.str();

  return res;
}

int main(int argc, char **argv) {
  auto errorReporter = ErrorReporter();

  if (argc != 2) {
    errorReporter.report("expected file path as argument");
    return 1;
  }

  auto fileName = argv[1];
  auto file = readFile(fileName, &errorReporter);
  errorReporter.setFileName(fileName);

  if (file.has_value()) {
    std::string code = file.value();
    auto stringTable = StringTable{};
    auto lexer = Lexer(code, &stringTable, &errorReporter);

    auto parser = Parser(&lexer, &errorReporter);
    if (parser.anyErrors()) {
      return 1;
    }

    auto ast = parser.parse().value();

    auto analyzer = Analyzer(&errorReporter, &ast);
    analyzer.analyze();

    if (analyzer.anyErrors()) {
      return 1;
    }

    auto cbackend = CBackend(&ast);
    auto cSrc = cbackend.emit();

    std::cout << "AST:\n";
    std::cout << ast;
    std::cout << "\nCodegen:\n";
    std::cout << cSrc;
  }

  return 0;
}
