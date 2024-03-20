#include "ErrorReporter.hh"
#include "Parser.hh"
#include <fstream>
#include <sstream>

std::optional<std::string> readFile(const std::string &filename) {
  std::ifstream file(filename, std::ios::binary);
  if (!file.is_open()) {
    std::cerr << "Error: Could not open file " << filename << "\n";
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
  errorReporter.setFileName(fileName);

  auto file = readFile(fileName);
  if (file.has_value()) {
    std::string code = file.value();
    auto stringTable = StringTable{};
    auto lexer = Lexer(code, &stringTable, &errorReporter);

    try {
      auto allocator = BumpAllocator();
      auto parser = Parser(&allocator, lexer, &errorReporter);
      auto fn = parser.parseFunctionDeclaration();
      if (fn.has_value()) {
        std::cout << *fn.value() << "\n";
      }
    } catch (UnclosedDelimiter ud) {
      errorReporter.report("unclosed delimiter", ud.line);
      return 1;
    }
  }

  return 0;
}
