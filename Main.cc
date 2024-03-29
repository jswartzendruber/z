#include "ErrorReporter.hh"
#include "Parser.hh"
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

    try {
      auto parser = Parser(&lexer, &errorReporter);
      auto ast = parser.parse();

      if (parser.anyErrors()) {
        return 1;
      }

      // if (ast.has_value()) {
      //   std::cout << ast.value();
      // }
    } catch (UnclosedDelimiter ud) {
      errorReporter.report("unclosed delimiter", ud.line);
      return 1;
    }
  }

  return 0;
}
