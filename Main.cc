#include "Lib/Analyzer.hh"
#include "Lib/CBackend.hh"
#include "Lib/Parser.hh"
#include <cstdlib>
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

bool saveFile(const std::string &filename, const std::string &fileContents,
              ErrorReporter *errorReporter) {
  std::ofstream file(filename, std::ios::out);

  if (!file) {
    std::stringstream ss;
    ss << "could not open file '" << filename << "'";
    errorReporter->report(ss.str());
    return false;
  }

  file << fileContents;

  if (file.fail()) {
    std::stringstream ss;
    ss << "could not write to file '" << filename << "'";
    return false;
  }

  return true;
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

    std::string cSrcFileName = argv[1];
    cSrcFileName += ".c";
    std::string cExecutableFileName = argv[1];
    cExecutableFileName += ".out";

    saveFile(cSrcFileName, cSrc, &errorReporter);
    std::stringstream compileCommand;
    compileCommand << "gcc -o " << cExecutableFileName << " " << cSrcFileName;
    system(compileCommand.str().c_str());
  }

  return 0;
}
