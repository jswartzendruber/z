#include "../Lib/Parser.hh"
#include <stddef.h>
#include <stdint.h>

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  std::string code = std::string(reinterpret_cast<const char *>(data), size);
  auto stringTable = StringTable{};
  auto errorReporter = ErrorReporter(true);
  auto lexer = Lexer(code, &stringTable, &errorReporter);
  auto parser = Parser(&lexer, &errorReporter);
  auto ast = parser.parse();

  return 0;
}