#include "BumpAllocator.hh"
#include "Lexer.hh"
#include "Parser.hh"
#include <fstream>
#include <sstream>

std::optional<std::string> readFile(const std::string& filename) {
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

int main (int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "Error: expected file path as argument\n";
    return 1;
  }

  auto file = readFile(argv[1]);
  if (file.has_value()) {
    std::string code = file.value();
    auto stringTable = StringTable{};
    auto lexer = Lexer(code, &stringTable);

    try {
      std::optional<Token> token;
      while ((token = lexer.nextToken())) {
	std::cout << *token << "\n";
      }
    } catch (UnclosedDelimiter _) {
      std::cerr << "Error: Unclosed delimeter beginning at line " << lexer.currentLine << "\n";
      return 1;
    }
  }
  
  auto ba = BumpAllocator(1024 * 1024);
  auto x = ba.allocate(IntegerValue(24));
  auto y = ba.allocate(IntegerValue(1324));
  auto z = ba.allocate(BinaryExpression(Operation::Add, x, y));

  std::cout << *z << "\n";

  return 0;
}
