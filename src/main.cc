#include "lexer.hh"

int main () {
  auto code = "fn 123\n\n main \"hi\" 3.14 (){} true false;";
  auto lexer = Lexer(code);

  try {
    std::optional<Token> token;
    while ((token = lexer.nextToken())) {
      std::cout << *token << "\n";
    }
  } catch (UnclosedDelimiter d) {
    std::cout << "Error: Unclosed delimeter beginning at line " << lexer.currentLine << "\n";
  }

  return 0;
}
