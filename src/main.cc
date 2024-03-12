#include "lexer.hh"

int main () {
  auto code = "(){}";
  auto lexer = Lexer(code);

  {
    std::optional<Token> token;
    while ((token = lexer.nextToken())) {
      std::cout << *token << "\n";
    }
  }

  return 0;
}
