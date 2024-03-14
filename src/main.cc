#include "lexer.hh"
#include <fstream>
#include <sstream>
#include <cassert>
#include <cstring>

class BumpAllocator {
private:
  unsigned char *buf;
  size_t buf_len;
  size_t curr_offset;

  uintptr_t align_forward(uintptr_t ptr, size_t align) {
    uintptr_t p = ptr;
    uintptr_t a = (uintptr_t) align;
    // Same as (p % a) but faster as 'a' is a power of two
    uintptr_t modulo = p & (a - 1);

    if (modulo != 0) {
      // If 'p' address is not aligned, push the address to the
      // next value which is aligned
      p += a - modulo;
    }

    return p;
  }

  bool is_power_of_two(uintptr_t x) {
    return (x & (x - 1)) == 0;
  }

public:
  BumpAllocator(size_t size) : buf_len(size), curr_offset(0) {
    buf = (unsigned char *) malloc(size);
  }

  ~BumpAllocator() {
    free(buf);
  }

  template <class Type>
  Type *allocate() {
    size_t size = sizeof(Type);
    size_t align = 8;

    // Align 'curr_offset' forward to the specified alignment
    uintptr_t curr_ptr = (uintptr_t) buf + (uintptr_t) curr_offset;
    uintptr_t offset = align_forward(curr_ptr, align);
    offset -= (uintptr_t) buf; // Change to relative offset

    // Check to see if the backing memory has space left
    if (offset + size <= buf_len) {
      void *ptr = &buf[offset];
      curr_offset = offset + size;

      // Zero new memory by default
      memset(ptr, 0, size);
      return (Type *) ptr;
    }

    // Return NULL if the arena is out of memory (or handle differently)
    return NULL;
  }
};

template <class T>
T *allocateNode(BumpAllocator& allocator, T t) {
  return new (allocator.allocate<T>()) T{t};
}

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

struct Thingy {
  size_t a;
  float b;
  std::string c;

  Thingy(size_t a, float b, std::string c) : a(a), b(b), c(c) {}

  friend std::ostream& operator<<(std::ostream& os, const Thingy& t) {
    os << "Thingy { a: " << t.a << ", b: " << t.b << ", c: \"" << t.c << "\" }";
    return os;
  }
};

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
  int *a = allocateNode(ba, 123);
  Thingy *th = allocateNode(ba, Thingy{ 478, 12.3, "hello" });

  std::cout << *a << "\n";
  std::cout << *th << "\n";

  return 0;
}
