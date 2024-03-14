#include "BumpAllocator.hh"

uintptr_t BumpAllocator::align_forward(uintptr_t ptr, size_t align) {
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

BumpAllocator::BumpAllocator(size_t size) : buf_len(size), curr_offset(0) {
  buf = (unsigned char *) malloc(size);
}

BumpAllocator::~BumpAllocator() {
  free(buf);
}

