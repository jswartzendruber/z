#ifndef BUMPALLOCATOR_HH
#define BUMPALLOCATOR_HH

#include <cstring>
#include <cstdint>
#include <cstdlib>

class BumpAllocator {
private:
  unsigned char *buf;
  size_t buf_len;
  size_t curr_offset;

  uintptr_t align_forward(uintptr_t ptr, size_t align);

  // Allocate sizeof(Type) bytes and return the pointer
  template <class Type>
  Type *allocateInternal(size_t size) {
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

public:
  BumpAllocator() = delete;
  BumpAllocator(size_t size);
  BumpAllocator& operator=(const BumpAllocator&) = delete;
  BumpAllocator(BumpAllocator&&) = delete;
  BumpAllocator& operator=(BumpAllocator&&) = delete;
  ~BumpAllocator();

  // Reserve space for t and save it in the allocator
  template <class T>
  T *allocate(T t) {
    size_t size = sizeof(T);
    return new (allocateInternal<T>(size)) T{t};
  }

  template <class T>
  T *allocateBytes(int bytes) {
    return allocateInternal<T>(bytes);
  }
};

#endif
