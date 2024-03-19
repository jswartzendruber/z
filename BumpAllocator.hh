#ifndef BUMPALLOCATOR_HH
#define BUMPALLOCATOR_HH

#include <cstdlib>
#include <cstring>
#include <vector>

template <size_t slabSize = 4096> class BumpAllocator {
private:
  std::vector<void *> slabs;
  size_t leftInCurrentSlab;
  void *currPtr;

  void allocateSlab() {
    int slabIdx = slabs.size();
    slabs.push_back(malloc(slabSize));
    leftInCurrentSlab = slabSize;
    currPtr = slabs[slabIdx];
  }

  // Allocate sizeof(Type) bytes and return the pointer
  template <class T> T *allocateInternal(size_t size) {
    if (size > leftInCurrentSlab) {
      allocateSlab();
    }

    void *ptr = currPtr;
    currPtr = (char *)currPtr + size;
    leftInCurrentSlab -= size;

    // Zero new memory by default
    memset(ptr, 0, size);
    return (T *)ptr;
  }

public:
  BumpAllocator &operator=(const BumpAllocator &) = delete;
  BumpAllocator(BumpAllocator &&) = delete;
  BumpAllocator &operator=(BumpAllocator &&) = delete;

  BumpAllocator() : slabs() { allocateSlab(); }

  ~BumpAllocator() {
    for (auto slab : slabs) {
      free(slab);
    }
  }

  // Reserve space for t and save it in the allocator
  template <class T> T *allocate(T t) {
    return new (allocateInternal<T>(sizeof(T))) T{t};
  }
};

#endif
