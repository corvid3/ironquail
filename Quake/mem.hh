#pragma once

#include "SDL_assert.h"
#include "common.hh"
#include "sys.hh"
#include <array>

namespace QMem {
void*
malloc(std::size_t const bytes);
void
free(void* ptr);

unsigned
get_max();
unsigned
get_used();
unsigned inline get_free()
{
  return get_max() - get_used();
};
};

// stack allocation engine
// intended for small allocations, like strings
// or small, short lived std::vectors
// use carefully
class StackEngine
{
  struct Header;

  unsigned constexpr static max_kb = 64;
  unsigned constexpr static max_b = kibi(max_kb);

  unsigned m_used = 0;
  Header* m_last_dealloc = nullptr;
  std::array<unsigned char, max_b> m_stack;

  char* get_data_ptr();

  // run through the entire list,
  // and collect + merge all adjacent free allocations
  // returns a header that is of at least `until' size
  Header* merge_free(uint32_t const until);

  // minimum size should be the data allocation, not including
  // the allocation size of the header
  Header* get_first_free_header(uint32_t const minimum_size);
  void* internal_allocate(std::size_t const size);

public:
  StackEngine();

  void* allocate(std::size_t const bytes);
  void* realloc(void* ptr, std::size_t const bytes);
  void deallocate(void* ptr);
};

template<typename T>
class QStackAlloc
{
  StackEngine& m_engine;

public:
  using value_type = T;

  QStackAlloc(StackEngine& engine)
    : m_engine(engine)
  {
  }

  T* allocate(std::size_t const n)
  {
    return reinterpret_cast<T*>(m_engine.allocate(sizeof(T) * n));
  }

  void deallocate(void* ptr, std::size_t const) { m_engine.deallocate(ptr); }
};

template<typename T>
struct QAlloc
{
  using value_type = T;

  T* allocate(std::size_t const n)
  {
    return reinterpret_cast<T*>(QMem::malloc(sizeof(T) * n));
  }
  void deallocate(T* ptr, std::size_t) { QMem::free(ptr); }

  bool operator==(QAlloc const&) const { return true; }
};
