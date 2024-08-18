#pragma once

#include "SDL_assert.h"
#include "mathlib.hh"
#include "sys.hh"
#include <array>
#include <cstdint>

/*

ideas to ponder:

  - storing the destructor function associated with
    a pointer, and then check if the pointer is free'd by the
    destruction of an engine
    if the pointer hasn't been freed, then call
    the destructor to ensure proper cleanup of resources
    in case a longjmp is called (see _Host_Frame)

*/

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

// allocation engine
// intended for lots of small allocations, short lived allocations
// like strings or small vectors
// use carefully
class SmallAllocEngine
{
  struct Header;

  unsigned const m_max_kb;
  unsigned const m_max_b;

  unsigned m_used = 0;
  Header* m_last_dealloc = nullptr;
  void* m_data_ptr = nullptr;

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
  SmallAllocEngine(unsigned const max_kb = 32);
  ~SmallAllocEngine();

  void* allocate(std::size_t const bytes);
  void* realloc(void* ptr, std::size_t const bytes);
  void deallocate(void* ptr);
};

template<typename Engine>
class AllocEngineTraits
{
  Engine& m_engine;

public:
  AllocEngineTraits(Engine& engine)
    : m_engine(engine)
  {
  }

  inline void* allocate(std::size_t const bytes)
  {
    return m_engine.allocate(bytes);
  }

  inline void deallocate(void* ptr) { m_engine.deallocate(ptr); }
};

// wrapper that converts any
// allocation engine to an arena
template<typename Engine>
class ArenaEngine
{
  AllocEngineTraits<Engine>& m_engine;

  void* m_ptr;

  // current top of the allocation
  // points to uninitialized data
  std::uint32_t m_offset;
  std::uint32_t m_amt_allocated;

  std::uint32_t constexpr static default_alloc_size = kibi(32);
  float constexpr static alloc_grow_factor = 1.75f;

public:
  ArenaEngine(Engine& engine)
    : m_engine(engine)
  {
    m_ptr = m_engine.allocate(default_alloc_size);
    m_amt_allocated = default_alloc_size;
    m_offset = 0;
  }

  ~ArenaEngine() { m_engine.deallocate(m_ptr); }

  void* allocate(std::size_t const bytes)
  {
    if (m_offset + bytes >= m_amt_allocated) {
      void* old_ptr = m_ptr;
      std::uint32_t old_size = m_amt_allocated;

      m_amt_allocated *= alloc_grow_factor;
      m_ptr = m_engine.allocate(m_amt_allocated);

      __builtin_memcpy(m_ptr, old_ptr, old_size);

      m_engine.deallocate(m_ptr);
    }

    void* out = reinterpret_cast<char*>(m_ptr) + m_offset;
    m_offset += bytes;
    return out;
  }

  // do jack shit . it don't matter!
  void deallocate(void*) {}
};

template<typename T>
class QSmallAlloc
{
  SmallAllocEngine& m_engine;

public:
  using value_type = T;

  QSmallAlloc(SmallAllocEngine& engine)
    : m_engine(engine)
  {
  }

  template<typename O>
  QSmallAlloc(QSmallAlloc<O> const& rhs)
    : m_engine(rhs.m_engine)
  {
  }

  T* allocate(std::size_t const n)
  {
    return reinterpret_cast<T*>(m_engine.allocate(sizeof(T) * n));
  }

  void deallocate(void* ptr, std::size_t const) { m_engine.deallocate(ptr); }
};

template<typename T>
struct QGeneralAlloc
{
  using value_type = T;

  T* allocate(std::size_t const n)
  {
    return reinterpret_cast<T*>(QMem::malloc(sizeof(T) * n));
  }
  void deallocate(T* ptr, std::size_t) { QMem::free(ptr); }

  bool operator==(QGeneralAlloc const&) const { return true; }
};

// global small allocator engine
// data is initialized in main (main_sdl.cc)
// extern ptr is stored in mem.cc
extern SmallAllocEngine* g_small_alloc;
