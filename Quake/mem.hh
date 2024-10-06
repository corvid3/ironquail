#pragma once

#include "SDL_assert.h"
#include "mathlib.hh"
#include "sys.hh"
#include <array>
#include <cstdint>
#include <memory>
#include <optional>
#include <string_view>

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
free(void const* ptr);

// returns the allocation size of a given pointer
unsigned
allocation_size(void* ptr);

struct Deleter
{
  void operator()(void* ptr) { QMem::free(ptr); }
};

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
// intended for lots of small allocations, short lived
// allocations like strings or small vectors use carefully
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

class QMemAllocEngine
{
public:
  inline void* allocate(std::size_t const bytes) { return QMem::malloc(bytes); }
  inline void deallocate(void* ptr) { QMem::free(ptr); }
};

template<typename Engine>
class AllocEngineTraits
{
  Engine& m_engine;

public:
  AllocEngineTraits(Engine& engine = Engine{})
    : m_engine(engine)
  {
  }

  inline void* allocate(std::size_t const bytes,
                        std::optional<std::string_view> name = std::nullopt)
  {
    return m_engine.allocate(bytes, name);
  }

  inline void deallocate(void* ptr) { m_engine.deallocate(ptr); }
};

// wrapper that converts any
// allocation engine to an arena
// TODO: move all non-template code to mem.cc
template<auto engine>
class ArenaEngine
{
  decltype(engine)& m_engine;

  void* m_ptr;

  // current top of the allocation
  // points to uninitialized data
  std::uint32_t m_offset;
  std::uint32_t m_amt_allocated;

  float constexpr static alloc_grow_factor = 1.75f;

  struct Header
  {
    char const* name;
    short unsigned name_size;
  };

public:
  ArenaEngine(unsigned const default_alloc_size = kibi(4))
    : m_engine(engine)
  {
    m_ptr = m_engine.allocate(default_alloc_size);
    m_amt_allocated = default_alloc_size;
    m_offset = 0;
  }

  ~ArenaEngine() { m_engine.deallocate(m_ptr); }

  void* allocate(std::size_t bytes,
                 std::optional<std::string_view> name = std::nullopt)
  {
    bytes += sizeof(Header);

    if (m_offset + bytes >= m_amt_allocated) {
      void* old_ptr = m_ptr;
      std::uint32_t old_size = m_amt_allocated;

      m_amt_allocated *= alloc_grow_factor;
      m_ptr = m_engine.allocate(m_amt_allocated);

      __builtin_memcpy(m_ptr, old_ptr, old_size);

      m_engine.deallocate(m_ptr);
    }

    Header* out = reinterpret_cast<Header*>(m_ptr) + m_offset;
    if (name) {
      out->name = QMem::malloc(name->size());
      out->name_size = name->size();
      __builtin_memcpy(out->name, name->data(), name->size());
    } else {
      out->name_size = 0;
    }

    m_offset += bytes;
    return out + 1;
  }

  // dumps all information to the console
  void dump() {}

  // do jack shit . it don't matter!
  void deallocate(void*) {}

  // deallocates EVERYTHING.
  void reset() { m_offset = 0; }
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

  T* allocate(std::size_t const n) { return _allocate(n); }
  void deallocate(T* ptr, std::size_t) { return _deallocate(ptr, 0); }

  bool operator==(QGeneralAlloc const&) const { return true; }

  static T* _allocate(std::size_t const n)
  {
    return reinterpret_cast<T*>(QMem::malloc(sizeof(T) * n));
  }
  static void _deallocate(T* ptr, std::size_t) { QMem::free(ptr); }

  class Deleter
  {
  public:
    void operator()(T* ptr)
    {
      if (ptr)
        ptr->~T();
      QGeneralAlloc::_deallocate(ptr, 0);
    }
  };
};

template<typename T>
class Pool
{
  struct Header
  {
    bool free;
  };

  Header* m_list;
  unsigned m_list_size;
  unsigned m_taken;

  static constexpr unsigned init_list_size = 64;
  static constexpr float grow_factor = 1.75f;

  T* try_find()
  {
    for (unsigned i = 0; i < m_list_size; i++) {
      auto& h = m_list[i];
      if (h.free) {
        h.free = false;
        m_taken++;
        return static_cast<T*>(h + 1);
      }
    }

    return nullptr;
  }

public:
  Pool(const Pool&) = delete;
  Pool(Pool&&) = delete;
  Pool& operator=(const Pool&) = delete;
  Pool& operator=(Pool&&) = delete;

  Pool()
    : m_list(QMemAllocEngine().allocate((sizeof(T) + sizeof(Header)) *
                                        init_list_size))
    , m_list_size(init_list_size)
    , m_taken(0)
  {
    for (unsigned i = 0; i < init_list_size; i++) {
      Header& h = m_list[i];
      h.free = true;
    }
  }

  ~Pool() { delete m_list; }

  T* allocate()
  {
    auto find = try_find();
    if (find != nullptr)
      return find;

    auto const new_size = m_list_size * grow_factor;
    Header* new_list =
      QMemAllocEngine().allocate((sizeof(T) + sizeof(Header)) * new_size);
    __builtin_memcpy(new_list, m_list, m_list_size);
    m_list_size = new_size;
    QMemAllocEngine().deallocate(m_list);
    m_list = new_list;

    find = try_find();
    if (find == nullptr)
      Sys_Error("unable to allocate space for memory pool\n");

    return find;
  }

  T* deallocate(T* ptr)
  {
    Header* h = static_cast<Header*>(ptr) - 1;
    h->free = true;
    m_taken--;
  }

  // deallocates all
  T* reset()
  {
    Header* header = m_list;
    for (unsigned i = 0; i < m_list_size; i++, header++) {
      header->free = true;
    }
    m_taken = 0;
  }
};

template<typename T, template<typename M> typename A = QGeneralAlloc>
using q_uptr = std::unique_ptr<T, typename A<T>::Deleter>;
template<typename T>
using q_wptr = std::weak_ptr<T>;

extern QMemAllocEngine g_qmem_alloc_engine;

// used for all allocations related directly to
// the game client, like models, sounds, etc
// do not use as scratchpad space!
// this is not a direct analogue to the old
// extern ArenaEngine<g_qmem_alloc_engine> cl_arena;
// extern ArenaEngine<g_qmem_alloc_engine> sv_arena;
