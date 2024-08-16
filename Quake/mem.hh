#pragma once

#include <array>
#include <memory>
#include <string>
#include <string_view>

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
