#pragma once

// alternative cache implementation
#include "str.hh"
#include <cstddef>

template<typename T>
class CacheAllocator
{
  void* internal_alloc(std::size_t const bytesize);
  void* internal_dealloc(std::size_t const bytesize);

public:
  using value_type = T;
  T* allocate(std::size_t const n);
  void deallocate(T* ptr);
};
