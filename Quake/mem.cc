#include "mem.hh"

namespace QMem {

struct Header
{
  unsigned size;
};

unsigned static max_mem_b = 1024 * 1024 * 512;
unsigned static used_mem_b = 0;

void*
malloc(std::size_t const bytes)
{
  Header* data = static_cast<Header*>(::operator new(bytes + sizeof(Header)));
  data->size = bytes;
  return data + 1;
}

void
free(void* ptr)
{
  Header* data = static_cast<Header*>(ptr) - 1;
  used_mem_b -= data->size;
  ::operator delete(data);
}

unsigned
get_max()
{
  return max_mem_b;
}

unsigned
get_used()
{
  return used_mem_b;
}

};
