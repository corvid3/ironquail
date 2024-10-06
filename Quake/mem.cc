#include "mem.hh"
#include "str.hh"
#include <cstdint>
#include <new>
// #include<alloca.h>

QMemAllocEngine g_qmem_alloc_engine{};
ArenaEngine<g_qmem_alloc_engine> cl_arena{};
ArenaEngine<g_qmem_alloc_engine> sv_arena{};

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
  used_mem_b += bytes;
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
allocation_size(void const* ptr)
{
  Header const* hdr = reinterpret_cast<Header const*>(ptr);
  return hdr->size;
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

struct SmallAllocEngine::Header
{
  // uses the top bit of the size integer to determine
  // if this header is free...
  // we're not gonna be allocating anything greater than even like a megabyte
  // on the stack
  // -crow

  // size of the allocation, not including header
  std::uint32_t m_size;
  char data[];

  bool is_free() { return m_size & (1 << 31); }
  void set_free() { m_size |= 1 << 31; }
  void set_not_free() { m_size &= ~(1 << 31); }
};

SmallAllocEngine::SmallAllocEngine(unsigned const max_kb)
  : m_max_kb(max_kb)
  , m_max_b(kibi(m_max_kb))
{
  m_data_ptr = QMem::malloc(m_max_b);
  __builtin_memset(m_data_ptr, 0, m_max_b);

  Header* header = reinterpret_cast<Header*>(m_data_ptr);
  header->m_size = m_max_b - sizeof(Header);
  header->set_free();
}

SmallAllocEngine::~SmallAllocEngine()
{
  QMem::free(m_data_ptr);
}

char*
SmallAllocEngine::get_data_ptr()
{
  return reinterpret_cast<char*>(m_data_ptr);
}

SmallAllocEngine::Header*
SmallAllocEngine::get_first_free_header(std::uint32_t const minimum_size)
{
  char* data = get_data_ptr();

  std::uint32_t offset = 0;

  while (offset < m_max_b) {
    Header* header = reinterpret_cast<Header*>(data + offset);
    if (header->is_free() && header->m_size >= minimum_size)
      return header;
    offset += header->m_size + sizeof(Header);
  }

  return nullptr;
}

SmallAllocEngine::Header*
SmallAllocEngine::merge_free(std::uint32_t const until)
{
  char* data = get_data_ptr();
  std::uint32_t offset = 0;

  Header* old_header = nullptr;

  while (offset < m_max_b) {
    Header* this_header = reinterpret_cast<Header*>(data + offset);

    offset += this_header->m_size + sizeof(Header);
    if (!old_header)
      continue;

    if (old_header->is_free() && this_header->is_free()) {
      old_header->m_size += this_header->m_size + sizeof(Header);

      if (old_header->m_size >= until)
        return old_header;
    }

    old_header = this_header;
  }

  return nullptr;
}

void*
SmallAllocEngine::internal_allocate(std::size_t const size)
{
  // get a free header, and if the header size is more than
  // or equal to 64 bytes longer than we need,
  // split the allocation header
  // by retaining some extra space after the allocation,
  // theres some space left over for any extra
  // short reallocations

  Header* header = get_first_free_header(size);

  // if we couldn't find a free header,
  // run through the entire allocation list &
  // merge free allocations
  if (header == nullptr) {
    header = merge_free(size);

    if (header == nullptr)
      throw std::bad_alloc();
  }

  if (header->m_size >= size + 64) {
    char* data = reinterpret_cast<char*>(header);
    data += sizeof(Header);
    data += size + 64;

    Header* split_header = reinterpret_cast<Header*>(data);
    split_header->m_size = header->m_size - size + 64 - sizeof(Header);
    header->m_size = size + 64;
  }

  header->set_not_free();

  return header->data;
}

void*
SmallAllocEngine::allocate(std::size_t const bytes)
{
  if (m_last_dealloc != nullptr and m_last_dealloc->is_free() and
      m_last_dealloc->m_size >= bytes) {
    m_last_dealloc->set_not_free();
    return m_last_dealloc->data;
  }

  return internal_allocate(bytes);
}

void*
SmallAllocEngine::realloc(void* old_ptr, std::size_t const bytes)
{
  Header* header = reinterpret_cast<Header*>(old_ptr) - 1;
  if (header->m_size >= bytes)
    return old_ptr;

  void* new_ptr = allocate(bytes);
  __builtin_memcpy(new_ptr, old_ptr, header->m_size);
  deallocate(old_ptr);
  return new_ptr;
}

void
SmallAllocEngine::deallocate(void* ptr)
{
  Header* header = reinterpret_cast<Header*>(ptr) - 1;
  header->set_free();
  m_last_dealloc = header;
}
