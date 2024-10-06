#pragma once

#include <algorithm>
#include <iterator>
#include <list>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include "mem.hh"

template<template<typename M> typename A = QGeneralAlloc>
using q_str = std::basic_string<char, std::char_traits<char>, A<char>>;
template<template<typename M> typename A = QGeneralAlloc>
using q_strstr = std::basic_stringstream<char, std::char_traits<char>, A<char>>;
template<typename T, template<typename M> typename A = QGeneralAlloc>
using q_vec = std::vector<T, A<T>>;
template<typename T, template<typename M> typename A = QGeneralAlloc>
using q_list = std::list<T, A<T>>;

extern template class std::
  basic_string<char, std::char_traits<char>, QGeneralAlloc<char>>;
extern template class std::
  basic_stringstream<char, std::char_traits<char>, QGeneralAlloc<char>>;

template<typename T,
         unsigned const SIZE,
         template<typename M> typename A = QGeneralAlloc>
class q_array
{
public:
  using value_type = T;
  using size_type = std::size_t;
  using iterator = T*;
  using const_iterator = T const*;

private:
  using AL = A<value_type>;
  AllocEngineTraits<AL> m_al;
  value_type* m_data;

public:
  q_array(AL a = AL{})
    : m_al(a)
  {
    m_data = m_al.allocate(SIZE);
  }
  ~q_array() { m_al.deallocate(m_al); };

  q_array(q_array const& rhs)
    : m_al(rhs.m_al)
  {
    if (rhs.m_data == nullptr)
      return;

    m_data = m_al.allocate(SIZE);
    __builtin_memcpy(m_data, rhs.m_data, sizeof(value_type) * SIZE);
  }

  q_array(q_array&& rhs)
    : m_al(rhs.m_al)
    , m_data(rhs.m_data)
  {
    rhs.m_data = nullptr;
  }

  q_array& operator=(q_array const& rhs)
  {
    m_al = rhs.m_al;

    if (rhs.m_data != nullptr) {
      this->m_data = nullptr;
    } else {
      m_data = m_al.allocate(SIZE);
      __builtin_memcpy(m_data, rhs.m_data, sizeof(value_type) * SIZE);
    }

    return *this;
  }

  q_array& operator=(q_array&& rhs)
  {
    m_al = rhs.m_al;
    m_data = rhs.m_data;
    rhs.m_data = nullptr;
  }

  value_type& operator[](size_type const ind) { return m_data[ind]; }
};

template<typename T, template<typename M> typename A = QGeneralAlloc>
class q_unique_vec
{
public:
  using value_type = T;
  using size_type = std::size_t;
  using iterator = T*;
  using const_iterator = T const*;

private:
  using AL = A<value_type>;

  value_type* m_data;
  size_type m_size;
  AllocEngineTraits<A<value_type>> m_alloc;

public:
  q_unique_vec(AL allocator = AL{})
    : m_data(nullptr)
    , m_size(0)
    , m_alloc(allocator)
  {
  }

  // copies a range of values into itself
  template<typename from>
  explicit q_unique_vec(from start, from end, AL alloc = AL{})
    : q_unique_vec(end - start / sizeof(T), alloc)
  {
    std::copy(start, end, m_data);
  }

  explicit q_unique_vec(size_type const size, AL alloc = AL{})
    : q_unique_vec(alloc)
  {
    m_data = m_alloc.allocate(size);
  }

  ~q_unique_vec()
  {
    if (m_data != nullptr)
      m_alloc.deallocate(m_data);
  }

  q_unique_vec(q_unique_vec const&) = delete;
  q_unique_vec operator=(q_unique_vec const&) = delete;
  q_unique_vec(q_unique_vec&& rhs)
    : m_data(rhs.m_data)
    , m_size(rhs.m_size)
  {
    rhs.m_data = nullptr;
    rhs.m_size = 0;
  }

  q_unique_vec operator=(q_unique_vec&& rhs)
  {
    m_data = rhs.m_data;
    m_size = rhs.m_size;
    rhs.m_data = nullptr;
    rhs.m_size = 0;
  }

  constexpr size_type size() noexcept { return m_size; }
  constexpr operator bool() noexcept { return m_data != nullptr; }
  value_type& operator[](size_type const idx)
  {
    if (idx >= m_size)
      throw std::out_of_range(
        "attempted to index past the bounds of a unique_vec");

    return m_data[idx];
  }

  void free() { m_alloc.deallocate(m_data); }
  T* data() noexcept { return m_data; }
  T const* ptr() const noexcept { return m_data; }
  bool empty() const noexcept { return m_data != nullptr; }

  iterator begin() { return m_data; }
  iterator end() { return m_data + m_size; }
  const_iterator cbegin() const { return m_data; }
  const_iterator cend() const { return m_data + m_size; }
};

int
caseins_streq(std::string_view const lhs, std::string_view const rhs);
