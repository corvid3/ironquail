#pragma once

#include <sstream>
#include <string>
#include <vector>

#include "mem.hh"

template<template<typename M> typename A = QGeneralAlloc>
using q_str = std::basic_string<char, std::char_traits<char>, A<char>>;
template<template<typename M> typename A = QGeneralAlloc>
using q_strstr = std::basic_stringstream<char, std::char_traits<char>, A<char>>;
template<typename T, template<typename M> typename A = QGeneralAlloc>
using q_vec = std::vector<T, A<T>>;

extern template class std::
  basic_string<char, std::char_traits<char>, QGeneralAlloc<char>>;
extern template class std::
  basic_stringstream<char, std::char_traits<char>, QGeneralAlloc<char>>;

int
caseins_streq(std::string_view const lhs, std::string_view const rhs);
