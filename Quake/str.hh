#pragma once

#include "mem.hh"
#include <string>

template<template<typename M> typename A = QAlloc>
using q_str = std::basic_string<char, std::char_traits<char>, A<char>>;
extern template class std::
  basic_string<char, std::char_traits<char>, QAlloc<char>>;

bool
caseins_streq(std::string_view const lhs, std::string_view const rhs);
