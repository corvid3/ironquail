#include "str.hh"
#include <cctype>

template class std::basic_string<char, std::char_traits<char>, QAlloc<char>>;

bool
caseins_streq(std::string_view const lhs, std::string_view const rhs)
{
  if (lhs.size() != rhs.size())
    return false;

  using sty = decltype(lhs);

  for (sty::size_type i = 0; i < lhs.size(); i++) {
    sty::value_type const lhs_v = lhs[i];
    sty::value_type const rhs_v = rhs[i];

    if (std::tolower(lhs_v) != std::tolower(rhs_v))
      return false;
  }

  return true;
}
