#include "str.hh"
#include <cctype>

template class std::basic_string<char, std::char_traits<char>, QAlloc<char>>;

int
caseins_streq(std::string_view const lhs, std::string_view const rhs)
{
  if (lhs.size() != rhs.size())
    return 1;

  using sty = decltype(lhs);
  sty::iterator lhs_i = lhs.cbegin();
  sty::iterator rhs_i = rhs.cbegin();

  while (lhs_i != lhs.cend()) {
    sty::value_type const lhs_v = *lhs_i;
    sty::value_type const rhs_v = *rhs_i;

    if (std::tolower(lhs_v) != std::tolower(rhs_v))
      return 1;

    lhs_i++;
    rhs_i++;
  }

  return 0;
}
