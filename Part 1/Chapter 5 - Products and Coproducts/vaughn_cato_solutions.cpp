#include <variant>
#include <cassert>


template<class... Ts>
struct overloaded : Ts...
{
  using Ts::operator()...; 
  overloaded(Ts... arg) : Ts(arg)... {}
};


int main()
{
  // Challenge 6
  ///////////////

  auto i = [](int n) { return n; };
  auto j = [](bool b) { return b ? 0 : 1; };

  auto m = [&](std::variant<int,bool> e) -> int
  {
    return std::visit(overloaded{i, j}, e);
  };

  assert(m(3) == 3);
  assert(m(false) == 1);
}
