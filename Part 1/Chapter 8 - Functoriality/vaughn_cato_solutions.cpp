#include <utility>
#include <cassert>

using std::pair;


constexpr auto id = [](auto x){ return x; };


struct Pair {
  static constexpr auto map = [](auto a, auto b) { return pair(a,b); };

  static constexpr auto bimap = [](auto g, auto h)
  {
    return
      [g,h]<typename A, typename B>(pair<A,B> x){
        return map(g(x.first), h(x.second));
      };
  };

  static constexpr auto first = [](auto g) { return bimap(g, id); };
  static constexpr auto second = [](auto h) { return bimap(id, h); };
};


constexpr auto challenge5 = []
{
  using A = int;
  using B = float;
  using C = double;
  using D = char;
  A a = 5;
  B b = 2.5f;
  C c = 3.5;
  D d = 'd';
  assert(Pair::map(a,b) == pair(a,b));
  assert(Pair::map(c,d) == pair(c,d));
  auto g = [](A a){ return C(a); };
  auto h = [](B b){ return D('a' + int(b)); };
  assert(Pair::bimap(g,h)(Pair::map(a,b)) == pair(g(a),h(b)));
  assert(Pair::first(g)(Pair::map(5,2.5)) == Pair::map(g(5),2.5));
  assert(Pair::second(h)(Pair::map(5,2.5)) == Pair::map(5,h(2.5)));
};


int main()
{
  challenge5();
}
