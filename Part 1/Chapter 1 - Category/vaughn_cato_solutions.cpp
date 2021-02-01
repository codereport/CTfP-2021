#include <cassert>


static void challenge1()
{
  auto identity = [](auto x) { return x; };
  assert(identity(5) == 5);
}


static void challenge2()
{
  auto compose = [](auto f, auto g) { return [=](auto x){ return f(g(x)); }; };
  auto multiply_by_5 = [](auto x){ return x*5; };
  auto add_2 = [](auto x) { return x+2; };
  assert(compose(add_2,multiply_by_5)(2) == 12);
}


static void challenge3()
{
  auto identity = [](auto x) { return x; };
  auto compose = [](auto f, auto g) { return [=](auto x){ return f(g(x)); }; };
  auto multiply_by_5 = [](auto x){ return x*5; };
  auto add_2 = [](auto x) { return x+2; };
  assert(compose(identity,multiply_by_5)(2) == 2*5);
  assert(compose(identity,multiply_by_5)(7) == 7*5);
  assert(compose(add_2,identity)(7) == 9);
}


static void challenge4()
{
  // You could create a WWW category where the objects were web pages and
  // the arrows were reachabiliity.  Each page would be reachable from
  // itself by not clicking on anything.  Reachability composes since if
  // you can reach page A from page B, and reach page B from page C, you
  // can reach page A from page C.
}


static void challenge5()
{
  // Facebook wouldn't be a category if the arrows were friendship since
  // it doesn't compose (if A is a friend of B, and B is a friend of C,
  // then A is not necessarily a friend of C).
}


static void challenge6()
{
  // A directed graph is a category if there are edges from each node to
  // itself, and if it is always true that if there is an edge from node A to
  // node B and an edge from node B to node C, there is also an edge from
  // node A to node C.
}


int main()
{
  challenge1();
  challenge2();
  challenge3();
  challenge4();
  challenge5();
  challenge6();
}
