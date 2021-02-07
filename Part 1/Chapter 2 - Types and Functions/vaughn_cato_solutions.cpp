#include <cassert>
#include <iostream>
#include <map>
#include <sstream>
#include <random>

using std::cerr;


namespace {
template <typename In, typename Out>
struct Table {
  static inline std::map<void *, std::map<In,Out>> entries;
};
}


static auto memoize = [](auto f)
{
  return [f](auto... x) mutable
  {
    auto args = std::tuple{x...};
    using In = decltype(args);
    using Out = decltype(f(x...));
    std::map<In, Out> &table = Table<In, Out>::entries[&f];
    auto iter = table.find(args);

    if (iter != table.end()) {
      return iter->second;
    }

    Out result = f(x...);
    table.insert({args,result});
    return result;
  };
};


static void challenge1()
{
  struct Fibonacci {
    int64_t operator()(int64_t x) const
    {
      if (x < 2) return 1;
      return (*this)(x-2) + (*this)(x-1);
    }
  };

  Fibonacci fibonacci;

  auto memoized_fibonacci = memoize(fibonacci);
  assert(memoized_fibonacci(1) == 1);
  assert(memoized_fibonacci(2) == 2);
  assert(memoized_fibonacci(3) == 3);
  assert(memoized_fibonacci(4) == 5);

  auto make_str = [](auto x){ std::ostringstream s; s << x; return s.str(); };

  cerr << "Begin 1: " << memoized_fibonacci(40) << "\n";
  cerr << "Begin 2: " << memoized_fibonacci(40) << "\n";
  auto memoized_make_str = memoize(make_str);
  assert(memoized_make_str(1) == "1");
  assert(memoized_make_str(1) == "1");
  assert(memoized_make_str(2.5) == "2.5");
  assert(memoized_make_str(2.5) == "2.5");
}


static void challenge2()
{
  std::mt19937 engine(/*seed*/1);
  auto f = [&](){ return std::uniform_int_distribution<int>(1,6)(engine); };
  auto fm = memoize(f);
  auto f1 = f();
  auto f2 = f();
  auto f3 = f();
  auto fm1 = fm();
  auto fm2 = fm();
  auto fm3 = fm();
  assert(fm2 == fm1);
  assert(fm3 == fm1);
}


static void challenge3()
{
  auto f = [&](int seed){
    std::mt19937 engine(seed);
    return std::uniform_int_distribution<int>(1,6)(engine);
  };

  auto fm = memoize(f);
  auto f2 = memoize(std::uniform_int_distribution<int>(1,6));
  assert(fm(1) == f(1));
  assert(fm(12) == f(12));
  assert(fm(18) == f(18));
}


static void challenge4()
{
}


static void challenge5()
{
  auto bool_func = [](int i){
    return [i](bool a){ return a ? bool(i & 1) : bool(i & 2); };
  };

  assert(bool_func(0)(false) == false);
  assert(bool_func(0)(true) == false);
  assert(bool_func(3)(false) == true);
  assert(bool_func(3)(true) == true);

  // 4 functions
}


static void challenge6()
{
  // One function from Void to Void : absurd void
  // One function from Void to Unit
  // Two functions from Void to Bool
  // One function from Unit to Void : absurd unit
  // One function from Unit to Unit
  // Two Functions from Unit to Bool
  // One function from Bool to Void : absurd bool
  // One function from Bool to Unit
  // 4 functions from Bool to Bool
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
