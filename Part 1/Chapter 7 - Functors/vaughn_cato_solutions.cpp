#include <functional>
#include <iostream>
#include <cassert>
#include <optional>
#include <sstream>
#include <string>

using std::function;
using std::optional;
using std::nullopt;
using std::istream;
using std::istringstream;
using std::string;


template <typename A>
auto compose = []<typename B, typename C>(function<auto(B)->C> f)
{
  return
    [f](function<auto (A)->B> g) -> function<auto (A) -> C>
    {
      return [f,g](A a){ return f(g(a)); };
    };
};


struct Optional {
  static inline auto map =
    []<typename A>(A a) -> optional<A> { return a; };

  static inline auto fmap =
    []<typename A, typename B>(function<auto (A) -> B> f)
    {
      return [f](optional<A> Fa) -> optional<B>
      {
        if (Fa) return f(*Fa);
        else return nullopt;
      };
    };
};


template <typename T>
static auto read(istream &stream)
{
  T x;
  stream >> x;
  return x;
}


template <typename R> struct Reader
{ 
  template <typename T>
  static inline function map = [](R r) -> T { return read<T>(r); };

  static inline auto fmap = compose<R>;
};


static void testOptionalFunctor()
{
  {
    // Map objects
    optional<int> maybe_5 = Optional::map(5);
    assert(maybe_5 == 5);
  }
  {
    // Map identity
    auto id = [](auto x) { return x; };
    function<auto(int)->int> id_int = id;
    assert(Optional::fmap(id_int)(5) == 5);
    assert(Optional::fmap(id_int)(nullopt) == nullopt);
  }
  {
    // Map an arbitrary function
    function<auto (int)->float> f = [](int a) { return a+0.5f; };
    assert(Optional::fmap(f)(5) == 5.5f);
    assert(Optional::fmap(f)(nullopt) == nullopt);
  }
  {
    // Map a composed function
    function<auto(int)->int> g = [](int x){ return x + 1; };
    function<auto(int)->int> f = [](int x){ return x * 2; };

    function<auto (optional<int>) -> optional<int>> gf_prime =
      Optional::fmap(compose<int>(g)(f));

    assert(gf_prime(5) == 11);
    assert(gf_prime(nullopt) == nullopt);
  }
}


static void testReaderFunctor()
{
  {
    // Test mapping an int to a reader of ints
    function int_reader = Reader<istream&>::map<int>;
    istringstream stream("5");
    assert(int_reader(stream) == 5);
  }
  {
    // Testing mapping identity
    istringstream stream("7");
    function id = [](int x){ return x; };
    function int_reader = Reader<istream&>::map<int>;
    assert(Reader<istream&>::fmap(id)(int_reader)(stream) == 7);
  }
  {
    // Testing mapping an int-to-string function to an
    // int-reader-to-string-reader function
    istringstream stream("6");
    function int_to_string = [](int x){ return std::to_string(x); };
    function int_reader = Reader<istream&>::map<int>;
    auto fmap = Reader<istream&>::fmap;
    function int_reader_to_string_reader = fmap(int_to_string);
    function string_reader = int_reader_to_string_reader(int_reader);
    assert(string_reader(stream) == "6");
  }
}


int main()
{
  testOptionalFunctor();
  testReaderFunctor();
}
