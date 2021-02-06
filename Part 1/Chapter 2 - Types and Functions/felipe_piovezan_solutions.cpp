#include <functional>
#include <iostream>
#include <map>
#include <tuple>
#include <vector>
#include <random>

// 1. Define a higher-order function (or a function object) memoize in your
// favorite language.

template <typename... ArgTys, typename F>
auto memoize(F &&f) {
  using RetTy = typename std::invoke_result<F, ArgTys...>::type;
  using KeyTy = std::tuple<ArgTys...>;
  using MapTy = std::map<KeyTy, RetTy>;

  return [f = std::forward<F>(f),
          memo = MapTy{}](ArgTys &&...args) mutable -> RetTy & {
    auto key = KeyTy(args...);
    auto cached = memo.find(key);
    if (cached != memo.end())
      return cached->second;
    auto [it, success] =
        memo.emplace(std::move(key), f(std::forward<ArgTys>(args)...));
    return it->second;
  };
}

void test1() {
  auto lambda = memoize<int, int>(
      [](int num, int val) { return std::vector<int>(num, val); });

  auto print_collection = [](const auto &x) {
    for (auto element : x)
      std::cout << element << ", ";
    std::cout << "\n";
  };

  assert(&lambda(2, 1) == &lambda(2, 1));
  print_collection(lambda(2, 1));
  print_collection(lambda(3, 2));
  assert(&lambda(2, 1) == &lambda(2, 1));
  assert(&lambda(3, 2) == &lambda(3, 2));
}

// 2. Try to memoize a function from your standard library that you normally use
// to produce random numbers. Does it work?

// Memoization only applies to pure functions.


// 3. Most random number generators can be initialized with a seed. Implement a
// function that takes a seed, calls the random number generator with that seed,
// and returns the result. Memoize that function. Does it work?

void test3() {
  auto single_random_number = [](auto seed) {
    auto rng = std::mt19937(seed);
    return rng();
  };
  auto memo = memoize<int>(single_random_number);
  std::cout << memo(0) << "\n";
  std::cout << memo(0) << "\n";
  assert(&memo(0) == &memo(0));
  assert(&memo(1) == &memo(1));
  std::cout << memo(1) << "\n";
}


// 4. Which of these C++ functions are pure?
// Only the factorial function.

// 5. How many different functions are there from Bool to Bool?
auto f0 = [] (bool b) { return true; };
auto f1 = [] (bool b) { return false; };
auto f2 = [] (bool b) { return b; };
auto f3 = [] (bool b) { return !b; };

// 6. Draw a picture of a category whose only objects are the types Void, ()
// (unit), and Bool; with arrows corresponding to all pos- sible functions
// between these types. Label the arrows with the names of the functions.

//
//         id_void
//                                                   +-+------+
//         +-+                                       | |      |
//         | |                                       | |true  |
//         + v             absurd                    + v      |
//    +--+ VOID +------------------------------->   BOOL<-----+
//    |                         +---------------->  ^  ^ false|
//    |                         |   +-------------> |  |      |
//    |                         |   |               |  |not   |
//    | absurd                  |   |               +---------+
//    |                         |   |                        id_bool
//    |                         |   |
//    |                         |   |                   +
//    |                         |   |                   |
//    |                         |   |                   |
//    |                         |   |                   |
//    |                         |   |                   |
//    |                   false |   | true              |
//    |                         |   |                   |
//    |                         |   |                   |  constant
//    |                                                 |
//    +---------------------->  UNIT <------------------+
//                              + ^
//                              | |
//                              | |
//                              | |
//                              +-+
//                              id_unit

int main() {
  test1();
  test3();
}
