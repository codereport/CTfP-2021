// requires -std=c++17

#include <iostream>
#include <optional>
#include <cmath>

auto identity = [](auto x) { return std::optional{x}; };
auto compose2 = [](auto f, auto g) {
  return [f,g](auto x) {
    auto const res1 = f(x);
    return res1.has_value() ? g(res1.value()) : std::nullopt;
  };
};

// define composition as binary infix operator for use in fold expression
template <typename F, typename G>
auto operator>>(F f, G g) { return compose2(f,g); }

// binary left fold of the pack of functions
template <typename ... Funcs>
auto compose(Funcs... funcs) { return (identity >> ... >> funcs); }

auto square          = [](auto x) { return std::optional(x*x); };
auto safe_root       = [](auto x) { return (x >= 0) ? std::optional{std::sqrt(x)} : std::nullopt; };
auto safe_reciprocal = [](auto x) { return (x != 0) ? std::optional{1/x}          : std::nullopt; };

// point-free function definition
auto safe_root_reciprocal = compose(safe_reciprocal, safe_root);

int main() {
  std::cout << "test compose2 and identity\n";
  std::cout << identity(42).value() << '\n';
  std::cout << (identity >> safe_root)(4).value() << '\n';
  std::cout << (safe_root >> identity)(4).value() << '\n';

  std::cout << "test compose\n";
  std::cout << compose()(2).value() << '\n';
  std::cout << compose(square)(2).value() << '\n';
  std::cout << compose(square,square)(2).value() << '\n';
  std::cout << compose(square,square,square)(2).value() << '\n';

  std::cout << "test std::optional properties\n";
  std::cout << safe_root_reciprocal(0.25).value() << '\n';
  std::cout << safe_root_reciprocal(4.).value() << '\n';
  std::cout << std::boolalpha << safe_root_reciprocal(-1.).has_value() << '\n';
  return 0;
}
