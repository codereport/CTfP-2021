// https://www.godbolt.org/z/G8WzE4

#include <map>
#include "fmt/core.h"

// Challenge 1

auto memoize(auto fn) {
    return [done = std::map<int,int>{}, fn](auto n) mutable  {
        if (auto it = done.find(n); it != done.end())
            return it->second;
        auto const val = fn(n);
        done[n] = val;
        return val;
    };
}

// Challenge 2
// rand is not a pure function so it definitely won't work

// Challenge 3
// https://en.wikipedia.org/wiki/Linear_congruential_generator

struct lcg {
    auto operator()(int seed) {
        return (seed * 8121 + 28411) % 134456;
    }
};

// Challenge 4
// 1) factorial is pure
// 2) getchar is not pure
// 3) f is not pure
// 4) f is not pure

// Challenge 5

auto same_(bool b) -> bool { return b;     }
auto diff_(bool b) -> bool { return !b;    }
auto true_(bool b) -> bool { return true;  }
auto fals_(bool b) -> bool { return false; }

// Challenge 6

// ¯\_(ツ)_/¯

auto main() -> int {

    auto plus1_memo = memoize([](auto e) { return e + 1; });

    fmt::print("{}\n", plus1_memo(1));
    fmt::print("{}\n", plus1_memo(2));
    fmt::print("{}\n", plus1_memo(1));

    auto lcg_memo = memoize(lcg{});

    fmt::print("{}\n", lcg_memo(1)); // 36532
    fmt::print("{}\n", lcg_memo(1)); // 36532
    fmt::print("{}\n", lcg_memo(2)); // 44653

    return 0;
}
