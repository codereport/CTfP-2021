
// Chapter 1: Category

B f(A a);
C g(B b);

C g_after_f(A a) {
    return g(f(a));
}

template<class T> T id(T x) { return x; }

// Godbolt: https://godbolt.org/z/Yzf4Wh

std::reverse(v.begin(), v.end());
std::transform(v.begin(), v.end(), v.begin(), 
    [](auto e) { return e + 1; });

// Chapter 2: Types & Functions

// from book
int fact ( int n) {
    int result = 1;
    for (int i = 2; i <= n; ++ i)
        result *= i;
    return result;
}

// C++20: https://www.godbolt.org/z/xETPfo
using namespace std::ranges;

auto fact(int n) -> int {
    auto vals = views::iota(1, n);
    return std::reduce(begin(vals), end(vals), 1, std::multiplies{});
}

// C++23 potentially

using namespace std::ranges;

auto fact(int n) -> int {
    return views::iota(1, n) | fold(1, std::multiplies{});
}

// Chapter 9: Function Types

// https://www.godbolt.org/z/vj65KWdGx
auto sort_array_by_parity(auto& A) {
    std::ranges::partition(A, 
        [](auto e) { return e % 2 == 0; });   
    return A;
}
