
// Chapter 1

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
