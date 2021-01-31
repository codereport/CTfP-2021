
// Chapter 1

B f(A a);
C g(B b);

C g_after_f(A a) {
    return g(f(a));
}

template<class T> T id(T x) { return x; }
