
B f(A a);
C g(B b);

C g_after_f(A a) {
    return g(f(a));
}
