using System

//Q1 : Implemet identity
T identity<T>(T x) => x;

//Q2 : Implement composition
Func<A,C> compose<A,B,C>(Func<A,B> f, Func<B,C> g) => (x) => g(f(x));

//Q3 : Test that composition function respects identity
bool test<A,B>(Func<A,B> f,A input, B expected) where B : class {
    bool[] tests = {
                    compose<A,B,B>(f,identity)(input) == expected,
                    compose<A,A,B>(identity,f)(input) == expected,
                   };
    return tests.All((x) => x == true);
} 
