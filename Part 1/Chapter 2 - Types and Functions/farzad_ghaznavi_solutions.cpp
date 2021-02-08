#include <algorithm>
#include <chrono>
#include <functional>
#include <iostream>
#include <map>
#include <string>
#include <tuple>
#include <thread>

using namespace std;

template<
    size_t I = 0,
    typename... Ts,
    typename std::enable_if<I == sizeof...(Ts), bool>::type = true
>
string toString(tuple<Ts...> t){
    return "";
}

template<
    size_t I = 0,
    typename... Ts,
    typename std::enable_if<I < sizeof...(Ts), bool>::type = true
>
string toString(tuple<Ts...> t){
    string tmp = toString<I+1>(t);
    return to_string(get<I>(t)) + ( tmp.empty() ? "" : "," + tmp );
}

// #1
template<
    typename Output,
    typename... Input
>
function<Output(Input...)> Memoize(function<Output(Input...)> f, string name) {
    map<tuple<Input...>, Output> m;

    return [f, m, name](Input... input) mutable -> Output {
        tuple<Input...> args = tie(input...);

        if (m.contains(args)) {
            cout << name << "(" << toString(args) << ") --> " << m[args] << endl;
        }
        else {
            cout << "Calculating " << name << "(" << toString(args) << ")" << endl;
            this_thread::sleep_for(2000ms);
            m.emplace(args, f(input...));
        }

        return m[args];
    };
}



// #2
// No.



// #3
void MemoizeRand() {
    function<int(unsigned)> randomize = [](unsigned seed) {
        srand(seed);
        return rand();
    };

    auto randomize_memoized = Memoize(randomize, "randomize");


    cout << randomize_memoized(1) << endl;
    cout << randomize_memoized(1) << endl;
}



// #4
//  a -> yes
//  b -> no
//  c -> yes
//  d -> no



int main() {
    function<int(int)> fib;
    function<int(int)> fib_memoized;
    fib = [&fib, &fib_memoized](int n) {
        if (n <= 1) {
            return n;
        }

        return fib_memoized(n-1) + fib_memoized(n-2);
    };

    fib_memoized = Memoize(fib, "fib");

    fib_memoized(1);
    fib_memoized(2);
    fib_memoized(3);
    fib_memoized(4);
    fib_memoized(5);
    fib_memoized(6);
    fib_memoized(7);

    fib_memoized(1);
    fib_memoized(2);
    fib_memoized(3);
    fib_memoized(4);
    fib_memoized(5);
    fib_memoized(6);
    fib_memoized(7);

    MemoizeRand();


    return 0;
}
