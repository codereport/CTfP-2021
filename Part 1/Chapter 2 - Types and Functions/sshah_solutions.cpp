#include <iostream>
#include <type_traits>
#include <map>
#include <functional>
#include <thread>
#include <chrono>
#include <cstdlib>
#include <ctime>

// Q1: Create and try out a memoize function
namespace detail{
// TODO: Get this working for lambdas(as they dont behave like function pointers)
template <typename T>
struct function_traits_helper;

template <typename RetType, typename...Args>
struct function_traits_helper<RetType(*)(Args...)>{
    using ReturnType = RetType;
    using ArgsTupleType = std::tuple<Args...>;
};
}

template <typename F>
struct function_traits: detail::function_traits_helper<std::decay_t<F>> {};


template <typename F>
auto memoize(F&& f) {
    using fn_info = function_traits<F>;
    using RetType = typename fn_info::ReturnType;
    using ArgsTupleType = typename fn_info::ArgsTupleType;
    using MapType = std::map<ArgsTupleType, RetType>;
    
    return [memo = MapType{}, f = std::forward<F>(f)](auto... args) mutable -> RetType {
        auto memoEntry = std::make_tuple(args...);
        if(auto it = memo.find(memoEntry); it != memo.end()) {
            return it->second;
        } else {
            // TODO: How to perfect forward the args below?
            auto myResult = f(args...);
            memo.emplace(memoEntry, myResult);
            return myResult;
        }
    };
}

template <typename F, typename... Args>
void measureMemoPerf(F f, Args... args) {
    using namespace std::chrono_literals;
    typedef std::chrono::high_resolution_clock Time;
    typedef std::chrono::milliseconds ms;
    
    auto memoized_fn = memoize(f);
    
    auto t0 = Time::now();
    std::cout << "First call answer: " << memoized_fn(args...) << std::endl;
    auto t1 = Time::now();
    std::cout << "Second call answer: " << memoized_fn(args...) << std::endl;
    auto t2 = Time::now();
    std::cout << "Time for first(in ms):" << std::chrono::duration_cast<ms>(t1 - t0).count() << std::endl;
    std::cout << "Time for second(in ms):" << std::chrono::duration_cast<ms>(t2 - t1).count() << std::endl;
    std::this_thread::sleep_for(1ms);
    std::cout << "Expected answer(non-memoized fn call): " << f(args...) << std::endl;
    std::cout << "\n\n";
}

int expensive_calculation_1(int elem) {
    using namespace std::chrono_literals;
    std::this_thread::sleep_for(10ms);
    return elem * 2;
}

std::string expensive_calculation_2(char s, int elem) {
    using namespace std::chrono_literals;
    std::this_thread::sleep_for(20ms);
    return std::string(s, elem);
}

uint32_t get_seed() {
    return std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count();
}

int unseeded_rng() {
    std::srand(get_seed());
    return std::rand();
}

int seeded_rng(uint32_t seed) {
    std::srand(seed);
    return std::rand();
}


int main(){
    
    // Q1: Test memoize with some expensive calculations
    measureMemoPerf(expensive_calculation_1, 10);
    measureMemoPerf(expensive_calculation_2, 20, 'x');
    
    // Q2: Try memoize a rng function from the C++ standard library
    // The unseeded version is not pure and as a result cannot be memoized
    measureMemoPerf(unseeded_rng);
    
    // Q3: Now try the same with a seeded random function
    // Yes, it works - given a starting seed, a rng is expected to produce the same sequence of numbers.
    measureMemoPerf(seeded_rng, get_seed());   
}


/**
 * Q4: Which of the provided C++ functions are pure?
 * (a) Yes, but the memoization implementation above will not use the memo for the intermediate
 * factorial calculations. We will require an open recursion based implementation for that.
 * (b) No, it depends on the state of stdin
 * (c) "std::cout" is a global/static and stateful object pointing to stdout. Accordingly, it fills up the output buffer as a side effect. 
 * (d) No, again we have a static "y" which means that every call to f will be stateful.
**/

// Q5: Functions from bool -> bool
struct BoolFunctions{
    static bool identity(bool x) { return x; };
    static bool complement(bool x) { return !x; };
    static bool falsify(bool x) { return false; };
    static bool truify(bool x) { return true; };
};

/**
 * Q6: Functions between unit/void/bool.
 * From Void: identity, absurd to Bool/Unit which are not actually callable.
 * From Unit: identity, return {true, false}
 * From Bool: identity, complement, return Unit, return {true, false}
 * 
**/

