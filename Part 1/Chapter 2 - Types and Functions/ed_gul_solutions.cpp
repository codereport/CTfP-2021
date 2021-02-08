#include <iostream>

#include <map>
#include <stdlib.h>
#include <time.h>
#include <chrono>

// helpers
int sum(int i, int j) { std::cout << "Calculating sum for: " << i << " + " << j << std::endl; return i + j; }
int succ(int i) { std::cout << "Calculating succ for: " << i << std::endl; return i + 1; } double succDouble(int i) { std::cout << "Caluating succDouble for: " << i << std:: endl; return static_cast<double>(i+1)+0.5; }
unsigned long time_since_epoch_count() { return std::chrono::system_clock::now().time_since_epoch().count(); }
unsigned long long fact(int n)
{
    unsigned long long result = 1;
    for (int i = 2; i <= n; i++)
        result *= i;
    return result;
}

// TODO: add perfect forwarding
// #1 memoize : returns memoized version of argument function
template <class F > auto memoize(F f)
{
    auto mem_f = [f](auto... args) { 
        std::tuple<decltype(args)...> myTuple(args...); // tuple to pass into std::maps template param (might be better way)
        static std::map<std::tuple<decltype(args)...>, decltype(f(args...))> results; // map: input -> f(input)
        if (results.find(myTuple) != results.end())
        {
            return results[myTuple];
        }
        results[myTuple] = f(args...);
        return results[myTuple];
    };
    return mem_f;
}

// #2 memoize rand : 
// * It "works" in a sense that the randInt function is not called a second time on the same inputs. 
// * It doesn't work in a sense that the randInt function no longer performs its job
void test_memoize_rand()
{
    auto randInt = [](){
        srand(time_since_epoch_count());
        std::cout << "Calculating random int" << std::endl;
        return rand() % 100;
    };
    
    auto mem_rand_int = memoize(randInt);
    std::cout << mem_rand_int() << std::endl; // 
    std::cout << mem_rand_int() << std::endl; // prints same as above
}

// #3 memoize rand seeded :
// * random functionality works
// * but not really a point in memoizing since input should never be the same so random function will always be called
void test_memoize_rand_seeded()
{
    auto randInt = [](int seed){
        srand(seed);
        std::cout << "Calculating random int" << std::endl;
        return rand() % 100;
    };

    auto mem_rand_int_seeded = memoize(randInt);
    std::cout << mem_rand_int_seeded(time_since_epoch_count()) << std::endl; 
    std::cout << mem_rand_int_seeded(time_since_epoch_count()) << std::endl; // prints different than above
}


int main(int argc, char *argv[])
{

    return 0;
}

void test_memoize()
{
    auto memoizedSucc = memoize(succ);
    std::cout << memoizedSucc(5) << std::endl;
    std::cout << memoizedSucc(5) << std::endl;
   
    auto memoizedSum = memoize(sum);
    std::cout << memoizedSum(5,6) << std::endl;
    std::cout << memoizedSum(5,6) << std::endl;
    std::cout << memoizedSum(1,6) << std::endl;
    std::cout << memoizedSum(1,6) << std::endl;
}


