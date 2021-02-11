using System;
using System.Collections.Generic;

//Q1 : Memoize a function
Func<A,B> memoize<A,B>(Func<A,B> f) {
    Dictionary<A,B> cache = new();
    return (A x) => {
        Func<B> output =  cache.ContainsKey(x) switch {
                        true => () => cache[x],
                        _    => () => {
                            cache.Add(x,f(x));
                            return cache[x];
                        }
                    };
        return output();
    };
}

decimal factorial(decimal n) => n == 0? 1 : n * factorial(n-1);
Func<decimal,decimal> memoizedFactorial = memoize<decimal,decimal>(factorial);

Console.WriteLine(memoizedFactorial(23));
Console.WriteLine(memoizedFactorial(23));

// Ex2 : Try to memoize a function from your standard library that you
//       normally use to produce random numbers. Does it work?
//       No, same input will produce same output and since unit is 1 value then we'll always 
//       get the same output 
int randomize(int i) {
    var r = new System.Random();
    return r.Next(i);
}

Func<int,int> MemoizedRandomize =  memoize<int,int>(randomize);


// Ex3 : Most random number generators can be initialized with a seed.
//       Implement a function that takes a seed, calls the random number
//       generator with that seed, and returns the result. Memoize that
//       function. Does it work?
//       Yes in a way, same seed will produce same sequence 
//        

int seededRandomize(int seed) {
    var r = new System.Random(seed);
    return r.Next();
}

Func<int,int> MemoizedSeededRandomize =  memoize<int,int>(seededRandomize);

// Ex4: Determine which functions are pure
    //  1) Y
    //  2) N
    //  3) N interacts with cout (side effects)
    //  4) N has a statically scoped variable making it tapping into the static state of the program


// Ex5 : How many different functions are there from Bool to Bool? Can
//       you implement them all?
//      - there are 2*2 possible combinations of [0,1]
Func<bool,bool> True  => (x) => true ;
Func<bool,bool> False => (x) => false;
Func<bool,bool> not   => (x) => !x   ;
Func<bool,bool> id    => (x) => x    ;

// Ex6 : Category consisting of Void, Unit and Bool
    //  See manu_kamath_diagram_Q6.md

