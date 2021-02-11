open System
open System.Collections.Generic
open FSharp.Core

// Ex 1 : Define a higher-order function (or a function object) memoize in
//        your favorite language.

let memoize (f:'a->'b) = 
    let memo = Dictionary<_,_>()
    let innerProcess x = 
        match memo.TryGetValue x with
        | true , x -> 
            x
        | false, _ -> 
            let v = f x
            memo.Add (x, v)
            v
    innerProcess

//test :
let isPrime (n:int) = 
    [2..Math.Sqrt(float n)|>int] |> List.forall (fun i -> n % i <> 0)
    
let MemoizedIsPrime = 
    memoize isPrime
// Ex2 : Try to memoize a function from your standard library that you
//       normally use to produce random numbers. Does it work?
//       No, same input will produce same output and since unit is 1 value then we'll always 
//       get the same output 
let randomize = 
    let r = System.Random()
    fun () -> r.Next()
let MemoizedRandomize = 
    memoize randomize

// Ex3 : Most random number generators can be initialized with a seed.
//       Implement a function that takes a seed, calls the random number
//       generator with that seed, and returns the result. Memoize that
//       function. Does it work?
//       Yes in a way, same seed will produce same sequence 
//        

let seededRandomize seed = 
    let r = Random(seed)
    fun () -> r.Next()

let MemoizedSeededRandomize =
    memoize seededRandomize

// Ex4: Determine which functions are pure
    //  1) Y
    //  2) N
    //  3) N interacts with cout (side effects)
    //  4) N has a statically scoped variable making it tapping into the static state of the program


// Ex5 : How many different functions are there from Bool to Bool? Can
//       you implement them all?
//      - there are 2*2 possible combinations of [0,1]
let ``true``  _ = true
let ``false`` _ = false
let negate x    = not x
let id (x:bool) = x 

// Ex6 : Category consisting of Void, Unit and Bool
    //  See manu_kamath_diagram_Q6.md