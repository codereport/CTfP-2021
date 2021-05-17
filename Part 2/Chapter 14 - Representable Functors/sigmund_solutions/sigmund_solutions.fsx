/// CTfP Chapter 14 Challenges

/// Question 4: Using the `Stream` representation, memoize a function that squares its argument
/// 
printfn "CTfP - Chapter 14 - Question 4"

// No typeclasses or HKT in fSharp, so we must do one-offs
type Stream<'t> = Cons of 't * Lazy<'t Stream>
type StreamAlpha<'a, 't> = ('a -> 't) -> 't Stream
type StreamBeta<'a, 't> = 't Stream -> ('a -> 't)

let rec streamTabulate<'a> : StreamAlpha<int, 'a> = 
    fun f -> Cons (f 0, lazy streamTabulate (((+) 1) >> f))

let rec streamIx<'a> : StreamBeta<int, 'a> = 
    fun s n -> 
        match (s, n) with
        | Cons (b,  _), 0 -> b
        | Cons (_, bs), n -> streamIx (bs.Force()) (n - 1)

let memoSqare = streamTabulate (fun x -> x * x)

[0..10] 
    |> List.map ( (memoSqare |> streamIx) >> sprintf "%i" ) 
    |> String.concat ", " 
    |> printfn "  %s"
// Output:
//   0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100



/// Question 6: The functor `Pair a = Pair a a` is representable. Can you guess the type that
/// represents it? Implement `tabulate` and `index`.
///
printfn "CTfP - Chapter 14 - Question 6"

// It seems that the represeting type for `Pair a a` is `Bool`, or - equivalently - any type of
// cardinality 2.

// Once again, sadly no typeclasses/HKT
type Pair<'t> = Pair of 't * 't
type PairAlpha<'a, 't> = ('a -> 't) -> 't Pair
type PairBeta<'a, 't> = 't Pair -> ('a -> 't)

type Side =
    | Lt
    | Rt

let rec pairTabulate<'a> : PairAlpha<Side, 'a> = 
    fun f -> Pair (f Lt, f Rt)

let rec pairIx<'a> : PairBeta<Side, 'a> =
    fun p s -> 
        match (p, s) with
        | Pair (l, _), Lt -> l
        | Pair (_, r), Rt -> r

let memoPair = 
    pairTabulate (
        function
        | Lt -> "left"
        | Rt -> "right" )
    
printfn "  %s" ((memoPair |> pairIx) Lt)
printfn "  %s" ((memoPair |> pairIx) Rt)

// Output:
//   left
//   right