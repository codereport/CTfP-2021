open System


//Q1 : Implemet identity
//     function composition done with the << or the >> operators 
let (>>) f g = f >> g
//Q2 : Implement composition
//     identity function is built in as id
let identity x = x

//Q3 : Test that composition function respects identity
let test = 
    let check f x y = 
        f x = y
    [|
        check (identity >> identity) 23 23
        check (identity >> ((+) 6)) 17 23
        check ((+) 6 >> identity) 17 23
    |] |> Array.fold (( && )) true
    
// 1.4.5, . Is Facebook a category, with people as objects and friendships asmorphisms?
//  No because it doesn't follow the rule of associativity 
// let f (a, b) = a is friends with b
// f (a, b) and f (c, b) does not imply f (a, c)

//  1.4.6, when is a directed graph a category
//  if every node has a self referencing edge and and the graph (with out those self referencing edges) 
//  is a complete graph