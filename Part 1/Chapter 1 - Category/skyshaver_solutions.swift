// 1. Implement, as best as you can, the identity function 
// in your favorite language
func identity<T>(_ a: T) ->T { 
     a 
}

// 2. Implement the composition function in your favorite language.
// It takes two functions as arguments and returns a function that 
// is their composition.

func g_after_f<A, B, C>(a: A, f: (A) -> B, g: (B) -> C) -> C 
{
    g(f(a))
}

// example
func f<T: BinaryInteger>(a: T) -> Double {
    Double(a) + 0.13
}

func g<T>(b: T) -> String {
     "\(b)"
}

var str = g_after_f(a: 1, f: f, g: g)