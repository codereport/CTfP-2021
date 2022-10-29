// 1.
func identity<A>(_ a: A) -> A { a }

assert(identity(42) == 42)

// 2.
func combine<A, B, C>(
  _ f: @escaping (A) -> B,
  _ g: @escaping (B) -> C
) -> (A) -> C {
  {
    g(f($0))
  }
}

// 3.
var f: (Int) -> Bool = { $0 > 0 }
var g: (Bool) -> String = { "\($0)" }
var combined = combine(f, g)
assert(combined(-1) == "false")
