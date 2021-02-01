// 1. Implement, as best as you can, the identity function in your favorite
// language (or the second favorite, if your favorite language happens to be
// Haskell).

auto id = [](auto &&x) { return std::forward<decltype(x)>(x);};

// I would expect code below to work, but it gets lost with rvalue references
// being bound to lvalue references :(
template <typename T> auto id(T &&x) { return std::forward<T>(x); }

// 2. Implement the composition function in your favorite language. It takes two
// functions as arguments and returns a function that is their composition.
template <typename T1, typename T2> auto compose(T1 &&f1, T2 &&f2) {
  return [&](auto &&...args) {
    return f1(f2(std::forward<decltype(args)>(args)...));
  };
}

// 3. Write a program that tries to test that your composition function respects
// identity.

// This is impossible, there are uncountably many functions to test.
// However, here's a sample program:
int main() {
  auto plus_one = [](double x) { return x + 1; };
  auto with_id = compose(id, compose(plus_one, id));
  std::cout << with_id(10.);
}

// 4. Is the world-wide web a category in any sense? Are links morphisms?

// Not all pages have a link to themselves, i.e., the unit of composition.

// 5. Is Facebook a category, with people as objects and friendships as morphisms?

// Without defining what the composition operation is, it's hard to answer
// this. A category needs three entities: the objects, the morphisms, and the
// composition function. Let's show that, no matter how we define
// composition, it would fail to be a *function*.

// Consider three people: A friends with B, and B friends with C, but A and B
// are not friends. Clearly Facebook has three such people.
// The composition function ∘ for A, B and C would be defined as:
// `∘: morph(B, C) x morph(A, B) -> morph(A, C)`,
// where morph(X, Y) is the **set** of all morphisms from X to Y.

// Now, we have established that the set `morph(A, C)` is empty. As such, no
// matter how we define ∘, it would fail to map elements of its non-empty
// Domain (`morph(B, C) x morph(A, B)`). As such, ∘ would fail to be a function.
// □

// Helpful explanation I found:
//  The composition operation is really a class function which for each triple
//  (𝑋,𝑌,𝑍)∈Obj(𝐶) outputs a function 𝜑(𝑋,𝑌,𝑍):Hom𝐶(𝑋,𝑌)×Hom𝐶(𝑌,𝑍)→Hom𝐶(𝑋,𝑍).
// https://math.stackexchange.com/questions/2574430/how-do-we-know-that-composition-of-morphisms-are-defined

// In the Facebook example, it's impossible to define such class function.

// 6. When is a directed graph a category?
// When nodes have self-edges and whenever the existence of edges A->B and B->C
// implies the existence of an edge A-> C. When this happens, we can define a
// composition operation that satisfies Associativity and existence of Identity.
