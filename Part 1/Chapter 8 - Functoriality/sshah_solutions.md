---
title: CTFP Ch8 Challenges
created: '2021-03-26T17:18:01.373Z'
modified: '2021-03-27T00:38:52.775Z'
---

# CTFP Ch8 Challenges

### Q1
> Show that the data type:
```data Pair a b = Pair a b```
is a bifunctor. 
For additional credit implement all three methods of Bifunctor and use equational reasoning to show that these definitions are compatible with the default implementations whenever they can be applied.

From the text, it should be possible to show if we can prove functoriality on each individual argument.
Recollecting the Functor laws:
1. Preserve Identity: `fmap id = id`
2. Preserve Composition: `fmap (g.f) = (fmap g . fmap f)`

**Fixing the first argument**, the defn of fmap would be:
```
instance Functor (Pair a) where
  fmap f (Pair a x) = Pair a (f x)
```
- Checking Rule 1:
```
fmap id (Pair a x)
= {defn of fmap} Pair a (id x) = Pair a x
= {defn of id} id (Pair a x)
```
- Checking Rule 2:
```
fmap (g.f) (Pair a x)
= {defn of fmap} Pair a (g.f x)
= {defn of composition} Pair a (g(f(x)))
= {defn of fmap} fmap g (Pair a (f(x)))
= {defn of fmap} (fmap g . fmap f) (Pair a x)
``` 
This can similarly be shown if we fix the second argument.

**Extra Credit**
```
instance Bifunctor (Pair) where
  bimap f g (Pair x y) = Pair (f x) (g y)
  first f (Pair x y) = Pair (f x) y
  second g (Pair x y) = Pair x (g y)
```

Let's compare to the original implementation with equational reasoning to substitute our implementation:
```
bimap g h = first g . second h
Pair (g x) (h y) = first g (second h (Pair x y))
= first g (Pair x (h y))
= Pair (g x) (h y)

first g = bimap g id
Pair (g x) y = Pair (g x) (id y)
= Pair (g x) y

second h = bimap id h
Pair x (h y) = Pair (id x) (h y)
= Pair x (h y)
```

### Q2
> Show the isomorphism between the standard definition of Maybe and this desugaring:
`type Maybe' a = Either (Const () a) (Identity a)`

```
f :: Maybe a -> Maybe' a
f Just x = Right (Identity x)
f Nothing = Left (Const ())

f_rev :: Maybe' a -> Maybe a
f_rev Right (Identity x) = Just x
f_rev Left (Const ()) = Nothing
```

### Q3
> Let’s try another data structure. I call it a PreList because it’s a precursor to a List. It replaces recursion with a type parameter b.
`data PreList a b = Nil | Cons a b`
Show that PreList is an instance of Bifunctor.

We'll need to show that the functor laws are obeyed for `Nil` and for `Cons a b` while alternately fixing the arguments `a` and `b`. Below is the fmap defn with the first argument kept fixed.
```
instance Functor (PreList a) where
  fmap f Nil = Nil
  fmap f (Cons x y) = Cons x (f y)
```
- Checking Rule 1:
```
fmap id Nil
= {defn of fmap} Nil
= {defn of id} id (Nil)

fmap id Cons (x y)
= {defn of fmap} Cons x y
= {defn of id} id (Cons x y)
```
- Checking Rule 2:
```
fmap (g.f) Nil
= {defn of fmap} Nil
= {defn of fmap} fmap f Nil
= {defn of fmap} fmap g (fmap f Nil)
= (fmap g . fmap f) Nil

fmap (g.f) (Cons x y)
= {defn of fmap} Cons x (g.f y)
= {defn of composition} Cons x (g(f(y)))
= {defn of fmap} fmap g (Cons x (f(y)))
= {defn of fmap} (fmap g . fmap f) (Cons x y)
```
The same can be shown by fixing the second argument and varying the first argument.

### Q4
> Show that the following data types define bifunctors in a and b:
```
data K2 c a b = K2 c
data Fst a b = Fst a
data Snd a b = Snd b
```
Referred to the solution [here](https://github.com/awalterschulze/category-theory-for-programmers-challenges/blob/master).
```
instance Bifunctor (K2 c) where
  bimap _ _ (K2 c) = K2 c

instance Bifunctor Fst where
  bimap f _ (Fst a) = Fst (f a)

instance Bifunctor Snd where
  bimap _ g (Snd b) = Snd (g b)
```

### Q5
> Define a bifunctor in a language other than Haskell. Implement bimap for a generic pair in that language.

```cpp
// http://coliru.stacked-crooked.com/a/74b3eb01f3662a0c
template <typename A, typename B, typename C, typename D>
std::pair<C, D> bimap(std::function<C(A)> f, std::function<D(B)> g, std::pair<A, B> input) {
    return std::make_pair(f(input.first), g(input.second));
}
// Can similarly define fst, snd.
```

### Q6
> Should std::map be considered a bifunctor or a profunctor in the two template arguments Key and T? How would you redesign this data type to make it so?

Thanks to folks in the `functional-programming` slack group who helped me out with this question.

`std::map` could definitely be considered as a bifunctor as its possible to define a `bimap` that operates simultaneously on key-value pairs and transforms their individual types.
`bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2`.

If we use a function from `k -> Maybe v` to represent the map instead of using data, we can also consider it as a profunctor(Still don't fully understand why).
