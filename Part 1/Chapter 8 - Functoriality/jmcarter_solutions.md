# Challenges 8.9

### Q.1 Show that the data type

> `data Pair a b = Pair a b`

> is a byfunctor. For additional credit implement all three methods of `Bifunctor` and use equational reasoning to show that these definitions are compatible with the default implementations whenever they can be applied.

We can show by implementing bimap for Pair, and showing that Pair is a Functor in each argument.

```haskell
instance Bifunctor (Pair a b) where
    bimap f g (Pair a b) = Pair (f a) (g b)
```

It is sufficient to show that it is a functor in each argument separately.

Let's show that Pair is a functor in the second argument (keeping a constant). We get:

```haskell
instance Functor (Pair a) where 
    fmap f (Pair a b) = Pair a (f b)
```

We need to show that this is indeed a Functor.
Let's check the functor laws (identity and composition)

First, let's verify identity is preserved.
```haskell
fmap id (Pair a b)
= Pair a (id b) -- definition of fmap
= Paid a b -- definition of id on b
= id Pair a b -- definition of id on pair
```

Next, let's check for composition

```haskell
fmap (g . f) (Pair a b)
= Pair a ((g . f) b) -- definition of fmap
= Pair a (g (f b)) -- definition of composition
= fmap g (Pair a (f b)) -- definition of fmap
= fmap g (fmap f (Pair a b)) -- definition of fmap
= (fmap g . fmap f) Pair a b -- definition of composition
```

The same can be done by fixing b (Pair a b = a `Pair` b) and showing that (`Pair` b) is a functor:

```haskell
instance Functor (`Pair` b) where 
    fmap f (a `Pair` b) = a `Pair` (f b)
    -- equivalent as fmap f (Pair a b) = Pair a (f b)
```

The proof is identical as for the one when fixing a (not shown here)

#### Extra credit:
Here are the 3 functions for Pair Bifunctor:

```haskell
class Bifunctor Pair where
    bimap f g (Pair x y) = Pair (f x) (g y)
    first f (Pair x y) = Pair (f x) y
    second g (Pair x y) = Pair x (g y)
```

recall that the default implementations of Bifunctor is :

```haskell
class Bifunctor b where 
    bimap f g = first f . second g
    first f = bimap f id
    second g = bimap id g 
```

Let's use equational reasoning on all 3 functions to show they are equivalent.

First, let's check first
```haskell
first f (Pair x y)
= bimap f id (Pair x y) -- Definition of standard first
= Pair (f x) (id y) -- Definition of Pair bimap
= Pair (f x) y -- QED
```

Second, let's check second
```haskell
second g (Pair x y)
= bimap id g (Pair x y) -- Definition of standard second
= Pair (id x) (g y) -- Definition of Pair bimap
= Pair x (g y) -- QED
```

Finally, let's check bimap
```haskell
bimap f g (Pair x y)
= (first f . second g) (Pair x y)
= first f (Pair x (g y)) -- Definition of second on Pair
= Pair (f x) (g y) -- Definition of first on Pair ... QED
```

### Q.2: Show the isomorphism between the standard definition of Maybe and this desugaring:
> `type Maybe' a = Either (Const () a) (Identity a)`

> Hint: Define two mappings between the two implementations. For additional credit, show that they are the inverse of each other using equational reasoning.

```haskell
f :: (Maybe' a) -> (Maybe a)
f (Left Const ()) = Nil
f (Right Identity x) = Just x
```

```haskell
g :: (Maybe a) -> (Maybe' a)
g Nil = Left Const ()  
g (Just x) = Right Identity x  
```

We can use equational reasoning to show f and g a inverse of each other

Show that f.g is id on Maybe a
```haskell
f . g (Nil)
= f (Left Const ()) -- Apply g to Nil
= Nil -- Apply g to Left Const ()
-- f . g -- id for Nil

f . g (Just x) 
= f (Right Identity x)
= Just x
-- f . g is id for Just x
```

Show that g.f is id on Maybe' a
```haskell
g . f (Left Const ())
= g Nil
= Left Const ()

g . f (Right Identity x)
= g (Just x)
= Right Identity x
-- g . f is id for Maybe' a
```


### Q.3 Let’s try another data structure. I call it a PreList because it’s a precursor to a List. It replaces recursion with a type parameter b.
> `data PreList a b = Nil | Cons a b`

> Show that PreList is an instance of Bifunctor.

To show that PreList is a Bifunctor, we simply need to show that `Cons a b` is a bifunctor. But we already shown that in Q.1 (`Cons a b` is isomorphic to `Pair a b`)
Since `Nil` is a functor, and Coproduct (`|`) is a bifunctor, and `Cons a b` is a Bifunctor, PreList is a Bifunctor (QED)


### Q.4 Show that the following data types define bifunctors in a and b:
> `data K2 c a b = K2 c`
```haskell
instance Bifunctor (K2 c) where 
    bimap _ _ (K2 c) = K2 c -- bimap simply disregard both functions
```
> `data Fst a b = Fst a`
```haskell
instance Bifunctor Fst where
    bimap f _ (Fst a) = Fst (f a)
```

> `data Snd a b = Snd b` 
```haskell
instance Bifunctor Snd where
    bimap _ g (Snd b) = Snd (g b) 
```

These implementations are identical from those in the clowns and jokers paper except that they explicitely write f and g in the functions definitions, whereas we used placeholders for disregarded parameters.


### Q.5 Define a bifunctor in a language other than Haskell. Implement
bimap for a generic pair in that language.

Let's try to define it in python

```python
from typing import Callable

class Bifunctor:
    def bimap(self, f: Callable, g: Callable):
        raise NotImplementedError

def bimap(f: Callable, g: Callable):
    return lambda bf: bf.bimap(f, g)

def first(f: Callable):
    return bimap(f, lambda x: x)

def second(g: Callable):
    return bimap(lambda x: x, g)


class Pair(Bifunctor):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def bimap(self, f: Callable, g: Callable):
        return Pair(f(self.a), g(self.b))

    def __repr__(self):
        return f'Pair({self.a}, {self.b})'

    def __str__(self):
        return f'({self.a}, {self.b})'

    def __eq__(self, other):
        return self.a == other.a and self.b == other.b


def test_pair_bifunctor():
    assert Pair(5, True).bimap(lambda x: x*x, lambda x: not x) == Pair(25, False)

    assert first(lambda x: x*x)(Pair(5, True)) == Pair(25, True)
    assert second(lambda x: not x)(Pair(5, True)) == Pair(5, False)
    assert bimap(lambda x: str(x), lambda x: str(x))(Pair(5, True)) == Pair("5", "True")
```


### Q.6 Should std::map be considered a bifunctor or a profunctor in the two template arguments Key and T? How would you redesign this data type to make it so?

First, using our intuition, since a map can be viewed as a function, where the key is the argument to a function, and the value the result of the function. (Actually, a pure function can be "Memoized" as a map, or even implemented as a map if we have infinite memory, so that checks out). Therefore, a map can be viewed as a Profunctor.

The only problem with std::map is when trying to access a non-existing key. To make std::map a true profunctor, we probably need to return an Optional from accessing a value in map, and return an empty when the key is not present, and an Optional(val) when the key is present. Doing it this way, then it becomes a profunctor. (assuming dimap, lmap and rmap are all well defined)
