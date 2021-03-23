#### Q1: Show that the data type `data Pair a b = Pair a b` is a bifunctor.

```hs
instance Bifunctor Pair where
    bimap f g (Pair a b) = Pair (f a) (g b)
    first f   (Pair a b) = Pair (f a) b
    second g  (Pair a b) = Pair a (g b)
```

#### Q2: Show the isomorphism between the standard definition of Maybe and this desugaring `type Maybe' a = Either ( Const () a) ( Identity a)`

```hs
maybeToEither :: Maybe a -> Either (Const () a) (Identity a)
maybeToEither Nothing = Left (Const ())
maybeToEither (Just x) = Right (Identity x)

eitherToMaybe :: Either (Const () a) (Identity a) -> Maybe a
eitherToMaybe (Left (Const ())) -> Nothing
eitherToMaybe (Right (Identity x)) -> Just x
```

#### Q3: Show that `PreList` is an instance of Bifunctor.

See [here](https://github.com/awalterschulze/category-theory-for-programmers-challenges/blob/master/108-Functoriality.md#83-lets-try-another-data-structure-i-call-it-a-prelist-because-its-a-precursor-to-a-list-it-replaces-recursion-with-a-type-parameter-b)

#### Q4: Show that the following data types define bifunctors in `a` and `b`:

```hs
instance Bifunctor (K2 c) where 
    bimap _ _ (K2 c) = K2 c

instance Bifunctor Fst where
    bimap f _ (Fst a) = Fst (f a)

instance Bifunctor Snd where
    bimap _ g (Snd b) = Snd (g b) 
```

#### Q5: Define a bifunctor in a language other than Haskell.

[Godbolt](https://www.godbolt.org/z/8fhK8v)
```cpp
using Fn = int(int); // cheating for simplicity

template <typename B>
concept bifunctor = requires(B bf, Fn f, Fn g) {
    { bf.bimap(f, g) } -> std::same_as<B>;
};

struct pair {
    int a, b;
    auto bimap(auto f, auto g) const {
        return pair{f(a), g(b)};
    }
};
```
#### Q6: Should std::map be considered a bifunctor or a profunctor in the two template arguments `Key` and `T`?

The internet says profunctor.

