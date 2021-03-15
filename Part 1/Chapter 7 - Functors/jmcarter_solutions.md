# Challenges 7.4

### Q.1 : check functors law for `fmap _ _ = Nothing`

Functor Laws:
1. Map Id to Id : `fmap id_a = id_fa`
2. Preserves composition : `fmap (f.g) a = (fmap f) . (fmap g) a`

- fmap id x = Nothing (def of fmap)
- Nothing = id Nothing (def of id)
- expected id (Just x) (Does not work for Just x)

Since the first functor law doesn't hold for Just a, it fails to be a functor. (no need to check for Nothing, and to check law 2)


### Q.2 : Check functor laws for Reader Functor

fmap for reader is defined as `fmap = (.)`

Check Law 1:
```haskell
fmap id f
= (.) id f = id . f -- definition of fmap
= f = id f -- definition of composition with id
``` 

Check Law 2:
```haskell
fmap (h . g) f
= (.) (h . g) f = (h . g) . f -- Definition of fmap
= h . (g . f) -- Associativity law
= h . fmap g f  -- Definition of fmap
= (fmap h . fmap g) f -- Definition of fmap and Associativity
```

Both laws hold.


### Q.3 : Implement the reader functor in your second favorite language

Since fmap fro the reader functor is simply 'composition', then all we need is to implement function composition (which we did in Chapter 1). Here is a simple (non-variadic) implementation:
```python
def compose(f, g):
    return lambda x: f(g(x)) 
```


### Q.4 : Prove the functor laws for the list functor.

Here is the definition of fmap for List functor:

```haskell
instance Functor list where
    fmap _ Nil = Nil
    fmap f Cons x t = Cons (f x) (fmap f t)
```

Let's check id law with equational reasoning:

```haskell
fmap id Nil
= Nil -- Def of fmap
= id Nil -- Def of id


fmap id Cons x t
= Cons (id x) (fmap id t) -- Def of fmap
= Cons x (fmap id t) -- Def of id
= Cons x t -- by induction
= id Cons x t -- Def of id
```

Now let's check composition law with equational reasoning:

```haskell
fmap (f . g) Nil
= Nil -- Definition of fmap
= fmap f Nil -- Definition of fmap
= fmap f (fmap g Nil) -- Definition of fmap
= (fmap f) . (fmap g) Nil -- Definition of composition

fmap (f . g) Cons x t
= Cons (f . g x) (fmap (f . g) t) -- Def of fmap
= Cons (f . g x) ((fmap f . fmap g) t) -- By induction
= Cons (f (g x)) (fmap f (fmap g t)) -- Def of composition
= fmap f (Cons (g x) (fmap g t)) -- Def of fmap
= (fmap f) . fmap g Cons x t -- Def of fmap
= ((fmap f) . (fmap g)) Cons x t -- Def of composition

```


Ouf ... that was a mouthful!