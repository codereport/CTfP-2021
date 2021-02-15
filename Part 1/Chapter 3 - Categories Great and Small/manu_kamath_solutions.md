# Manu's Solutions to 3.6 Challenges

## Q1: Generate free categories

![Screenshot 2021-02-14 at 16 38 17](https://user-images.githubusercontent.com/26002127/107889799-fd85ca00-6ee2-11eb-8516-1e09f2eb8009.png)

## Q2: Identify kind of order

(a) (Preorder and) **Partial Order**; if A c B, then preorder condition is satisfied. Note, however, if A c B and B c A, that implies A = B so partial order is satisfied. (Partial order is also a special case of preorder; *all preorders are partial orders, but not vice-versa*. [This SO explains it quite well](https://math.stackexchange.com/a/3847120))

(b) **Partial order**. The question describes object slicing where T1 represents the derived type and T2 the base type, which is exactly the same concepts as the first part.

## Q3: Bool set-theoretical monoids

`Bool = {True, False}`

```[hs]
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
```

### Monoid w.r.t AND

```[hs]
instance Monoid Bool where
    mempty = True -- neutral element: T && F = F, T && T = T. Also, it is commutative
    mappend a b = (&&) a b -- point-wise definition
```

### Monoid w.r.t OR

```[hs]
instance Monoid Bool where
    mempty = False -- commutative
    mappend a b = (||) a b
```

## Q4: Bool categorical monoid

The "global" object is the type itself (i.e. `Bool`).

The morphisms are A:= `&& False`, and B:= `&& True`. Composition rules are:

- `A . B => A`
- `A . A => A`
- `B . A => A`
- `B . B => B`

## Q5: (+) % 3 as a monoid category

Since we're mod-ing by 3, there will be 3 morphisms adding 0,1,2 (to yield those values for the entire operation):

- A: `(+ 0) % 3` [NOTE: this is also the neutral element]
- B: `(+ 1) % 3`
- C: `(+ 2) % 3`

Composition is as follows: (they are all commutative and associative)

- `A . * = *` (Here, `*` denotes either A, B, C since A is the identity morphism)
- `B . B = C`
- `C . B = A`
- `C . C = B`
