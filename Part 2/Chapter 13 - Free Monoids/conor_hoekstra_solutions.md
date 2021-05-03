### Question 1

Taken from: http://danshiebler.com/2018-11-10-category-solutions/

Say `f: A -> A'` is a monoid isomorphishm. Then there exists some `g: A' -> A` such that `g f a = a`. Given the unit `u` in `A, for all a' in A'`, we see `g (a' * f u) = g a' * g f u = g a' * u = g a'`. Since `g` is injective, this means that `a' = a' * f u`, so `f u` is the right unit for all `a' in A'`. We can do the same to show `f u` is the left unit as well.

### Question 2

**What is the image of the empty list `[]`?** 
1

**Is `[3]` mapped to 3**
Yes

**What is the image of `[1, 2, 3, 4]`?** 24

**How many different lists map to 12?**
```hs
concatMap permutations . filter ((==12) . product) $ subsequences [2..12] -- yields:
[[3,4],[4,3],[2,6],[6,2],[12]]
-- but this misses [2,2,3] and its permutations
-- Also, should 1 be included? if so:
[[3,4],[4,3],[1,3,4],[3,1,4],[4,3,1],[3,4,1],[4,1,3],[1,4,3],[2,6],[6,2],[1,2,6],[2,1,6],[6,2,1],[2,6,1],[6,1,2],[1,6,2],[12],[1,12],[12,1]]
```

### Question 3

Taken from: http://danshiebler.com/2018-11-10-category-solutions/

Solution This monoid is lists of unit `()` with concatenation. This is isomorphic to integers over addition.

```hs
forward :: List () -> Int
forward x = length x

inverse :: Int -> List ()
inverse x = replicate x [()]
```
