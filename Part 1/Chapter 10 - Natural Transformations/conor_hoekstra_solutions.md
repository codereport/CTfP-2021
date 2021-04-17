### Question 1

```hs
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []
```

### Question 2

```hs
readerToList :: Reader () a -> [a]
readerToList (Reader _) = []

readerToList2 :: Reader () a -> [a]
readerToList2 (Reader g) = [g ()] 
```

Infinite # of lists = infinite # of natural transformations.

### Question 3 & 6

See [here](https://github.com/awalterschulze/category-theory-for-programmers-challenges/blob/master/110-Natural-Transformations.md#103-continue-the-previous-exercise-with-reader-bool-and-maybe)
