### Question 2

Similar to the same reason that the List functor is not representable, going from `Maybe x` to `(a -> x)`, when the `Maybe` contains `Nothing` is not possible for any `x`.

### Question 3

Yes (the `Reader` functor is isomorphic to itself).

### Question 4 & 5

See: http://danshiebler.com/2018-11-10-category-solutions/

### Question 6

`Boolean` can be used.

```hs
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

import Data.Kind (Type)

data Pair a = Pair { getL, getR :: a }
    deriving (Show, Eq, Functor)

class Functor f => Representable f where
  type Rep f :: Type
  index :: f a -> Rep f -> a
  tabulate :: (Rep f -> a) -> f a

  -- Must be isomorphic to ((->) (Rep f))
  --   tabulate . index = id
  --   index . tabulate = id

instance Representable Pair where
  type Rep Pair = Bool
  index p False = getL p
  index p True = getR p
  tabulate f = Pair (f False) (f True)

main :: IO ()
main = do
    print $ index (Pair 1 2) False -- 1
    print $ index (Pair 1 2) True  -- 2
    -- print $ tabuldate ???
```
