-- Rank-N types just for redundancy, since normally we wouldn't export the Memoize constructor, but rather 
-- a function that have the necessary binds.
{-# LANGUAGE RankNTypes #-}
module Exercises where 

import Data.Map (Map)
import qualified Data.Map as Map 
import Control.Monad.State.Lazy (State, MonadState(put, get), runState, evalState)
import System.Random ( Random(randomIO) )
import Data.Void ( absurd, Void )

------------------------
-- Exercise 1
------------------------
{-
Define a higher-order function (or a function object) memoize in
your favorite language. This function takes a pure function f as
an argument and returns a function that behaves almost exactly
like f, except that it only calls the original function once for every
argument, stores the result internally, and subsequently returns
this stored result every time it’s called with the same argument.
You can tell the memoized function from the original by watching its performance. For instance, try to memoize a function that
takes a long time to evaluate. You’ll have to wait for the result
the first time you call it, but on subsequent calls, with the same
argument, you should get the result immediately.
-}

-- | We can think of a memoizable function as a pair: Function, memory.
data Memoize a b = Memoize 
    { fun :: Ord a => a -> b
    -- Since it would be nice to access memory in log(n) time
    -- we shall use a Map, nevertheless, note that Map needs an ordered type for
    -- keys, thus we will not be able to memoize all functions.
    , mem :: Ord a => Map a b
    }

-- | Creates a Memoize function.
memoizeFunction :: Ord a => (a -> b) -> Memoize a b
memoizeFunction f = Memoize {fun = f, mem = Map.empty}

-- | Given an input, yields a new state of the Memoized function
-- together with the result inside the state monad.
evalMemoize' :: Ord a => a -> State (Memoize a b) b
evalMemoize' x = do
    memoizedObject <- get
    case Map.lookup x $ mem memoizedObject of
        Just b  -> return b
        Nothing -> do
            let b' = fun memoizedObject x
            let mem' = Map.insert x b' $ mem memoizedObject
            put memoizedObject {mem = mem'}
            return b'

-- | Same as evalMemoize' but without wrapping the result in the State monad.
evalMemoize :: Ord a => a -> Memoize a b -> (b, Memoize a b)
evalMemoize x memo = (b', memo') where
    b' = case Map.lookup x $ mem memo of
            Just b  -> b
            Nothing -> fun memo x
    mem' = Map.insert x b' $ mem memo
    memo' = memo {mem = mem'}

------------------------
-- Exercise 2
------------------------
{-
Try to memoize a function from your standard library that you
normally use to produce random numbers. Does it work?
-}

-- | Since randomIO is not pure, it will always produce different outcomes.
memoizedRandomIO :: Random a => Memoize () (IO a)
memoizedRandomIO =  memoizeFunction (\() -> randomIO)

------------------------
-- Exercise 3
------------------------
{-
Most random number generators can be initialized with a seed.
Implement a function that takes a seed, calls the random number
generator with that seed, and returns the result. Memoize that
function. Does it work?
-}

-- Sadly, my implementation can't generate random numbers with a seed
-- since StdGen (or Randomgen g) does not hace an Eq nor an Ord instance :(.
--memoizedRandom :: Memoize StdGen (a,StdGen)
--memoizedRandom = memoizeFunction random

------------------------
-- Exercise 4
------------------------
{-
How many different functions are there from Bool to Bool? Can
you implement them all?

Since: 

Bool -> Bool ~ Bool ^ Bool (read ~ as bijective) 

And |Bool ^ Bool| = 2^2 = 4

We have 4 Functions:

-- | The identity
idBool :: Bool -> Bool
idBool x = x

-- | The constant function false
false :: Bool -> Bool
false _ = False

-- | The constant function true
true :: Bool -> Bool
true _ = True

-- | The negation:
not :: Bool -> Bool
not True = False
not False = True
-}

------------------------
-- Exercise 5
------------------------
{- I don't know C++, so no memoization there D: -}

------------------------
-- Exercise 6
------------------------
{-
Draw a picture of a category whose only objects are the types
Void, () (unit), and Bool; with arrows corresponding to all possible functions between these types. 
Label the arrows with the names of the functions.
-}

{-
There is exactly 1 arrow from Void to any type, and that is just an instanciation of
absurd :: Void -> a. Type annotations are just for making this instanciation explicit
but we could disregard them.
-}

fromVoidToBool :: Void -> Bool
fromVoidToBool = absurd :: Void -> Bool

fromVoidToUnit :: Void -> ()
fromVoidToUnit = absurd :: Void -> ()

-- The best way to make some sense of this beast
-- is remembering that since we are in a category, 
-- every object must have an identity arrow. Thus
-- we must have a function from Void to Void.
fromVoidToVoid :: Void -> Void
fromVoidToVoid = absurd :: Void -> Void -- could also be written as: id 

{- Then we have the arrows from the Unit type to Bool -}

unitToTrue :: () -> Bool
unitToTrue _ = True

unitToFalse :: () -> Bool
unitToFalse _ = False

{- We would also have the function from Bool to Unit-}

boolToUnit :: Bool -> ()
boolToUnit _ = ()

{- Finally, since there are no functions from a to Void, we must have arrows from each type to itself-}

idUnit :: () -> ()
idUnit = id :: () -> ()

idBool :: Bool -> Bool
idBool = id :: Bool -> Bool

true :: Bool -> Bool
true _ = True

false :: Bool -> Bool
false _ = False

not :: Bool -> Bool
not True  = False
not False = True

------------------------
-- Testing
------------------------

-- | An implementation of fibonnaci with exponential complexity.
badFib :: Int -> Int
badFib 0 = 0
badFib 1 = 1
badFib n = badFib (n-1) + badFib (n-2)

-- | Memoized version of the badFib function.
memoizedFib :: Memoize Int Int
memoizedFib = memoizeFunction badFib

-- | Pref run with: stack runghc, if loaded from the ghci results will be cached after the first run. 
-- But if you don't really want to install stack, remember to execute: 
-- :m -Initial_Mockingbird_solutions
main :: IO ()
main = do
    -- Testing on big fibonaccis.
    -- Complexity of underlying Fib is exponential.
    let xs = [30,20,30,30,31,20,31]
    putStrLn "Over the input:"
    print xs
    putStrLn "Notice the difference of times:"
    -- Forces evaluation and also prints the result, how convenient is that.
    print $ evalState (sequence $ evalMemoize' <$> xs) memoizedFib
    -- Number of random integers to be evaluated
    putStrLn "For 5 random numbers:"
    let n = 5
    let (y', _) = runState (sequence $ evalMemoize' <$> replicate n ()) memoizedRandomIO
    -- We must specify that we want integers
    y'' <- sequence y' :: IO [Int]
    -- Surprisingly it works, i mean, it throws DIFFERENT integers. I thought that since we always supply
    -- () it would always evaluate the same thing, but sorpresas te da la vida.
    print y''
    putStrLn "It yields 5 different integer results."
    return ()
