{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, GADTs #-}
module Ctfp where

import GHC.TypeLits
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- Ex 3 AND and OR monoids

newtype AndBool = AndBool Bool deriving(Show, Eq)

instance Semigroup AndBool where
  (AndBool a) <> (AndBool b)  = AndBool (a && b)

instance Monoid AndBool where mempty = AndBool True

newtype OrBool = OrBool Bool deriving(Show, Eq)

instance Semigroup OrBool where
  (OrBool a) <> (OrBool b)  = OrBool (a || b)

instance Monoid OrBool where mempty = OrBool False

-- Ex 5 "Typed" monoid
data Mo (n :: Nat) (m :: Nat) where
  Elem :: (CmpNat m n ~ LT) => Mo n m

comp :: (CmpNat ((Mod (a + b) n)) n ~ LT) => Mo n a -> Mo n b -> Mo n (Mod (a + b) n)
comp _ _ = Elem

y :: Mo 3 2 -- Mo 3 1 does not compile
y = comp (Elem :: Mo 3 1) (Elem :: Mo 3 1)

-- TESTING --  
instance Arbitrary AndBool where
  arbitrary = arbitrary >>= \b -> return (AndBool b)

instance EqProp AndBool where (=-=) = eq
  
instance Arbitrary OrBool where
  arbitrary = arbitrary >>= \b -> return (OrBool b)

instance EqProp OrBool where (=-=) = eq

test = do
  quickBatch (monoid (undefined :: AndBool))
  quickBatch (monoid (undefined :: OrBool))
