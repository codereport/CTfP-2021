{-# LANGUAGE ViewPatterns #-}
-- Easier functions unwrapping when testing.
-- For testing laws:
-- https://www.austinrochford.com/posts/2014-05-27-quickcheck-laws.html

module Chapter4 where
import Test.QuickCheck ( quickCheck,  Fun )
import Test.QuickCheck.Function ( Fun, apply )
import Prelude hiding (return)
--data Optional a = Nada | Algo a deriving (Eq,Show)

type Optional a = Maybe a

-- | Identity
return :: a -> Optional a
return = Just

-- | Forward Composition
(>=>) :: (a -> Optional b) -> (b -> Optional c) -> (a -> Optional c)
(>=>) f g = 
    \x ->
        let mb = f x  
        in case mb of
            Just c -> g c
            Nothing   -> Nothing

safeRoot :: Double -> Optional Double
safeRoot x 
    | x >= 0     = Just $ sqrt x
    | otherwise = Nothing

safeReciprocal ::  Double -> Optional Double
safeReciprocal 0 = Nothing
safeReciprocal n = Just (1/n)

composedRootReciprocal :: Double -> Optional Double
composedRootReciprocal = safeReciprocal >=> safeRoot


prop_KleisiLeftId :: Eq b => a -> Fun a (Optional b) -> Bool
prop_KleisiLeftId x (apply -> g) = (return >=> g) x == g x 

prop_KleisiRightId :: Eq b => a -> Fun a (Optional b) -> Bool
prop_KleisiRightId x (apply -> g) = (g >=> return) x == g x

prop_KleisiComposition :: Eq d => a -> Fun a (Optional b) -> Fun b (Optional c) -> Fun c (Optional d) -> Bool 
prop_KleisiComposition x (apply -> f) (apply -> g) (apply -> h) = ((f >=> g) >=> h) x == (f >=> ( g >=> h)) x

main :: IO ()
main = do
    putStrLn "Checking left identity:"
    quickCheck (prop_KleisiLeftId :: Double -> Fun Double (Optional String) -> Bool)
    quickCheck (prop_KleisiLeftId :: String -> Fun String (Optional String) -> Bool)
    putStrLn "Checking right identity:"
    quickCheck (prop_KleisiRightId :: Double -> Fun Double (Optional String) -> Bool)
    quickCheck (prop_KleisiRightId :: String -> Fun String (Optional String) -> Bool)
    putStrLn "Checking Composition:"
    quickCheck (prop_KleisiComposition :: Double -> Fun Double (Optional Double) -> Fun Double (Optional Double) -> Fun Double (Optional Double) -> Bool )
    quickCheck (prop_KleisiComposition :: Int -> Fun Int (Optional String) -> Fun String (Optional Int) -> Fun Int (Optional Int) -> Bool )

    