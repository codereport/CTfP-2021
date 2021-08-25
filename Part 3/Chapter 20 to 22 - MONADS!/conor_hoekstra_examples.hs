import Control.Monad
import Data.Maybe

-- not generic to avoid error
safeHead :: [Int] -> Maybe Int
safeHead []    = Nothing
safeHead (x:_) = Just x

safeDouble :: Int -> Maybe Int
safeDouble x = Just (2 * x)

double :: Int -> Int
double = (2*)

main :: IO ()
main = do
    print $ safeHead [42,1729,343]
    print $ safeHead []
    print $ (safeHead >=> safeDouble) [42,1729,343]
    print $ fmap double $ safeHead [42,1729,343]
    print $ (fmap double . safeHead) [42,1729,343]
    print $ double <$> (safeHead [42,1729,343])
    print $ (<$>) double (safeHead [42,1729,343])
    print $ (safeHead >=> (return . double)) [42,1729,343]
    print $ (Just [42,1729,343]) >>= safeHead >>= safeDouble
    print $ (safeHead [42,1729,343]) >>= safeDouble
