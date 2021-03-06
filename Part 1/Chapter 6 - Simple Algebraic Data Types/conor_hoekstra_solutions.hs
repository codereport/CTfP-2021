-- Question 1

maybeToEither :: Maybe a -> Either () a
maybeToEither (Just x) = Right x
maybeToEither Nothing  = Left ()

eitherToMaybe :: Either () a -> Maybe a
eitherToMaybe (Left ()) = Nothing
eitherToMaybe (Right x) = Just x

main :: IO ()
main = do
    print $ maybeToEither (Just 42)    -- Right 42
    print $ eitherToMaybe (Right 1729) -- Just 1729

-- Question 5

eitherToPair :: Either a a -> (Bool, a)
eitherToPair (Left a)  = (False, a)
eitherToPair (Right a) = (True, a)

pairToEither :: (Bool, a) -> Either a a
pairToEither (False, a) = Left a
pairToEither (True, a)  = Right a

main :: IO ()
main = do
    print $ eitherToPair (Left 42)    -- (False, 42)
    print $ pairToEither (True, 1729) -- Right 1729
