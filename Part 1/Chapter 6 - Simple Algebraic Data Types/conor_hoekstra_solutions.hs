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
