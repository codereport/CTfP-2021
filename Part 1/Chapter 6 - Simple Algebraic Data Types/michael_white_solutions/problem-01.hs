f :: Maybe a -> Either () a
f (Just x) = Right x
f Nothing = Left ()

f' :: Either () a -> Maybe a
f' (Right x) = Just x
f' (Left ()) = Nothing

{-
  Which gives us:

  (f . f') (Right x) == Right x
  (f . f') (Left ()) == Left ()

  (f' . f) (Just x) == Just x
  (f' . f) Nothing == Nothing
-}
