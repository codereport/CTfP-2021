-- Question 1

maybe_to_either :: Maybe a -> Either () a
maybe_to_either Nothing = Left ()
maybe_to_either (Just x) = Right x

either_to_maybe :: Either () a -> Maybe a
either_to_maybe (Left ()) = Nothing
either_to_maybe (Right a) = Just a



--  Tried to do it with case analysis, but didn't load in ghci?

-- maybe_to_either :: Maybe a -> Either () a
-- 	case x of 
-- 		Nothing -> Left ()
-- 		Just a -> Right a

-- either_to_maybe :: Either () a -> Maybe a
-- either_to_maybe = x
--     case x of 
--         Left () -> Nothing
--         Right a -> Just a


-- Questions 4. Add new shape to Hierarchy

data Shape = Circle Float
            | Rect Float Float
            | Square Float  -- new

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square s) = s * s     -- new

circ :: Shape -> Float
circ (Circle r) = 2.0 * pi * r
circ (Rect d h) = 2.0 * (d + h)
circ (Square s) = 4.0 * s   -- new


-- Question 5.

either_to_pair :: Either a a -> (a, Bool)
either_to_pair (Left a)  = (a, True)
either_to_pair (Right a) = (a, False)

pair_to_either :: (Bool, a) -> Either a a
pair_to_either (a, True) = Left a
pair_to_either (a, False)  = Right a

