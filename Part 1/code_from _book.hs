
-- Chapter 1: Category

Prelude> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

-- Not from book
map (1+) . reverse

-- Chapter 2: Types & Functions

fact n = product [1 .. n]
