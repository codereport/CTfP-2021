-- Haskell portion of problem 4. See also the folder java-solutions.

data Shape = Circle Float
           | Rect Float Float
           | Square Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square s) = s * s

circ :: Shape -> Float
circ (Circle r) = 2 * pi * r
circ (Rect d h) = 2 * (d + h)
circ (Square s) = 4 * s
