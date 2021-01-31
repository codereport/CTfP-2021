main = do
  print testComposition

-- Helpers
assertEqual :: Eq a => a -> a -> Bool
assertEqual x y = if x == y then True else error "Not equal!"

-- Q1: Implemet identity(not in haskell) - done in python.
-- def identity(elem):
--   return elem  


-- Q2: Implement composition
gAfterF :: (b->c)->(a->b)->a->c
gAfterF g f = \x -> g (f x)

-- Q3: Test that composition function respects identity
testComposition = [assertEqual (gAfterF id id $ 5) 5,
                   assertEqual (gAfterF id (+3) $ 5) 8,
                   assertEqual (gAfterF (+3) id $ 5) 8]

{-|
Q4: Is the world wide web a category in any sense? Are links morphisms?
A: They don't fulfill the composition requirement of a category in my opinion i.e. if website A -> website B -> website C where '->' signifies links to, it isnt necessary that a direct link from A -> C exists. Additionally, websites dont always link to themselves thus not meeting the identity requirement either.

Q5: Is Facebook a category, with people as objects and friends as morphisms?
A: No, for the same reasons as Q4.

Q6: When is a directed graph a category?
A: (1) When every parent node links to every child node directly and (2) Every node links to itself.
-}
