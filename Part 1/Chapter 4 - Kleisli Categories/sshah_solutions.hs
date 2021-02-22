import Test.HUnit

-- Given
safeRoot :: Double -> Maybe Double
safeRoot x 
  | x < 0     = Nothing
  | otherwise = Just (sqrt x)

-- Q1: Identity and composition
partialFnId :: a -> Maybe a
partialFnId x = Just x

partialFnCompose :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
partialFnCompose g f = \x -> case (f x) of
                          Just value -> g value
                          Nothing -> Nothing

-- Testing Q1
testPartialFnMeetsCategoryRequirements1 = TestCase $ assertEqual "Id should pass through inputs" (Just 3) (partialFnId 3)
testPartialFnMeetsCategoryRequirements2 = TestCase $ assertEqual "Composition with identity is noop(1)" (Just 2.0) ((partialFnCompose partialFnId safeRoot) 4)
testPartialFnMeetsCategoryRequirements3 = TestCase $ assertEqual "Composition with identity is noop(2)" (Just 2.0) ((partialFnCompose safeRoot partialFnId) 4)

-- Q2: Implement safeReciprocal
safeReciprocal :: Double -> Maybe Double
safeReciprocal x
  | x == 0    = Nothing
  | otherwise = Just (1 / x)

-- Q3: Implement safeRootReciprocal via composing the above 2.
safeRootReciprocal :: Double -> Maybe Double
safeRootReciprocal = partialFnCompose safeRoot safeReciprocal

-- Testing Q3
testSafeRootReciprocal1 = TestCase $ assertEqual "Provides root of reciprocal for valid inputs" (Just 2.0) (safeRootReciprocal 0.25)
testSafeRootReciprocal2 = TestCase $ assertEqual "Provides Nothing on invalid inputs(1)" Nothing (safeRootReciprocal 0.0)
testSafeRootReciprocal3 = TestCase $ assertEqual "Provides Nothing on invalid inputs(2)" Nothing (safeRootReciprocal (-0.25))

main = do
  runTestTT $ TestList [testPartialFnMeetsCategoryRequirements1,
                        testPartialFnMeetsCategoryRequirements2,
                        testPartialFnMeetsCategoryRequirements3,
                        testSafeRootReciprocal1,
                        testSafeRootReciprocal2,
                        testSafeRootReciprocal3]
