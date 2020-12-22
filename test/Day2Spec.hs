module Day2Spec where

import           Day2
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Day2"
  [ day2, day2', day2'' ]

day2 :: TestTree
day2 = testCase "Count valid lines" $
  countValidLines sample @?= 2

day2' :: TestTree
day2' = testCase "Second interpretation" $
  map f (lines sample) @?= [True, False, False]
  where
    f = uncurry validate' . parseLine

day2'' = testCase "Count with second interpretation" $
  countValidLines' sample @=? 1

sample :: String
sample = unlines
  [ "1-3 a: abcde"
  , "1-3 b: cdefg"
  , "2-9 c: ccccccccc"
  ]

