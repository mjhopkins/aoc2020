module Day1Spec where

import Test.Tasty
import Test.Tasty.HUnit
import Day1

unitTests :: TestTree
unitTests = testGroup "Day1" 
  [ day1
  , day1' 
  ]  

sample :: [Int] 
sample = 
  [ 1721
  , 979
  , 366
  , 299
  , 675
  , 1456
  ]

day1 :: TestTree
day1 = testCase "First result" $ 
  firstSolution sample @?= 514579

day1' = testCase "All results" $ 
  sum3 sample @?= 241861950