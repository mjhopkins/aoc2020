module Day5Spec where

import           Day5
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Day5"
  [ testParse, findSeat ]

testParse :: TestTree
testParse = testCase "parse boarding passes" $
  traverse decode passes @?= Just decoded
  where
    passes =
      [ "FBFBBFFRLR"
      , "BFFFBBFRRR"
      , "FFFBBBFRRR"
      , "BBFFBBFRLL"
      ]
    decoded =
      [ BoardingPass 44  5 357
      , BoardingPass 70  7 567
      , BoardingPass 14  7 119
      , BoardingPass 102 4 820
      ]

findSeat :: TestTree
findSeat = testCase "find vacant seat with an occupied seat to either side" $
  mySeatID @?= 583