module Day8Spec where

import           Day8
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Day8"
  [ testParse
  , testAccValueOnLoop
  , testAccValueAfterFix
  ]

testAccValueAfterFix :: TestTree
testAccValueAfterFix = testCase "accumulator value on exit when erroneous instruction repaired" $
  accValueAfterFix sampleInstructions @?= 8

testAccValueOnLoop :: TestTree
testAccValueOnLoop = testCase "accumulator value when loop detected" $
  accValueOnLoop sampleInstructions @?= Just 5

testParse :: TestTree
testParse = testCase "parse" $
  sampleInstructions @?=
    [ Instruction Nop 0
    , Instruction Acc 1
    , Instruction Jmp 4
    , Instruction Acc 3
    , Instruction Jmp (-3)
    , Instruction Acc (-99)
    , Instruction Acc 1
    , Instruction Jmp (-4)
    , Instruction Acc 6
    ]

sampleInstructions :: [Instruction]
sampleInstructions = parse sample

sample :: String
sample = unlines
  [ "nop +0"
  , "acc +1"
  , "jmp +4"
  , "acc +3"
  , "jmp -3"
  , "acc -99"
  , "acc +1"
  , "jmp -4"
  , "acc +6"
  ]