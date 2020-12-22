module Main where

import qualified Day1Spec
import qualified Day2Spec
import qualified Day3Spec
import qualified Day4Spec
import qualified Day5Spec
import qualified Day6Spec
import qualified Day7Spec
import qualified Day8Spec
import qualified Day9Spec
import qualified Day10Spec
import qualified Day11Spec
import qualified Day12Spec
import qualified Day13Spec
import qualified Day14Spec
import qualified Day15Spec
import qualified Day16Spec
import qualified Day17Spec
import qualified Day18Spec
import qualified Day19Spec
import qualified Day20Spec
import qualified Day21Spec
import qualified Day22Spec
import qualified Day23Spec
import qualified Day24Spec
import qualified Day25Spec
import qualified Day26Spec
import qualified Day27Spec
import qualified Day28Spec
import qualified Day29Spec
import qualified Day30Spec
import qualified Day31Spec

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Unit Tests" unitTests ]

unitTests :: [TestTree]   
unitTests =   
  [ Day1Spec.unitTests
  , Day2Spec.unitTests
  , Day3Spec.unitTests
  , Day4Spec.unitTests
  , Day5Spec.unitTests
  , Day6Spec.unitTests
  , Day7Spec.unitTests
  , Day8Spec.unitTests
  , Day9Spec.unitTests
  , Day10Spec.unitTests
  , Day11Spec.unitTests
  , Day12Spec.unitTests
  , Day13Spec.unitTests
  , Day14Spec.unitTests
  , Day15Spec.unitTests
  , Day16Spec.unitTests
  , Day17Spec.unitTests
  , Day18Spec.unitTests
  , Day19Spec.unitTests
  , Day20Spec.unitTests
  , Day21Spec.unitTests
  , Day22Spec.unitTests
  , Day23Spec.unitTests
  , Day24Spec.unitTests
  , Day25Spec.unitTests
  , Day26Spec.unitTests
  , Day27Spec.unitTests
  , Day28Spec.unitTests
  , Day29Spec.unitTests
  , Day30Spec.unitTests
  , Day31Spec.unitTests
  ]