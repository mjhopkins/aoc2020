module Main where

import qualified Day1Spec
import qualified Day2Spec
import qualified Day3Spec

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Unit Tests" unitTests ]

unitTests :: [TestTree]   
unitTests =   
  [ Day1Spec.unitTests
  , Day2Spec.unitTests
  , Day3Spec.unitTests
  ]