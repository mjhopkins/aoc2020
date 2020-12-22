module Day3Spec where

import           Day3
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Day3"
  [ countTreesTest1
  , countTreesTest2
  , countTreesTest3
  , countTreesTest4
  , countTreesTest5
  , comboTest
  , invalidChar
  , invalidLen
  ]

countTreesTest1 :: TestTree
countTreesTest1 = testCase "slope 1 1" $
 countTrees sample 1 1 @?= 2

countTreesTest2 :: TestTree
countTreesTest2 = testCase "slope 3 1" $
 countTrees sample 3 1 @?= 7

countTreesTest3 :: TestTree
countTreesTest3 = testCase "slope 5 1" $
 countTrees sample 5 1 @?= 3

countTreesTest4 :: TestTree
countTreesTest4 = testCase "slope 7 1" $
 countTrees sample 7 1 @?= 4

countTreesTest5 :: TestTree
countTreesTest5 = testCase "slope 1 2" $
 countTrees sample 1 2 @?= 2

comboTest :: TestTree
comboTest = testCase "combo" $
  combo sample @?= 336

sample :: Grid
sample
  = loadStringUnsafe s
  where
    s = unlines
      [ "..##......."
      , "#...#...#.."
      , ".#....#..#."
      , "..#.#...#.#"
      , ".#...##..#."
      , "..#.##....."
      , ".#.#.#....#"
      , ".#........#"
      , "#.##...#..."
      , "#...##....#"
      , ".#..#...#.#"
      ]

invalidChar :: TestTree
invalidChar
  = testCase "invalid char" $ loadString s
  @?= Left "Problem with line 2: Invalid char ' ' at position 11"
  where
    s = unlines
      [ "..##......."
      , "#...#...#.."
      , ".#....#..#. "
      ]

invalidLen :: TestTree
invalidLen
  = testCase "invalid length" $ loadString s
  @?= Left "Problem with line 1: Expected 11 chars, but got 12"
  where
    s = unlines
      [ "..##......."
      , "#...#...#..#"
      , ".#....#..#."
      ]
