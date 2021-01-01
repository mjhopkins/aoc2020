module Day6Spec where

import           Control.DeepSeq   (NFData, force)
import           Control.Exception (ErrorCall (ErrorCall), Exception, evaluate,
                                    handle)
import qualified Data.Set          as Set
import           Day6              (parseAllGroups, parseAnyGroups, sumOfAllCounts,
                                    sumOfCounts)
import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (Assertion, assertFailure, testCase, (@?=))

unitTests :: TestTree
unitTests = testGroup "Day6"
  [ testGroup "any"
    [ testParseAnyGroups
    , testSumOfAnyCounts
    , mustBeLowercaseAlphabeticAny
    ]
  , testGroup "all"
    [ testParseAllGroups
    , testSumOfAllCounts
    , mustBeLowercaseAlphabeticAll
    ]
  ]

mustBeLowercaseAlphabeticAny :: TestTree
mustBeLowercaseAlphabeticAny = testGroup "must be lowercase alpha"
  [ shouldFail "digit"            "a1bc"
  , shouldFail "space"            "a bc"
  , shouldFail "tab"              "a\tbc"
  , shouldFail "carriage return"  "a\rbc"
  , shouldFail "capital"          "aBc"
  ]
  where shouldFail s = testCase s . assertError' "invalid char" . parseAnyGroups

mustBeLowercaseAlphabeticAll :: TestTree
mustBeLowercaseAlphabeticAll = testGroup "must be lowercase alpha"
  [ shouldFail "digit"            "a1bc"
  , shouldFail "space"            "a bc"
  , shouldFail "tab"              "a\tbc"
  , shouldFail "carriage return"  "a\rbc"
  , shouldFail "capital"          "aBc"
  ]
  where shouldFail s = testCase s . assertError' "invalid char" . parseAllGroups

testSumOfAnyCounts :: TestTree
testSumOfAnyCounts = testCase "sum of counts" $
  sumOfCounts sample @?= 11

testSumOfAllCounts :: TestTree
testSumOfAllCounts = testCase "sum of counts" $
  sumOfAllCounts sample @?= 6

testParseAnyGroups :: TestTree
testParseAnyGroups = testCase "parse groups" $
  parseAnyGroups sample @?=
    [ Set.fromList "abc"
    , Set.fromList "abc"
    , Set.fromList "abc"
    , Set.fromList "a"
    , Set.fromList "b"
    ]

testParseAllGroups :: TestTree
testParseAllGroups = testCase "parse groups" $
  parseAllGroups sample @?=
    [ Set.fromList "abc"
    , Set.fromList ""
    , Set.fromList "a"
    , Set.fromList "a"
    , Set.fromList "b"
    ]

sample :: String
sample = unlines
  [ "abc"
  , ""
  , "a"
  , "b"
  , "c"
  , ""
  , "ab"
  , "ac"
  , ""
  , "a"
  , "a"
  , "a"
  , "a"
  , ""
  , "b"
  ]

-- assertException :: (Exception e, Eq e, Show a) => e -> IO a -> IO ()
-- assertException ex action =
--     handleJust isWanted (const $ return ()) $ do
--         res <- action
--         assertFailure $ "Expected exception: " ++ show ex ++ "\nbut got: " ++ show res
--   where isWanted = guard . (== ex)

-- assertError :: (Show a, NFData a) => String -> a -> Assertion
-- assertError ex f = assertException (ErrorCall ex) $ evaluate (force f)

assertError' :: (Show a, NFData a) => String -> a -> Assertion
assertError' ex f
  = handle (\(ErrorCall msg) -> h msg) $ do
      res <- evaluate (force f)
      assertFailure ("Expected an error: " ++ ex ++ "\nbut got:" ++ show res)
  where
    h msg = if msg == ex then return () else assertFailure $ "Expected exception message: " ++ ex ++ "\nbut got: " ++ msg

assertException :: (Show a, NFData a, Exception e, Eq e) => e -> a -> Assertion
assertException ex f
  = handle h $ do
  -- = handleJust (guard . (== ex)) (const $ return ()) $ do
      res <- evaluate (force f)
      assertFailure ("Expected an exception: " ++ show ex ++ "\nbut got:" ++ show res)
  where
    h e = if e == ex then return () else assertFailure $ "Expected exception: " ++ show ex ++ "\nbut got: " ++ show e

assertError :: (Show a, NFData a) => String -> a -> Assertion
assertError msg = assertException (ErrorCall msg)
