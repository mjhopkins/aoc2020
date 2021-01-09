{-# LANGUAGE OverloadedLists #-}

module Day7Spec where

import           Day7
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Day7"
  [ testParseRules
  , testContainedDirectly
  -- , shinyGoldContainedCount
  -- , shinyGoldContained
  , shinyGoldContainsCount
  , shinyGoldContainsCount'
  , shinyGoldContains
  ]

testContainedDirectly :: TestTree
testContainedDirectly = testCase "direct containment" $
  optionsContainingDirectly "shiny gold" sampleRules @?= ["bright white", "muted yellow"]

shinyGoldContainedCount :: TestTree
shinyGoldContainedCount = testCase
  "The number of bag colors that can eventually contain at least one shiny gold bag is 4" $
  numberOfOptionsContainingAtLeast1 "shiny gold" sampleRules @?= 4

{-

In the above rules, the following options would be available to you:

  * A bright white bag, which can hold your shiny gold bag directly.
  * A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
  * A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
  * A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.

-}

shinyGoldContained :: TestTree
shinyGoldContained = testCase
  "Possible bag colors that can eventually contain at least one shiny gold bag" $
  optionsContainingAtLeast1 "shiny gold" sampleRules @?=
    [ "bright white"
    , "muted yellow"
    , "dark orange"
    , "light red"
    ]

{-
Consider again your shiny gold bag and the rules from the above example:

"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."

Then:

* faded blue bags contain 0 other bags.
* dotted black bags contain 0 other bags.
* vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
* dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.

So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags
within it) plus 2 vibrant plum bags (and the 11 bags within each of those):
1 + 1*7 + 2 + 2*11 = 32 bags!
-}

shinyGoldContains :: TestTree
shinyGoldContains = testCase "Bags that must be contained within a shiny gold bag" $
  bagsContainedWithin "shiny gold" sampleRules @?=
    [ ("dark olive", 1)
    , ("vibrant plum", 2)
    , ("faded blue", 13)
    , ("dotted black", 16)
    ]

shinyGoldContainsCount :: TestTree
shinyGoldContainsCount = testCase "Bags that must be contained within a shiny gold bag (i)" $
  countOfBagsContainedWithin "shiny gold" sampleRules @?= 32

shinyGoldContainsCount' :: TestTree
shinyGoldContainsCount' = testCase "Bags that must be contained within a shiny gold bag (ii)" $
  countOfBagsContainedWithin "shiny gold" sampleRules' @?= 126

testParseRules :: TestTree
testParseRules = testCase "Parse rules" $
  parseRules sample @?=
    [ Rule "light red"    [("bright white", 1), ("muted yellow", 2)]
    , Rule "dark orange"  [("bright white", 3), ("muted yellow", 4)]
    , Rule "bright white" [("shiny gold", 1)]
    , Rule "muted yellow" [("shiny gold", 2), ("faded blue", 9)]
    , Rule "shiny gold"   [("dark olive", 1), ("vibrant plum", 2)]
    , Rule "dark olive"   [("faded blue", 3), ("dotted black", 4)]
    , Rule "vibrant plum" [("faded blue", 5), ("dotted black", 6)]
    , Rule "faded blue"   []
    , Rule "dotted black" []
    ]

sampleRules :: [Rule]
sampleRules = parseRules sample

sample :: String
sample = unlines
  [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
  , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  , "bright white bags contain 1 shiny gold bag."
  , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  , "faded blue bags contain no other bags."
  , "dotted black bags contain no other bags."
  ]

sampleRules' :: [Rule]
sampleRules' = parseRules sample'

sample' :: String
sample' = unlines
  [ "shiny gold bags contain 2 dark red bags."
  , "dark red bags contain 2 dark orange bags."
  , "dark orange bags contain 2 dark yellow bags."
  , "dark yellow bags contain 2 dark green bags."
  , "dark green bags contain 2 dark blue bags."
  , "dark blue bags contain 2 dark violet bags."
  , "dark violet bags contain no other bags."
  ]
