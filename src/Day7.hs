{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Day7 where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.State
import           Data.Foldable
import           Data.List           (group)
import           Data.List.Extra     (split, splitOn, wordsBy)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Debug.Trace
import           System.IO.Unsafe
import           Text.Regex.TDFA

-------------------------------------------------------------------------------
-- Sample data
-------------------------------------------------------------------------------

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO . readFile $ "data/day7.txt"

inputRules :: [Rule]
inputRules = parseRules input

data Rule = Rule { ruleContainer :: String, ruleContents :: [(String, Int)] }
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Unique values
-------------------------------------------------------------------------------

uniqueColours :: [Rule] -> Set String
uniqueColours = Set.fromList . (=<<) (\(Rule col cols) -> col : map fst cols)

uniqueAdjectives :: [Rule] -> Set String
uniqueAdjectives = Set.map (fst . splitColour) . uniqueColours

uniqueBaseColours :: [Rule] -> Set String
uniqueBaseColours = Set.map (snd . splitColour) . uniqueColours

splitColour :: String -> (String, String)
splitColour s = case words s of
  [a, b] -> (a, b)
  _      -> error $ "Bad colour: " ++ s

-------------------------------------------------------------------------------
-- Containment relations
-------------------------------------------------------------------------------

optionsContainingAtLeast1 :: String -> [Rule] -> Set String
optionsContainingAtLeast1 colour rules =
  Set.fromList . flip execState [] . recurse $ colour

  where
    go :: String -> [String] -> [String]
    go col seen = filter (`notElem` seen) $ f col

    findNewCols :: String -> State [String] [String]
    findNewCols col = do
      let cols = f col
      seen <- get
      let new = filter (`notElem` seen) cols
      put $ seen ++ new
      return new

    recurse :: String -> State [String] ()
    recurse col = do
      new <- findNewCols col
      traverse_ recurse new

    -- zz start = mdo
    --   x <- trav

    allCols :: [String]
    allCols = []
    -- allCols = f colour ++ [ cols | col <- allCols, cols <- f col ]

    f :: String -> [String]
    f = flip optionsContainingDirectly rules
      -- let res = traceShow c $ optionsContainingDirectly c rules
      -- in trace ("res=" ++ show res) res

-- optionsContainingAtLeast1 :: String -> [Rule] -> Set String
-- optionsContainingAtLeast1 colour rules =
--   let dir = f colour
--   in Set.fromList $ dir ++ join (map f dir)
--   where
--     f c =
--       let res = traceShow c $ optionsContainingDirectly c rules
--       in trace ("res=" ++ show res) res
--     -- f = flip optionsContainingDirectly rules


numberOfOptionsContainingAtLeast1 :: String -> [Rule] -> Int
numberOfOptionsContainingAtLeast1 colour rules = length $ optionsContainingAtLeast1 colour rules

optionsContainingDirectly :: String -> [Rule] -> [String]
optionsContainingDirectly colour = map ruleContainer . filter ((colour `elem`) . map fst . ruleContents)

bagsContainedWithin :: String -> [Rule] -> Set (String, Int)
bagsContainedWithin col rules = trace "\nSTART" $ Set.fromList . counts $ go [col] []
  where
    directlyContained :: String -> [String]
    directlyContained = join . map (\(s,i) -> replicate i s) . ruleContents . getRuleFor

    go :: [String] -> [String] -> [String]
    go (col:cols) acc =
      let new = directlyContained col
      in
          -- trace ("looking at: " ++ col ++ "  " ++ show new ++ "\nacc: " ++ show (counts (new ++ acc)) ++ "\n")
          -- trace ("looking at: " ++ col ++ "  " ++ show new ++ "\nacc: " ++ show (counts acc) ++ "\nNext: " ++ show (new ++ cols) ++ "\n")
          id
        $ go (new ++ cols) (new ++ acc)

    go [] acc        = acc

    recurse :: String -> State [String] ()
    recurse = undefined

    getRuleFor :: String -> Rule
    getRuleFor col = fromMaybe (error $ "No rule for " ++ col) $ Map.lookup col rulesByCol

    rulesByCol :: Map String Rule
    rulesByCol
      = foldl1 (Map.unionWithKey (\k r1 r2 -> error ("Multiple rules for " ++ k ++ ":\n" ++ show r1 ++ "\n" ++ show r2)))
      . map (\r -> Map.singleton (ruleContainer r) r)
      $ rules
    -- rulesByCol = Map.fromList . map (\r -> (ruleContainer r, r)) $ rules

countOfBagsContainedWithin :: String -> [Rule] -> Int
countOfBagsContainedWithin col rules = sum . map snd . Set.toList $ bagsContainedWithin col rules

counts :: Ord a => [a] -> [(a, Int)]
counts = Map.toList . Map.unionsWith (+) . map (uncurry Map.singleton . (,1))

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

parseRules :: String -> [Rule]
parseRules = map parseRule . lines

parseRule :: String -> Rule
parseRule s
  = case getAllTextSubmatches (s =~ "\\`([a-z]+ [a-z]+) bags contain ([a-z0-9, ]+).\\'") of
      [_, s, s'] -> Rule s $ parseContained s'
      other      -> error $ "Bad line: " ++ show other

parseContained :: String -> [(String, Int)]
parseContained "no other bags" = []
parseContained s = case splitOn ", " s of
  ss -> map parseNumbered ss

parseNumbered :: String -> (String, Int)
parseNumbered s
  = fromMaybe (error $ "Bad line: " ++ s)
  $ match "(1) ([a-z]+ [a-z]+) bag" <|> match "([1-9][0-9]*) ([a-z]+ [a-z]+) bags"
    where
      match :: String -> Maybe (String, Int)
      match r = case getAllTextSubmatches (s =~ ("\\`" ++ r ++ "\\'")) of
        []        -> Nothing
        [_, n, s] -> Just (s, read n)
        other     -> error $ "bad line: " ++ show other

