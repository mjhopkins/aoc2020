{-# LANGUAGE ViewPatterns #-}

module Day5 where

import           Data.Foldable
import           Data.Maybe
import           System.IO.Unsafe

data BoardingPass
  = BoardingPass
  { bpRow :: Int
  , bpCol :: Int
  , bpId  :: Int
  }
  deriving (Eq, Show)

data SeatData = SeatData Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool

toBoardingPass :: SeatData -> BoardingPass
toBoardingPass (SeatData r0 r1 r2 r3 r4 r5 r6 c0 c1 c2)
  = let
      row = crunch [r0,r1,r2,r3,r4,r5,r6] 0 127
      col = crunch [c0,c1,c2] 0 7
      seat = row * 8 + col
    in BoardingPass row col seat

crunch :: [Bool] -> Int -> Int -> Int
crunch bs lo hi
  = (\(i,j) -> if i == j then i else error "Didn't reduce to a single value")
  $ foldl' (\(lo, hi) b -> split b lo hi) (lo, hi) bs

crunch' :: (Foldable t, Integral a, Show a) => t Bool -> a -> a -> (a, a)
crunch' bs lo hi = foldl' (\(lo, hi) b -> split b lo hi) (lo, hi) bs

split :: (Integral a, Show a) => Bool -> a -> a -> (a, a)
split b lo hi =  if b then (half + 1, hi) else (lo, half)
  where
    diff = hi - lo + 1
    half = case divMod diff 2 of
          (a,0) -> lo + a - 1
          _     -> error $ "Interval " ++ show lo ++ ", " ++ show hi ++ " not even"

parse :: String -> Maybe SeatData
parse s = case s of
  [  fb -> Just b0
   , fb -> Just b1
   , fb -> Just b2
   , fb -> Just b3
   , fb -> Just b4
   , fb -> Just b5
   , fb -> Just b6
   , lr -> Just b7
   , lr -> Just b8
   , lr -> Just b9
   ] -> Just $ SeatData b0 b1 b2 b3 b4 b5 b6 b7 b8 b9
  _ -> Nothing
 where
   fb 'F' = Just False
   fb 'B' = Just True
   fb _   = Nothing

   lr 'L' = Just False
   lr 'R' = Just True
   lr _   = Nothing

decode :: String -> Maybe BoardingPass
decode = fmap toBoardingPass . parse

{-# NOINLINE input #-}
input :: [String]
input = lines . unsafePerformIO . readFile $ "data/day5.txt"

inputData :: [BoardingPass]
inputData = fromMaybe (error "Bad data") $ traverse decode input

highestSeatID :: Int
highestSeatID = maximum seatIDs

seatIDs :: [Int]
seatIDs = map bpId inputData

occupancies :: [Bool]
occupancies = map (`elem` seatIDs) [0..1024]

occupancies' :: [(Int, Bool)]
occupancies' = map (\i -> (i,i `elem` seatIDs)) [0..1024]

-- mySeatId = 583
mySeatID :: Int
mySeatID = head mySeatMatches

mySeatPattern :: [Bool]
mySeatPattern = [True, False, True]

match :: Eq a => [Maybe a] -> [Maybe a] -> Bool
match as bs = and $ zipWith (=?=) as bs

(=?=) :: Eq a => Maybe a -> Maybe a -> Bool
(=?=) (Just a) (Just b) = a == b
(=?=) _ _               = True

boost :: Int -> [a] -> [Maybe a]
boost n as = replicate n Nothing ++ map Just as

matchAt :: Eq a => Int -> [a] -> [a] -> Bool
matchAt n as bs = match (boost n as) (map Just bs)

matches :: Eq a => [a] -> [a] -> [Int]
matches as bs = mapMaybe (\i -> if matchAt i as bs then Just i else Nothing) [0..]

mySeatMatches :: [Int]
mySeatMatches = map (+1) $ matches mySeatPattern occupancies
