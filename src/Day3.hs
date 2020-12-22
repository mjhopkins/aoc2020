module Day3 where

import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor      (Bifunctor (first))
import           Data.List
import           System.IO.Unsafe    (unsafePerformIO)

data Grid = Grid { width :: Int, height :: Int, rows :: [[Cell]] }
  deriving Eq

instance Show Grid where
  show g = "\n" ++ unlines ls
    where
      ls :: [String]
      ls = map (concatMap show) . rows $ g

cell :: Int -> Int -> Grid -> Maybe Cell
cell right down g
  | down >= height g = Nothing
  | right >= width g = cell (right - width g) down g
  | otherwise = Just $ rows g !! down !! right

data Cell = Blank | Tree
  deriving Eq

instance Show Cell where
  show Blank = "."
  show Tree  = "#"

loadString :: String -> Either String Grid
loadString s =
  let l = lines s
      w = length (head l)
  in fmap (\cs -> Grid w (length cs) cs) . traverse (uncurry (parseLine' w)) . zip [0 ..] $ l

parseLine' :: Int -> Int -> String -> Either String [Cell]
parseLine' len i
  = first (\s -> "Problem with line " ++ show i ++ ": " ++ s)
  . (checkLength len <=< parseLine)

checkLength :: Int -> [a] -> Either String [a]
checkLength len cells =
  let l = length cells
  in if l == len
      then Right cells
      else Left ("Expected " ++ show len ++ " chars, but got " ++ show l)

parseLine :: String -> Either String [Cell]
parseLine
  = traverse (uncurry parseCell)
  . (`zip` [0 ..])

loadStringUnsafe :: String -> Grid
loadStringUnsafe = either error id . loadString

loadFileUnsafe :: FilePath -> IO Grid
loadFileUnsafe = fmap loadStringUnsafe . readFile

parseCell :: Char -> Int -> Either String Cell
parseCell '.' _ = return Blank
parseCell '#' _ = return Tree
parseCell c i   = Left $ "Invalid char " ++ show c ++ " at position " ++ show i

countTrees' :: Grid -> Int -> Int -> Int
countTrees' grid right down = go 0 0 0
  where
    go acc x y = case cell x y grid of
      Nothing    -> acc
      Just Blank -> go acc (x + right) (y + down)
      Just Tree  -> go (acc + 1) (x + right) (y + down)

countTrees :: Grid -> Int -> Int -> Int
countTrees grid right down = execState (go 0 0) 0
  where
    go :: Int -> Int -> State Int ()
    go x y = do
      case cell x y grid of
        Nothing -> return ()
        Just c -> do
          when (c == Tree) $ modify (+1)
          go (x + right) (y + down)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO . readFile $ "data/day3.txt"

combo :: Grid -> Int
combo g = let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
          in product . map (uncurry (countTrees g)) $ slopes
