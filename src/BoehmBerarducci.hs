{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module BoehmBerarducci where

-------------------------------------------------------------------------------
-- Shapes
-------------------------------------------------------------------------------

data Shape'
  = Circle' { x :: Double, y :: Double, r :: Double }
  | Rectangle' { x :: Double, y :: Double, w :: Double, h :: Double }
  deriving (Eq, Ord, Show)

exampleCircle' :: Shape'
exampleCircle' = Circle' 2.0 1.4 4.5

exampleRectangle' :: Shape'
exampleRectangle' = Rectangle' 1.3 3.1 10.3 7.7

area' :: Shape' -> Double
area' shape = case shape of
  Circle' _ _ r      -> pi * r ^ (2 :: Int)
  Rectangle' _ _ w h -> w * h

demoShape' :: IO ()
demoShape' = do
  print exampleCircle'
  print (area' exampleCircle')
  print exampleRectangle'
  print (area' exampleRectangle')

type Shape
  = forall shape
  .  (Double -> Double -> Double -> shape)
  -> (Double -> Double -> Double -> Double -> shape)
  -> shape

_Circle :: Double -> Double -> Double -> Shape
_Circle x y r _circle _rectangle = _circle x y r

_Rectangle :: Double -> Double -> Double -> Double -> Shape
_Rectangle x y w h _circle _rectangle = _rectangle x y w h

exampleCircle :: Shape
exampleCircle = _Circle 2.0 1.4 4.5

exampleRectangle :: Shape
exampleRectangle = _Rectangle 1.3 3.1 10.3 7.7

area :: Shape -> Double
area shape = shape
  (\_ _ r -> pi * r ^ (2 :: Int))
  (\_ _ w h -> w * h)

showShape :: Shape -> String
showShape shape = shape
  (\x y r -> "Circle {x = " ++ show x ++ ", y = " ++ show y ++ ", r = " ++ show r ++ "}")
  (\x y w h -> "Rectangle {x = " ++ show x ++ ", y = " ++ show y ++ ", w = " ++ show w ++ ", h = " ++ show h ++ "}")

eqShape :: Shape -> Shape -> Bool
eqShape sh1 sh2 =
  let
    e = sh1 (\x y r -> Left (x,y,r)) (\x y w h -> Right (x,y,w,h))
  in
    sh2
      (\x y r -> either (== (x,y,r)) (const False) e)
      (\x y w h -> either (const False) (== (x,y,w,h)) e)

compareShape :: Shape -> Shape -> Ordering
compareShape sh1 sh2 =
  let
    e = sh1 (\x y r -> Left (x,y,r)) (\x y w h -> Right (x,y,w,h))
  in
    sh2
      (\x y r -> either  ((x,y,r) `compare`) (const GT) e)
      (\x y w h -> either (const LT) ((x,y,w,h) `compare`) e)

isCircle :: Shape -> Bool
isCircle shape = shape (\_ _ _ -> True) (\_ _ _ _ -> False)

isRectangle :: Shape -> Bool
isRectangle shape = shape (\_ _ _ -> False) (\_ _ _ _ -> True)

demoShape :: IO ()
demoShape = do
  putStrLn $ showShape exampleCircle
  -- putStrLn . showShape $ exampleCircle
  print (area exampleCircle)
  putStrLn $ showShape exampleRectangle
  print (area exampleRectangle)

-------------------------------------------------------------------------------
-- Trees
-------------------------------------------------------------------------------

data Tree' = Node Int Tree' Tree' | Leaf
  deriving (Eq, Show)

exampleTree' :: Tree'
exampleTree' = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)

preorder' :: Tree' -> [Int]
preorder' (Node v l r) = v : preorder' l ++ preorder' r
preorder' Leaf         = []

demoTree' :: IO ()
demoTree' = do
  print exampleTree'
  print (preorder' exampleTree')

type Tree = forall tree
          .  (Int -> tree -> tree -> tree)
          -> tree
          -> tree

_Node :: Int -> Tree -> Tree -> Tree
_Node v l r node leaf = node v (l node leaf) (r node leaf)

_Leaf :: Tree
_Leaf node leaf = leaf

showTree :: Tree -> String
showTree tree = tree
  (\v l r -> "Node " ++ show v ++ " ("  ++ l ++ ")(" ++ r ++ ")")
  "Leaf"

exampleTree :: Tree
exampleTree = _Node 1 (_Node 2 _Leaf _Leaf) (_Node 3 _Leaf _Leaf)

preorder :: Tree -> [Int]
preorder tree = tree (\v l r -> v : l ++ r) []

demoTree :: IO ()
demoTree = do
  print $ showTree exampleTree
  print (preorder exampleTree)