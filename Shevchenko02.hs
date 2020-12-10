{-# OPTIONS_GHC -Wall #-}
module Shevchenko02 where

-- ������ 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr xs = foldr (+) 0 xs
  
-- ������ 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n = foldl (*) 1 [2..n]

-- ������ 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
f :: Integer -> [Integer] -> [Integer]
concatFr xs ys = foldr f ys xs
f x ys = x:ys

-- ������ 4 -----------------------------------------
sortInsert:: [Integer] -> [Integer]
insert :: [Integer] -> Integer -> [Integer]
insert as a = [x | x <- as, x < a] ++ [a] ++ [x | x <- as, x >= a]
sortInsert xs = foldl (insert) [] xs

-- ������ 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ _ [] = []
map2 _ [] _ = []
map2 fc (x:xs) (y:ys) = fc x y : map2 fc xs ys

-- ������ 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart a n = sum [ (fromInteger (a)^i) / fromInteger (factorial i) | i <- [1..n]]

-- ������ 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl (+) 1 [2..]

-- ������ 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl (\x y -> x + y*y) 1 [2..]

-- ������ 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes xs ys= [ i| i <- [0.. length ys], xs == take (length xs) (drop i ys) ]

