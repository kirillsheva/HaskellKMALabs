{-# OPTIONS_GHC -Wall #-}
module HWC01 where

-- ������ 1 -----------------------------------------
factorial :: Integer -> Integer
factorial n = if n == 0 then 1 else n * factorial(n-1)

-- ������ 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if null xs then ys else if null ys then xs else (head xs + head ys):listSum (tail xs) (tail ys)

-- ������ 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven [xs] = [xs]
oddEven [] = []
oddEven (x1:x2:xs) = x2:x1:oddEven xs 

-- ������ 4 -----------------------------------------
position    ::  Int -> [Int] -> Int
position n xs | xs == [] = -1
              | head xs == n = 0
              | otherwise = 1 + (position  n (tail xs))
                   
-- ������ 5 -----------------------------------------
set :: [Int] -> [Int] 
set [] = []
set [xs] = [xs]
set (x:xs) = if notElem x xs then x:set xs else set xs
-- ������ 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set (xs++ys)

-- ������ 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = set ([x | y <- ys, x <-xs, x==y])

-- ������ 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1..]] 