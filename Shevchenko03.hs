{-# OPTIONS_GHC -Wall #-}
module Shevchenko03 where

-- Код - просто список символів - десяткових цифр '0' ..'9'
type Code = String

-- Крок гри (Move) будує конструктор Move використовуючи спробу (Code) і два цілих:  
--    кількість "биків" і "корів"  у пропозиції-спробі по відношенню до коду-числа 
data Move = Move Code Int Int
          deriving (Show, Eq)

-- Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches (c:cd) (a:att) = if c == a then 1 + exactMatches cd att else exactMatches cd att
exactMatches [] _ = 0
exactMatches _ [] = 0

-- Задача 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits cd = [count x cd| x <- ['0'..'9']] where
    count _ [] = 0
    count y (x:xs)
        | y == x = 1 + count y xs
        | otherwise = count y xs

-- Задача 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd att = sum $ f1 (countDigits cd) (countDigits att)
f1 :: [Int] -> [Int] -> [Int]
f1 (x:cd) (y:att) = (min x y) : f1 cd att
f1 _ _ = []
 
-- Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd att = Move att (exactMatches cd att) (matches cd att - exactMatches cd att)

-- Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move as bulls cows) cs = (Move as bulls cows) == (getMove cs as)

-- Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = filter (isConsistent mv) cdx

-- Задача 7 -----------------------------------------
allCodes :: Int -> [Code]
ns :: [Char]
ns= ['0'..'9']
allCodes 1 = [[num] | num <- ns]
allCodes n = concatMap (\cdx -> [cdx++[num] | num <- ns]) (allCodes (n-1))
   
-- Задача 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = f (allCodes (length cd)) cd 
f:: [Code] -> Code -> [Move]
f [] _ = []
f [c] cd = [getMove cd c]
f (c:cds) cd = (getMove cd c): f (filterCodes (getMove cd c) cds) cd  