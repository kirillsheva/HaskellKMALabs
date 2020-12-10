{-# OPTIONS_GHC -Wall #-}
module Shevchenko06 where

newtype Poly a = P [a]

-- Задача 1 -----------------------------------------
x :: Num a => Poly a
x = P [0,1]

-- Задача 2 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = clean a == clean b
clean :: (Eq a, Num a) => [a] -> [a]
clean = reverse . dropWhile (==0) . reverse

-- Задача 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = if preppd /= [] then f 0 preppd else "0"
        where
            preppd = clean p

            f :: (Num a, Eq a, Show a) => Int -> [a] -> String
            f _ [] = ""
            f e ([c]) = format e c 
            f e (0:cs) = f (e + 1) cs
            f e (c:cs) = f (e + 1) cs ++ " + " ++ format e c 

            format :: (Num a, Eq a, Show a) => Int -> a -> String
            format _ 0 = "0"
            format 1 1 = "x"
            format 0 c = show c
            format 1 c = show c ++ "x" 
            format e 1 = "x^" ++ show e
            format e c = show c ++ "x^" ++ show e
-- Задача 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ f a b
    where
        f :: (Num a) => [a] -> [a] -> [a]
        f aa [] = aa
        f [] bb = bb
        f (aa:as) (bb:bs) = aa + bb : f as bs

-- Задача 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = sum $ f 0 a b
    where
        f :: (Num a) => Int -> [a] -> [a] -> [Poly a] 
        f _ [] _ = [0]
        f c [aa] bs = [item c aa bs]
        f c (aa:as) bs = item c aa bs : f (c + 1) as bs
        item c aa bs = P $ replicate c 0 ++ map (*aa) bs

-- Задача 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P $ map (*(-1)) p 
    fromInteger n = P [fromInteger n] 
    -- Розумних означень не існує
    abs    = undefined
    signum = undefined

-- Задача 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P p) n = f 0 n p 
    where
        f :: (Num a) => Int -> a -> [a] -> a
        f _ _ [] = 0
        f e xx (c:cs) = c*xx^e + f (e + 1) xx cs 

-- Задача 8 -----------------------------------------
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 a = deriv a 
    nderiv n a = nderiv (n - 1) $ deriv a 

-- Задача 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    deriv (P xs) = P (zipWith (\i n -> fromInteger i * n) [1..] (drop 1 xs))
