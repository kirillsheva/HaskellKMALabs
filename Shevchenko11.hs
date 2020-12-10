{-# OPTIONS_GHC -Wall #-}
module Shevchenko11 where
import Data.List
import Data.Char(isLower)
import Data.Maybe
data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Задача 1 -----------------------------------------
func :: Eq a => a -> [(a, b)] -> b
func it par = fromJust (lookup it par)

checkSat :: BDD -> Env -> Bool
checkSat (idd, n) e | notElem idd ro = idd == 1| func x e = checkSat (right, n) e | otherwise        = checkSat (left, n) e
     where (x, left, right) = func idd n
           ro = map fst n
-- Задача 2 -----------------------------------------
sat :: BDD -> [[(Char, Bool)]]
sat bdd@(idd, n) = filter (\x -> checkSat bdd x) pos
      where v = (nub . sort) (map (\(_, (x, _, _)) -> x) n)
            pos = sequence (map (\x -> [(x, b) | b <- [True, False]]) v)

-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify  (Or  (Bvalue a) (Bvalue b)) = Bvalue $ a || b
simplify (And (Bvalue a) (Bvalue b)) = Bvalue $ a && b
simplify (Not (Bvalue b)) = Bvalue (not b)
simplify v = v

-- Задача 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict (Bvar x) idd bool
  | x == idd   = (Bvalue bool)
  | otherwise = Bvar x
restrict (Bvalue x) _ _= (Bvalue x)
restrict (And x y) idd bool= simplify (And (restrict x idd bool) (restrict y idd bool))
restrict (Not x) idd bool= simplify (Not (restrict x idd bool))
restrict (Or x y) idd bool = simplify (Or (restrict x idd bool) (restrict y idd bool))

-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildBDD :: BExp -> [Char] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' (Bvalue True) _ []     = (1, []) 
buildBDD' (Bvalue False) _ []     = (0, []) 
buildBDD' e t (x:xs) = (t, node : lt ++ rt) 
  where
    (left, lt) = buildBDD' (restrict e x False) (2 * t) xs
    (right, rt) = buildBDD' (restrict e x True) (2 * t + 1) xs
    node = (t, (x, left, right))

-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
buildROBDD :: BExp -> [Char] -> BDD
buildROBDD e xs
  = fb e 2 xs []

fb :: BExp -> NodeId -> [Char] -> [BDDNode] -> BDD
rev i t = lookup i [(v, k) | (k, v) <- t]
fb (Bvalue r) _ _ nod
  = (fromEnum r, nod)
fb e n (i : is) nod
  | l == r   
   = (l, nod') 
  | otherwise = maybe (n, noder : nod'')(\n' -> (n', nod''))(rev (i, l, r) nod'')
  where
    eF = restrict e i False
    eT = restrict e i True
    (l, nod')  = fb eF (2 *  n) is nod
    (r, nod'') = fb eT (2 * n + 1) is nod'
    noder = (n, (i, l, r))
-- Задача 7 -----------------------------------------
fullBexp :: String -> Maybe BExp 
fullBexp = undefined 

bexp :: String -> Maybe (BExp,String)
bexp = undefined

bcon :: String -> Maybe (BExp,String)
bcon = undefined

manyCon :: (BExp,String) -> Maybe (BExp,String)
manyCon = undefined

bdis :: String -> Maybe (BExp,String)
bdis = undefined

manyDis :: (BExp,String) -> Maybe (BExp,String)
manyDis = undefined

------------------------------------------------------
-- Приклади для тестування..
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])



