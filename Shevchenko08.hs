{-# OPTIONS_GHC -Wall #-}
module Shevchenko08 where
import Data.List
import Data.Tree

-- Задача 1 -----------------------------------------			   
rank :: Tree a -> Int
rank r = length (sons r) 

-- Задача 2-----------------------------------------
isBinomTree :: Ord a => Tree a -> Bool
isBinomTree (Node _ []) = True

-- Задача 3 -----------------------------------------
isBinomHeap :: Ord a => Forest a -> Bool
isBinomHeap = undefined

-- Задача 4 -----------------------------------------
combineTrees :: Ord a => Tree a -> Tree a -> Tree a
combineTrees b1 b2 | k b1 < k b2 = (Node (k b1)(b2 : sons b1))| otherwise = (Node (k b2)(b1 : sons b2))
sons :: Tree a -> [Tree a]
sons (Node _ (n))= n
k :: Tree a -> a
k (Node n _)= n
-- Задача 5 -----------------------------------------

extractMin :: Ord a => Forest a -> a
extractMin = minimum . (map val)
val :: Tree a -> a
val (Node a _) = a

-- Задача 6-----------------------------------------
mergeHeaps :: Ord a => Forest a -> Forest a -> Forest a
mergeHeaps h []= h
mergeHeaps [] heap= heap
mergeHeaps h@(t:ts) heap@(t':ts')| rank t < rank t' = t : mergeHeaps ts heap| rank t' < rank t = t' : mergeHeaps ts' h| otherwise = mergeHeaps (mergeHeaps ts ts') [combineTrees t t']

-- Задача 7-----------------------------------------
insert :: Ord a => a -> Forest a -> Forest a
insert x = mergeHeaps [Node x []]

-- Задача 8-----------------------------------------
deleteMin :: Ord a => Forest a -> Forest a
deleteMin heap = mergeHeaps heapp mrh 
  where
    mrt = head $ dropWhile (\t -> val t /= extractMin heap) heap
    mrh = reverse $ sons mrt
    heapp = filter (\t -> t /= mrt) heap

-- Задача 9-----------------------------------------
binomSort :: Ord a => [a] -> [a]
binomSort [] = []
binomSort bs = minV : binomSort (delete minV bs)
    where
      heap (b:bs) = Node b [] : heap bs
      heap [] = []
      minV = extractMin $ heap bs

-- Задача 10 -----------------------------------------
toBinary :: Forest a -> [Int]
toBinary h = reverse $ tb [0..] h
  where
    tb _ [] = []
    tb (n:ns) t'@(t:ts)| n == rank t = 1 : (tb ns ts)| otherwise   = 0 : (tb ns t')

-----------------------------------------------------  
-- Приклади деяких дерев...

t1, t2, t3, t4, t5, t6, t7, t8 :: Tree Int
--  Зауваження: t7 - результат злиття t5 і t6

-- t1 .. t4 з'являються на Мал. 1...
t1 = Node 4  []
t2 = Node 1 [Node 5 []]
t3 = Node 2 [Node 8 [Node 9 []], 
             Node 7 []]
t4 = Node 2 [Node 3 [Node 6 [Node 8 []], 
                     Node 10 []],
             Node 8 [Node 9 []],
             Node 7 []]

-- t5 і t6 зліва на Мал.2; t7 - справа на Мал.2
t5 = Node 4 [Node 6 [Node 8 []], 
                     Node 10 []]
t6 = Node 2 [Node 8 [Node 9 []], Node 7 []]
t7 = Node 2 [Node 4 [Node 6 [Node 8 []], Node 10 []],
             Node 8 [Node 9 []], 
             Node 7 []]

-- Додаткове дерево...
t8 = Node 12 [Node 16 []]

------------------------------------------------------
-- Приклади деяких куп...

h1, h2, h3, h4, h5, h6, h7 :: Forest Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 [Node 12 [Node 16 []],
              Node 5 []],
      Node 2 [Node 4 [Node 6 [Node 8 []],
                      Node 10 []],
              Node 8 [Node 9 []],
              Node 7 []]]

-- h3 показана на Мал.3...
h3 = [t1, t2, t4]

-- Дві додаткові купи використовуються далі. Вони зліва на Мал.4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 - результат злиття h4 і h5, справа на Мал.4(b)...
h6 = [Node 4 [],
      Node 1 [Node 4 [Node 6  [Node 8 []],
                      Node 10 []],
              Node 12 [Node 16 []],
              Node 5 []]]

-- h7 показана на Мал.5...
h7 = [Node 4 [Node 4 [Node 12 [Node 16 []],
                      Node 5 []],
              Node 6 [Node 8 []],
              Node 10 []]]  