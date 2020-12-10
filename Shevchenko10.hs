{-# OPTIONS_GHC -Wall #-}
module Shevchenko10 where

data BinTree a = EmptyB 
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- ïîðîæíº 2-3-äåðåâî!!!
                   deriving (Eq, Show)

-- Çàäà÷à 1 -----------------------------------------			   
isSearch :: (Ord a) => BinTree a -> Bool 
isSearch (Node _ EmptyB EmptyB) = True 
isSearch (Node a l EmptyB) = a > (getNode l) && isSearch l
isSearch (Node a EmptyB r)= a < (getNode r) && isSearch r 
isSearch (Node a l r) = a > (getNode l) && a < (getNode r) && isSearch l && isSearch r
isSearch EmptyB = True

isEmpty :: (Ord a) => BinTree a -> Bool 
isEmpty EmptyB = True 
isEmpty (Node _ _ _) = False
getNode :: BinTree a -> a 
getNode (Node a _ _) = a
-- Çàäà÷à 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool 
elemSearch (Node a tl tr) e = if (a/=e) then if (a>e) then elemSearch tl e else elemSearch tr e else True
elemSearch EmptyB _ = False
-- Çàäà÷à 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a 
insSearch EmptyB a = Node a EmptyB EmptyB
insSearch (Node a l r) x =
    if x < a
        then Node a (insSearch l x) r
        else if x > a
            then Node a l (insSearch r x)
            else Node a l r

-- Çàäà÷à 4 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a 
delSearch n x= if isEmpty(n)
 then n
else if (x<getNode(n)) 
  then (Node (getNode n) (delSearch (getLeft n) x) (getRight n))
else if (x>getNode(n)) 
 then (Node (getNode n) (getLeft n) (delSearch (getRight n) x))
else if (x==getNode(n)&&isEmpty(getLeft n)&&isEmpty(getRight n)) 
 then EmptyB
else if  (x==getNode(n)&&isEmpty(getLeft n))
   then getRight(n)
else if  (x==getNode(n)&&isEmpty(getRight n)) 
  then getLeft(n)
else (Node (getNode(getLeft(n))) (delSearch (getLeft(n)) (getNode(getLeft n))) (getRight n))

-- Çàäà÷à 5 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList a = trList (foldl (insSearch) EmptyB a)

trList :: (Ord a) => BinTree a -> [a]
trList EmptyB = []
trList (Node a l r) = trList(l) ++ [a] ++ trList(r)

-- Çàäà÷à 6-----------------------------------------
isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 (Leaf _) = True
isTree23 Empty23 = True
isTree23 (Node3 l x m y r) = x >= mT l && x == mint m && y >= mT m && y == mint r
isTree23 (Node2 l x r) = x >= mT l && x == mint r

----------------------------------------------

-- Çàäà÷à 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool 
elemTree23 Empty23 _ = False 
elemTree23 (Leaf a) e = e == a 
elemTree23 (Node2 tl x tr) e = if (e==x)
  then (elemTree23 tl e) || (elemTree23 tr e)
else if (e < x) 
  then elemTree23 tl e
   else elemTree23 tr e 
elemTree23 (Node3 tl x tm y tr) e = if (e < x) 
  then elemTree23 tl e
  else if (e==x)
   then (elemTree23 tl e) || (elemTree23 tm e) 
    else if (y==e)
     then (elemTree23 tm e) || (elemTree23 tr e)
     else if (y < e)
      then elemTree23 tr e 
      else elemTree23 tm e
-- Çàäà÷à 8-----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 l1 l2 = toList l1 == toList l2

-- Çàäà÷à 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a 
insTree23 = undefined

-- isTerminal tr = True <=> ÿêùî ñèíè âóçëà tr - ëèñòêè !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Ðåçóëüòàò âñòàâêè âóçëà â 2-3-äåðåâî, 
--   êîð³íü ÿêîãî - âóçîë âèäà Node2 àáî Node3 º îá`ºêò ³ç (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - ðåçóëüòàò âñòàâêè - îäíå 2-3-äåðåâî a 
--   : (a, Just (w, b)) - ðåçóëüòàò âñòàâêè äâà 2-3-äåðåâà a i b (w - íàéìåíøå çíà÷åííÿ â b)
--  insert v tr - äîäàº çíà÷åííÿ v â äîâ³ëüíå äåðåâî tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tr - äîäàºòüñÿ çíà÷åííÿ v â äåðåâî tr ç êîíåì - òåðì³íàëüíèé âóçîë 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insNode v tr - äîäàº çíà÷åííÿ v â äåðåâî tr ç êîðíåì - íåòåðì³íàëüíèé âóçîë 
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode = undefined

---  Á³íàðí³ äåðåâà 
bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB 
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB 
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                       EmptyB)

---- 2-3-äåðåâà
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )

getRight::BinTree a -> BinTree a
getRight EmptyB = error "nothing"
getRight (Node _ _ x) = x 
getLeft::BinTree a -> BinTree a 
getLeft EmptyB = error "nothing"
getLeft (Node _ x _) = x
toList :: (Ord a) => Tree23 a -> [a]
toList Empty23 = []
toList (Leaf e) = [e]
toList (Node2 t1 _ t2) = toList t1 ++ toList t2
toList (Node3 t1 _ t2 _ t3) = toList t1 ++ toList t2 ++ toList t3
mT :: Tree23 a -> a
mT (Leaf x) = x
mT (Node2 _ _ r) = mT r
mT (Node2 _ x Empty23) = x
mT (Node3 _ _ _ _ r) = mT r
mT (Node3 _ _ _ x Empty23) = x
mint :: Tree23 a -> a
mint (Leaf x) = x
mint (Node2 l _ _) = mint l
mint (Node2 Empty23 x _) = x
mint (Node3 l _ _ _ _) = mint l
mint (Node3 Empty23 x _ _ _) = x
