{-# OPTIONS_GHC -Wall #-}
module Shevchenko09 where

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Ord, Show)

-- Задача 1 -----------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (s:ss) (t:ts) = (s == t) && (isPrefix ss ts)

-- Задача 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition l1 l2  = part l1 l2 []
  where
    part [] ys pref = (pref, [], ys)
    part xs [] pref = (pref, xs, [])
    part (x:xs) (y:ys) pref
      | x /= y    = (pref, (x:xs), (y:ys))
      | otherwise = part xs ys (pref++[x])

-- Задача 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes ls = take (length ls) (iterate tail ls)

-- Задача 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring s t = any (isPrefix s) (suffixes t)

-- Задача 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings s m= [n-(length w) | w <- suffixes m, isPrefix s w]
    where n = length m

-- Задача 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf x) = [x]
getIndices (Node xs)= concatMap (getIndices . snd) xs

-- Задача 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' sub (Node z) = concatMap helper z
  where
    helper (a, t)
      | null s = getIndices t
      | null m = findSubstrings' s t
      | otherwise = []
      where
        (p,s,m) = partition sub a
findSubstrings' _ _ = []

-- Задача 8 -----------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (sub,n) (Node z) 
  | z == (map help z) = Node ((sub, Leaf n) : z)
  | otherwise = Node (map help z)
  where
    help (a,t)
      | null p = (a,t)
      | p == a = (a, insert (s_p,n) t)
      | p /= a = (p, Node [(s_p,Leaf n),(a_p,t)])
      where
        (p,s_p,a_p) = partition sub a

-- Ця функція задана
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

-- Задача 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring (Node x) = s
  where
    (n, s) = (maximum . concatMap (f (0, []))) x
    f :: (Int, String) -> (String, SuffixTree) -> [(Int, String)]
    f (n, x) (s, (Node lt))  = p : concatMap (f p) lt
      where
        p = (n + length s, x ++ s)
    f x _  = []


------------------------------------------------------
-- Приклади рядків і суфіксних дерев..

s1 :: String
s1 = "banana"

s2 :: String
s2  = "mississippi"

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]