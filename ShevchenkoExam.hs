{-# OPTIONS_GHC -Wall #-}
module Shevchenko00 where

data Quaternion = Quaternion Double Double Double Double 
                  deriving (Eq)

type Graph  = [[Int]]
data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                  deriving (Show, Eq) 

-- Задача 1 -----------------------------------------
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = equals : group others
    where
        (equals, others) = span (== x) (x:xs)
-- Задача 2 -----------------------------------------
bagSubbag :: String -> String -> Bool
bagSubbag [] _ = True
bagSubbag _ [] = False
bagSubbag (a:b) res | elem a res = bagSubbag b (delOne res a)| otherwise = False

-- Задача 3 -----------------------------------------
amge :: Int -> Char -> String
amge a sy = [sy | _<-[1..a]]

bagUnion :: String -> String -> String
bagUnion [] l = l
bagUnion (q:ls) lm = let a = 1+ amt ls q
                         b = amt lm q
                         c = remove ls q
                         d = remove lm q
                         r = if a>b then amge a q else amge b q in r ++ bagUnion c d

-- Задача 4 -----------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate element [1]

-- Задача 5 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency a = [(counter x a,x) | x <- frequency' a]

-- Задача  6 -----------------------------------------
instance Show Quaternion where
     show (Quaternion r i j k ) = show r ++ " + " ++ show i ++"i"++" + "++show j++"j"++" + "++show k ++"k"

-- Задача 7 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion r i j k) (Quaternion r1 i1 j1 k1) =
  (Quaternion (r + r1) (i + i1) (j + j1) (k + k1))

-- Задача 8 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion (Quaternion r i j k) (Quaternion r1 i1 j1 k1) =
  (Quaternion(r * r1 - i * i1 - j * j1 - k * k1)
    (r * i1 + i * r1 + j * k1 - k * j1)(r * j1 - i * k1 + j * r1 + k * i1)(r * k1 + i * j1 - j * i1 + k * r1))


--- Задача 9 ----------------------------------------
instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion
    negate (Quaternion r i j k) = (Quaternion (-r) (-i) (-j) (-k))
    fromInteger n = (Quaternion (fromIntegral n) 0 0 0)
    abs (Quaternion r i j k) = (Quaternion (sqrt (r*r + i*i + j*j + k*k)) 0 0 0)
    signum (Quaternion r i j k) = 
      (Quaternion (r/res) (i/res) (j/res) (k/res))
       where
        res = sqrt(r*r + i*i + j*j + k*k)

-- Задача 10 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int] 
shortWay [] _ _ = []
shortWay g r t = let w = gn g t [[r]] in if w==[] then [] else w!!0

-- Задача 11 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting g | length (components g) == 1 = True | otherwise = False

-- Задача 12 ------------------------------------
components :: Graph -> [[Int]] 
components g = chelp g 0 []

-- Задача 13 ------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity g e = maximum [length (shortWay g e res) | res <- nhelp g] - 1

-- Задача 14 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter = maximum . finder

findRadius :: Graph -> Int 
findRadius = minimum . finder

-- Задача 15 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter g =let res = findRadius g in [x | x <- [0..length g - 1], eccentricity g x == res]

-- Задача 16 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM a b c d) = b>0 
                              && maybe False (a>) (helpNode c)
                              && maybe False (a<) (helpNode d) 

-- Задача 17 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch r t = case r of
    NodeM a _ c d -> 
                    if a == t then True 
                               else 
                                if a < t then elemSearch d t 
                                else elemSearch c t
    EmptyM -> False

-- Задача 18 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
insSearch EmptyM v = NodeM v 1 EmptyM EmptyM
insSearch (NodeM a k l r) x =
    if x < a
        then NodeM a k(insSearch l x) r
        else if x > a
            then NodeM a k l (insSearch r x)
            else NodeM a (k+1) l r


-- Задача 19 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _=  EmptyM
delSearch (NodeM a b c d) v | v == a = if b>1 
  then NodeM a (b-1) c d 
  else delhelp (NodeM a b c d )| v < a = NodeM a b (delSearch c v) d| otherwise = NodeM a b c (delSearch d v)

-- Задача 20 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = helpsort $ foldl insSearch EmptyM xs

--------------Helpers---------------


delOne :: String -> Char -> String
delOne [] _ = ""
delOne (a:b) res | a==res = b| otherwise = [a] ++ delOne b res

element::[Integer]->[Integer]
element e = zipWith (+)(e ++ [0])([0] ++ e)

gn :: Graph -> Int -> [[Int]] -> [[Int]]
gn _ _ [] = []
gn g r x = let w = [n | n<-x, n!!(length n-1)==r]in if length w > 0 then w else gn g r (helper g x)
frequency' :: [Int] -> [Int]
frequency' [] = []
frequency' (x:xs) = x : filter (/=x) (frequency' xs)

chelp :: Graph -> Int -> [[Int]] -> [[Int]]
chelp g t r | t == length g = r| ret = chelp g (t+1) r| otherwise = chelp g (t+1) $ r ++ [hel] where ret = True `elem` [t `elem` x | x <- r]
                                                                                                     hel = [x | x <- [t..length g - 1], hw g t x]
counter :: Int -> [Int] -> Int
counter _ [] = 0
counter x (y:ys) | x==y = 1+(counter x ys)| otherwise = counter x ys
helper :: Graph -> [[Int]] -> [[Int]]
helper _ [] = []
helper g (a:as) = let adj = g!!(a!!(length a - 1))in [a++[x] | x<-adj, not (elem x a)] ++ helper g as

nhelp :: Graph -> [Int]
nhelp grap = [0..(length grap - 1)]
hw :: Graph -> Int -> Int -> Bool
hw g r w = [] /= shortWay g r w
finder :: Graph -> [Int]
finder g = [eccentricity g x | x <- nhelp g]

delhelp :: (Ord a) => BinTreeM a -> BinTreeM a
delhelp (NodeM _ _ l  EmptyM) = l
delhelp (NodeM _ _ EmptyM  d) = d
delhelp (NodeM _ b c d) = NodeM (leftEl d) b c d

remove :: String -> Char -> String
remove [] _ = []
remove (x:ss) a | x==a = remove ss a| otherwise = [x]++(remove ss a)
leftEl :: (Ord a) => BinTreeM a -> a
leftEl (NodeM a _ EmptyM  _) = a
leftEl (NodeM _ _ c _) = leftEl c
helpsort::(Ord a)=> BinTreeM a -> [a]
helpsort EmptyM = []
helpsort (NodeM a b c d ) = helpsort c ++(replicate b a)++helpsort d


amt :: String -> Char -> Int
amt [] _ = 0
amt (x:ss) a | a==x = 1 + (amt ss a)| otherwise = amt ss a


helpNode :: (Ord a) => BinTreeM a -> Maybe a 
helpNode (NodeM a _ _ _) = Just a
helpNode EmptyM = Nothing 

---------------------Тестові дані - Графи -------
gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   