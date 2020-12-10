{-# OPTIONS_GHC -Wall #-}
module Shevchenko05 where
import Data.List
type Graph = [[Int]]

-- Задача 1 ------------------------------------------
lucky ::   Int ->  [String]
lucky n = filter (\x -> (summ (fNumber x)) == (summ (last x))) all where
        all :: [String]
        fNumber :: String -> String
        last :: String -> String
        all = map show [(10 ^ (n*2 - 1))..(10 ^ (n*2) - 1)]
        fNumber n0 = take n n0
        last n0 = drop n n0
        summ n1 = sum $ map (\x -> read [x] :: Int) n1

-- Задача 2 -----------------------------------------  
queens ::  Int -> [[Int]]
queens x = filter check (create x)
           where create 0 = [[]]
                 create xs = [y : ys | y <- [1..x], ys <- create (xs-1)]
                 check (y:ys) = isSafe y ys && check ys
                 check [] = True
                 isSafe try ys = not (elem try ys || checkd try ys)
                 checkd try ys = any (\(numc,y) -> abs (try - y) == numc) $ zip [1..] ys
   
-- Задача 3 -----------------------------------------
maxLen ::  [Int] -> Int
maxLen s = length $ snd $ maximum $ [(length s1, s1) | s1 <- (allS s)]
 
-- Task 4 -----------------------------------------
maxSeq ::  [Int] ->  [Int]
maxSeq s = snd $ maximum $ [(length s1, s1) | s1 <- (allS s)]

-- Task 5 -----------------------------------------
allMaxSeq ::  [Int] -> [[Int]]
allMaxSeq s = filter (\s1 -> (length s1) == (maxLen s)) (allS s)


-- Задача 6 -----------------------------------------
genExpr ::  Int -> Int -> [String]
genExpr a b = map (\z -> inf (filter (/=' ') z) (nOpers + nOpers + 1)) $ filter (\y -> pol y == b) $ map (\x -> getPoliz (show a) x) (combinations nOpers "+-*")
        where
            combinations n _ | n <= 0 = [[]]
            combinations 1 xs = map (:[]) xs
            combinations n xs = (:) <$> xs <*> combinations (n-1) xs
            nOpers = (length $ show a) - 1
            getPoliz [] opers = opers
            getPoliz nums opers = if length nums == (nOpers + 1) then (head nums) : ' ' : getPoliz (tail nums) opers
            else (head nums) : ' ' : (head opers) : ' ' : getPoliz (tail nums) (tail opers)

-- Задача 7 -----------------------------------------
genExprBracket ::  Int -> Int -> [String]
genExprBracket  n b = map show (filter ((== Just b) . evaluate) (exps (reverse (nums n))))

-- Задача 8 -----------------------------------------
topolSortAll :: Graph -> [[Int]]
topolSortAll = undefined

--------------------------------------------
gr1 :: Graph 
gr1 = [[1,2,3], [], [3,4], [4],[]]

allS :: [Int] -> [[Int]]
allS l = filter chck (subsequences l)
        where
            chck seq0 = (sort seq0) == seq0 && isUnique seq0
            isUnique [] = True
            isUnique (h:[]) = True
            isUnique seq0 = (head seq0) /= (head $ tail seq0) && (isUnique $ tail seq0)

inf :: String -> Int -> String
inf [] _ = []
inf plz l = 
    if length plz == l
        then (head plz) : inf (tail plz) l
        else (head $ tail plz) : (head plz) : inf (tail $ tail plz) l


pol :: String -> Int
pol input = head $ foldl foldElems [] (words input)
        where
            foldElems (x:y:ys) "*" = (x * y):ys
            foldElems (x:y:ys) "+" = (x + y):ys
            foldElems (x:y:ys) "-" = (y - x):ys
            foldElems xs nmbr = read nmbr:xs

exps :: [Int] -> [ExP]
exps [x] = [ValueP x]
exps xs = do
        o <- [ Op (+)  "+" (const (const True)), Op (-) "-" (const (const True)), Op (*) "*" (const (const True))]
        pos <- [1..n-1]
        l <- exps (take pos xs)
        r <- exps (drop pos xs)
        return (ExP o l r)
        where 
          n = length xs
 
evaluate :: ExP -> Maybe Int
evaluate (ValueP v) = Just v
evaluate (ExP o l r) = do
    le <- evaluate l
    re <- evaluate r
    if isEquals o le re then Just (f o le re) else Nothing
 
instance Show ExP where
    show (ValueP v) = show v
    show (ExP o l r) = "(" ++ show l ++ name o ++ show r ++ ")" 

nums :: Int -> [Int]
nums n = if (n < 0) then [] else if n < 10 then [n] else n `mod` 10 : nums (n `div` 10)
data ExP = ValueP Int | ExP Op ExP ExP
data Op = Op {
    f :: Int -> Int -> Int,
    name :: String,
    isEquals :: Int -> Int -> Bool }