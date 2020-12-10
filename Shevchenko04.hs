{-# OPTIONS_GHC -Wall #-}
module Shevchenko04 where

type Graph  = [[Int]]

-- Задача 1 ------------------------------------
isGraph :: Graph -> Bool 
isGraph gr = if elem [] gr then True else False

-- Задача 2 ------------------------------------
isTournament :: Graph -> Bool 
edges :: Graph -> [(Int,Int)]
edges g = [(x,y)| x <- [0..(length g -1)], y <- g!!x] 
f:: Graph -> Int
f [] = 0
f (g:gr) = length (g:gr) + length gr +1
isTournament gr = if length (edges gr) == f gr then True else False

-- Задача 3 ------------------------------------
isTransitive :: Graph -> Bool 
isTransitive [[]] = False
isTransitive [] = False
isTransitive gr = foldl (&&) True[ (fst u_v, snd v_w) `elem` (aEdges gr) | u_v <- (aEdges gr), v_w <- (aEdges gr), (snd u_v == fst v_w)]
aEdges :: Graph -> [(Int, Int)]
aEdges gr = [(x, y) | x <- (nodes gr), y <- gr !! x]

-- Задача 4 ------------------------------------
buildTransitive :: Graph -> Graph 
sort :: [Int] -> [Int]
build :: [[[Int]]] -> [Int]
remove :: Int -> [Int] -> [Int]
set :: [Int] -> [Int]
nodes :: Graph -> [Int]
nodes graph = [0 .. ((length graph) - 1)]
build ways = sort (set (foldl1 (++) (dWay ways)))
sort [] = []
sort xs = foldl insert [] xs
insert :: [Int] -> Int -> [Int]
insert xs x = (filter (<= x) xs) ++ [x] ++ (filter (> x) xs)
set [] = []
set (x:xs) = x : (set (remove x xs))
remove _ [] = []
remove x (y:ys)| x == y = remove x ys| otherwise = y : (remove x ys)
dWay :: [[[Int]]] -> [[Int]]
dWay ways = [x | y <- ways, g <- y, g /= [], let x = (init g)]
buildTransitive gr = map build (map (sumWay gr) (nodes gr))

-- Задача 5 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
isit::([[[Int]]]) -> Bool
sumWay:: Graph ->Int ->[[[Int]]] 
sumWay gr v = until isit(step gr)[[[v]]]
isit wss = null (head wss)
step ::Graph -> [[[Int]]] -> [[[Int]]]
step gr wss@(wsn:_) = [t:w | w@(v:vs) <- wsn, notElem v vs, t <- gr!!v]:wss
everWay :: Graph -> Int -> Int -> [[Int]]
everWay gr from to =[way | ways <- sumWay gr from, way <- ways, (head way) == to]
longWay gr from to | null (everWay gr from to) = Nothing| null (head (everWay gr from to)) = Nothing| otherwise = Just $ reverse $ head $ (everWay gr from to)

-- Задача 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay = undefined

-- Задача 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic = undefined

-- Задача 8 ------------------------------------
topolSort :: Graph -> Maybe [Int] 
topolSort = undefined

-- Задача 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort = undefined

---------------------Тестові дані - Графи -------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]