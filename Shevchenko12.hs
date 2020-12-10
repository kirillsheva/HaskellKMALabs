{-# OPTIONS_GHC -Wall #-}
module Shevchenko12 where
import Data.Maybe
import Data.Char
data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- Çì³ííà
         | Const Value     -- êîíñòàíòà
         | Op Exp Bop Exp  -- Îïåðàö³ÿ
                 deriving (Show, Eq)
-- Á³íàðí³ (2-àðãóìåíòà) îïåðàòîðè
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | And | Or
            deriving (Show, Eq)

data Stmt = Assign String Exp
          | Read String 
          | Write Exp
          | Incr String
          | If Exp Stmt 
          | While Exp Stmt       
          | For Stmt Exp Stmt Stmt
          | Block [(String,Type)] [Stmt]        
          deriving (Show, Eq)
data Type = It | Bt deriving (Show, Eq)
type Program = Stmt

type StateW = ([String], [(String,Value)], [String])

type VarEnv  = [(String,Type)]

-- Çàäà÷à 1 -----------------------------------------
getValue :: StateW -> String -> Value
getValue n t = fromJust $ lookup t(helper n)

updValue :: StateW -> String -> Value -> StateW
updValue st idd v = (func st, m (helper st) idd v,func2 st)
                      where
                        m ((r,t):y) iddd val  | r == iddd = (iddd,val):y | otherwise = (r,t) : m y iddd val
                        m [] _ _ = []

-- Çàäà÷à 2 ----------------------------------------- 
readValue :: StateW -> Type -> (StateW,Value)
readValue ([], r, t) It = (([], r, t), I 0)
readValue ([], r, t) Bt = (([], r, t), B False)
readValue q Bt | (head $func q)  == "False" = ((tail $ func q, (helper q), (func2 q)), B False) |(head $func q)  == "True" = ((tail $func q, (helper q), (func2 q)), B True) | otherwise = (q, B False)
readValue q It | all (isDigit) $ head $ func q = ((tail $ func q, (helper q), (func2 q)), (I $ read $ head $ func q))| otherwise = (q, I 0)

-- Çàäà÷à 3 -----------------------------------------
writeValue :: StateW -> Value -> StateW 
writeValue st v = (func st, helper st, [show v] ++ func2 st)
  
-- Çàäà÷à 4 ----------------------------------------- 
evExp :: StateW -> Exp -> Value
evExp v (Var l) = getValue v l
evExp v (Op e y e1) = case y of
    Plus ->I ((getVal (evExp v e)) + (getVal (evExp v e1)))
    Minus ->I ((getVal (evExp v e)) - (getVal (evExp v e1)))
    Times ->I ((getVal (evExp v e)) * (getVal (evExp v e1)))
    Div ->I ((getVal (evExp v e)) `div` (getVal (evExp v e1)))
    Gt ->B ((getVal (evExp v e)) > (getVal (evExp v e1)))
    Ge ->B ((getVal (evExp v e)) >= (getVal (evExp v e1)))
    Lt ->B ((getVal (evExp v e)) < (getVal (evExp v e1)))
    Le ->B ((getVal (evExp v e)) <= (getVal (evExp v e1)))
    Eql -> B ((evExp v e) == (evExp v e1))
    And ->if ((getBool $ evExp v e) == True) &&((getBool (evExp v e1)) == True) then B True else B False
    Or -> if ((getBool $ evExp v e) == False) &&((getBool (evExp v e1)) == False)then B False else B True
evExp _ (Const v) = v

-- Çàäà÷à 5 -----------------------------------------
evStmt :: StateW -> Stmt -> StateW 
evStmt = undefined

-- Çàäà÷à 6 -----------------------------------------
evProgram :: Program -> [String] -> [String]
evProgram = undefined

-- Çàäà÷à 7 -----------------------------------------
iswfOp :: Bop -> [Type] -> Maybe Type 
iswfOp b [Bt, Bt]| b == And || b == Or = Just Bt
iswfOp b [It, It] | b == Eql ||  b == Gt || b == Ge || b == Le ||  b == Plus || b == Lt ||  b == Times ||b == Minus || b == Div= Just Bt
iswfOp _ _ = Nothing

-- Çàäà÷à 8 -----------------------------------------
iswfExp :: Exp -> VarEnv -> Maybe Type 
iswfExp (Const (I _)) _ = Just It
iswfExp (Const (B _)) _ = Just Bt
iswfExp (Op e b e1) v | iswfExp e v == Nothing = Nothing| iswfExp e1 v == Nothing = Nothing| otherwise = iswfOp b [fromJust $ iswfExp e v, fromJust $ iswfExp e1 v]
iswfExp (Var n) v = lookup n v 
iswfExp (Var _) [] = Nothing
-- Çàäà÷à 9 -----------------------------------------
iswfStmt :: Stmt -> VarEnv -> Bool 
iswfStmt _ [] = True
iswfStmt (Assign _ _) _ = True
iswfStmt (For s1 c s2 s3) v = iswfExp c v == Just Bt && iswfStmt s2 v && iswfStmt s3 v && iswfStmt s1 v 
iswfStmt (Incr t) v = lookup t v == Just It
iswfStmt (Block i s) v = and (map (`iswfStmt` (v++i)) s)
iswfStmt (If c stmt ) v = iswfStmt stmt v && (iswfExp c v == Just Bt)
iswfStmt (While c t) v = iswfStmt t v && (iswfExp c v == Just Bt)

--------------------------------
iswfProgram :: Program -> Bool 
iswfProgram st = iswfStmt st []

getVal :: Value -> Int
getVal (I r) = r
getBool :: Value -> Bool
getBool (B r) = r
helper :: (r, t, y) -> t
helper (_,t, _) = t
func :: (r, t, y) -> r
func (r, _, _) = r
func2 :: (r, t, y) -> y
func2 (_, _, y) = y
-- Ïðîãðàìè -------------------------------------------

{- Ââîäèòü äâà ö³ë³ çíà÷åííÿ b ³ e, 
   ÿêùî âîíè äîäàòí³, òî â çì³íí³é out
   îá÷èñëåííÿ çíà÷åííÿ b â ñòåïåí³ e. 
   Çíà÷åííÿ out âèâîäèòüñÿ

   { int b, e, out;  
     read b; read e; out:= 1;
	 if (b>0 & e>0)
	   {int i; for (i:=0; i<e; i++) out := out*b}; 
     write out  
   }
-}
power :: Program
power = Block [("b",It),("e",It),("out",It)]
              [ Read "b", Read "e", Assign "out" (Const(I 1))
              , If (Op (Op (Var "b") Gt (Const(I 0)))
                       And
                       (Op (Var "e") Gt (Const(I 0))) )
                   (Block [("i",It)]
                        [For (Assign "i" (Const(I 0))) (Op (Var "i") Lt (Var "e")) (Incr "i")
                              (Assign "out" (Op (Var "out") Times (Var "b")))
                        ]
                   )
              , Write (Var "out") 
              ]

{- Ââîäèòü ö³ëå çíà÷åííÿ à,
   ÿêùî âîíî íåâ³ä"ºìíå, òî çíàõîäèòü ö³ëîãî çíà÷åííÿ 
   êîðíÿ êâàäðàòíîãî ç³ çíà÷åííÿ a ³ âèâîäèòü éîãî.
   Ïðè â³ä"ºìíîìó à âèâîäèòüñÿ -1.   
   
   { int a, b; 
     read a; b := 0;
	 if (a>=0)
	   {bool c; c:=true; 
	    while(c) {b++; c:= a >= b*b}
	   };
     write (b-1)
   } 	
-} 
squareRoot :: Program
squareRoot = Block [("a",It),("b",It)]
                   [ Read "a", Assign "b" (Const (I 0))
                   , If (Op (Var "b") Ge (Const(I 0)))
                        (Block [("c", Bt)] 
                               [Assign "c" (Const (B True)),
                                While (Var "c")
                                 (Block []
                                   [(Incr "b"), 
                                    Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                   ])
                               ]
                        )
                   , Write (Op (Var "b") Minus (Const (I 1)))
                   ]

{- Ââîäèòü ö³ëå çíà÷åííÿ ,
   ÿêùî âîíî íåâ³ä"ºìíå, òî îá÷èñëþº â³äïîâ³äíå ÷èñëî 
   Ô³áîííà÷³ â çì³íí³é out ³ âèâîäèòü éîãî.
   Ïðè â³ä"ºìíîìó à âèâîäèòüñÿ 0.
   
   {int in, out; 
    read in; out := 0; 
	if (in>=0) 
      {int f0, f1, c; 
	   f0 := 1; f1 := 1; out := 1;
       if (in > 1)  
         for (c := 1; c < in; c++) 
		   {out := f0 + f1; f0 := f1; f1 := out}
	 };
    write out	 
  }
-}
fibonacci :: Program
fibonacci = 
    Block [("in",It), ("out",It)]
          [ Read "in",  Assign "out" (Const (I 0))
          , If (Op (Var "in") Gt (Const(I 1))) 
               (Block [("f0",It), ("f1",It), ("c",It)]
                     [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                      If (Op (Var "in") Gt (Const (I 1)))
                         (For (Assign "c" (Const (I 1)))
                             (Op (Var "c") Lt (Var "in")) 
                             (Incr "c")
                             (Block []
                                    [Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                    , Assign "f0" (Var "f1")
                                    , Assign "f1" (Var "out")
                                    ]
                              )
                         )
                     ])
          , Write (Var "out")
          ]




