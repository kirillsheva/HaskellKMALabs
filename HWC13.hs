{-# OPTIONS_GHC -Wall #-}
module Shevchenko13 where

import Text.ParserCombinators.Parsec

-- ������ 1 -----------------------------------------
fullBrace :: Parser()
fullBrace = spaces >> b >> eof
 
b :: Parser ()
b = f1 <|> f2 <|> f3 <|> spaces

balance  :: String -> Bool
balance str = either (const False) (const True) (parse fullBrace "" str)
   
-- ������ 2 ----------------------------------------- 
data Bexp = Bvalue Bool | Bvar Char | Not Bexp 
          | And Bexp Bexp | Or Bexp Bexp  deriving (Eq, Show)  

fullBe :: Parser Bexp 
fullBe = do{
  w <- parserw;
  eof;
  return w}

bexpd :: Parser Bexp
bexpd = symbol '(' *> parserw <* symbol ')'
        <|> do
            _ <- string "true"
            return (Bvalue True)
        <|> do 
            _ <- string "false"
            return (Bvalue False)
        <|> do 
            symbol '!'
            Not <$> bexpd
        <|> Bvar <$> letter
parserw :: Parser Bexp
parserw = chainl1 bexpc $ do
                    _ <- symbol '|'
                    return Or

bexpc :: Parser Bexp
bexpc = chainl1 bexpd $ do 
                    _ <- symbol '&'
                    return And

anBexp :: String -> Maybe Bexp
anBexp str = case (parse fullBe "" str) of
                Left _   ->  Nothing
                Right ex -> Just ex

-- ������ 3 ----------------------------------------- 
type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

fullXML :: Parser XML 
fullXML =  
  do {
  spaces;
  e <- elm;
  spaces;
  eof;
  return e}

anXML :: String -> Maybe XML
anXML str = case (parse fullXML "" str) of
               Left _    -> Nothing
               Right res -> Just res
xml :: Parser XML
xml = try $ elm <|> txt

----------------  ���� SPL  ------------   
data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- �����
         | Const Value     -- ���������
         | Op Exp Bop Exp  -- ��������
                 deriving (Show, Eq)

-- ������� (2-���������) ���������
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | Ba | Bo
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

{- ������� 
  symbol = ';' | '{' | '}' | '(' | ')' 
  identif=  char {digit | char}
  keyword= "int" | "bool" | "read" | "write" |"if" | "while" | "for" | "true" | "false"
  iden   =  identif .... not "int" "bool" "read" ""write" "if" "while" "for" "true" "false"
  number = digit { digit }.
  mulOp  = "*" | "/".
  addOp  = "+" | "-".
  relOp  = "<" | "<=" | ">" | ">=" | "==" 
  disOp  = "&" 
  conOp  = "|"
  typev  = "int" | "bool" 
-}
iden :: Parser String
iden = try( do {nm <- identif;
                if (any(nm==) ["int","bool","read","write","if","while","for","true","false"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm 
               } ) 

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop   
mulOp = (oper "*" Times) <|> (oper "/" Div)

disOp :: Parser Bop   
disOp = (oper "&" Ba)

conOp :: Parser Bop   
conOp = (oper "|" Bo)

-- ��������� �� "�������" ������� � ����		
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

--   :type Op -----> Exp -> Bop -> Exp -> Exp 
--   :type flip Op -------> Bop -> Exp -> Exp -> Exp         
expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
expOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum)) 

typev :: Parser Type 
typev = do {keyword "int"; return It}
        <|> do {keyword "bool"; return Bt} 

-- ������ 4 -----------------------------------------
identif :: Parser String
identif = do{
          a <- letter;
          be <- many $ letter<|>digit;
          return (a:be)}
     

number :: Parser Int
number  = read <$> (many1 $ digit)
 
addOp :: Parser Bop  
addOp =  try (oper "-" Minus) <|> try (oper "+" Plus)

relOp :: Parser Bop  
relOp = try (oper "<" Lt)
        <|> try (oper ">" Gt)
        <|> try (oper ">=" Ge)
        <|> try (oper "<=" Le) 
        <|> try (oper "==" Eql)

{- ������ 
  factor = '(' expr ')' | number | "true" | "false" | iden
  term   = factor { mulOp factor }
  relat  = term { addOp term }
  conj   = relat [relOp relat] 
  disj   = conj { conOp conj}   
  expr   = disj { disOp disj}
-}
factor :: Parser Exp
factor = do { symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Const (I nm))}
     <|> do {keyword "true"; return (Const (B True))}
     <|> do {keyword "false"; return (Const (B False))}
     <|> do {cs <- lexem iden; return (Var cs) }
     <?> "factor"

-- ������ 5 -----------------------------------------
term :: Parser Exp     
term = chainl1 factor $ expOp mulOp  

relat :: Parser Exp
relat = chainl1 term $ expOp addOp

conj :: Parser Exp
conj = chainl1 relat $ expOp relOp

disj :: Parser Exp
disj = chainl1 conj $ expOp conOp

expr :: Parser Exp
expr = chainl1 disj $ expOp disOp

{- ���������
  stmt   = "for" forSt | "while" whileSt | "if" ifSt 
         | "read" inSt | "write" outSt | iden assSt | blockSt  
  forSt  = '(' stmt ';' expr ';' stmt ')' stmt 
  whileSt= '(' expr ')' stmt 
  ifSt   = '(' expr ')' stmt 
  inSt   = iden 
  outSt  = expr
  assSt  = "++" | ":=" expr 
  blockSt= '{' {defin} listSt '}' 
  defin  = type listId ';'
  listId = iden {',' iden}
  listSt = stmt {';' stmt}  
  program= stmt eos 
-}   
stmt :: Parser Stmt 
stmt = do {keyword "for"; forSt}
       <|> do {keyword "while"; whileSt}
       <|> do {keyword "if"; ifSt}
       <|> do {keyword "read"; inSt}
       <|> do {keyword "write"; outSt}
       <|> do {var <- lexem iden; assignSt var}
       <|> blockSt
       <?> "statement"

-- ������ 6 -----------------------------------------
forSt :: Parser Stmt  
forSt   = do {symbol '('; te <- lexem $ stmt; symbol ';';at <-lexem $ expr; symbol ';'; ay <- lexem $ stmt; symbol ')';ar <-lexem $ stmt; return  (For te at ay ar)}

whileSt :: Parser Stmt               
whileSt = do{_ <- lexem $ char '(';wr <- lexem $ expr;_ <- lexem $ char ')';we <- lexem $ stmt;return $ While wr we}
              
ifSt :: Parser Stmt              
ifSt =  do 
          symbol '('
          idf <- expr
          symbol ')'
          fe <- stmt
          return (If idf fe)

assignSt :: String -> Parser Stmt 
assignSt var = try $ do {_ <- lexem  (string ":="); v1 <- lexem expr; return (Assign var v1)}
                 <|>do {_ <- lexem (string "++"); return (Incr var)} 

blockSt :: Parser Stmt
blockSt = do{
          _ <- lexem(char '{');
          x1 <- many defin;
          x2 <- lexem(listSt);
          _ <- lexem(char '}');
          return $ Block x1 x2} 

defin :: Parser (String, Type)
defin = do
    x1 <- lexem typev
    x2 <- lexem iden
    _ <- lexem (string ";")
    return (x2, x1) 


listSt :: Parser [Stmt]
listSt = do
    x1 <- lexem(stmt)
    x2 <- many (do {_ <- lexem(char ';'); lexem(stmt)})
    return (x1 : x2)
inSt :: Parser Stmt              
inSt    = undefined  

outSt :: Parser Stmt              
outSt    = undefined  
              
---------------------------------------------	
-- ������� �������
---------------------------------------------				
program :: Parser Stmt 
program = do {spaces; r <- stmt; eof; return r}

parseSPL :: String -> Either ParseError Program
parseSPL s = parse program "" s





name :: Parser Name
name = 
  do 
       lett <- letter 
       n <- many (digit <|> letter <|> oneOf ".-")
       return (lett:n)

slash :: Parser String
slash = many (noneOf "\"")

elm :: Parser XML
elm = (do 
          _ <- try (string "<")
          n <- name
          a <- try  (attribute)
          _ <- try  (string ">")
          x <- try (many xml)
          _ <-  try (string "</") 
          _ <- try (name)
          _ <- try (string ">")
          return (Element n a x))
fullValue :: Parser String
fullValue = do {
  symbol '"';
  val <- slash;
  symbol '"';
  return val}


f3 :: Parser ()
f3 = do
  symbol '['
  spaces
  b
  symbol ']'
  spaces
  b

f2 :: Parser ()
f2 = do
  symbol '{'
  spaces
  b
  symbol '}'
  spaces
  b

f1 :: Parser ()
f1 = do 
    symbol '('
    spaces
    b
    symbol ')'
    spaces 
    b

attribute :: Parser Attributes
attribute = many (do 
                spaces 
                num <- name
                spaces
                _ <- string "="
                spaces 
                val <- fullValue
                return (num,val))

txt :: Parser XML
txt = Text <$> (many1 $ noneOf "<>")
---------------------------------------------
--- ���� ��� ����������
--------------------------------------------- 
casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

squareRoot :: String
squareRoot =
   "{int a, b; \
   \ read a; b := 0; \
   \ if (a>= 0)\
   \    {bool c; c:=true; while(c) {b++; c:= a >= b*b}\
   \    };\
   \  write (b-1)\
   \ }"

squareRootAST :: Program
squareRootAST = Block [("a",It),("b",It)]
                   [ Read "a", Assign "b" (Const (I 0))
                   , If (Op (Var "a") Ge (Const(I 0))) 
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

fibonacci :: String
fibonacci = 
 " {int in, out; read in; out := 0; \n\
   \if (in>=0){int f0, f1,c; \n\
   \           f0 := 1; f1 := 1; out := 1; \n\
   \           if(in>1) \n \
   \              for (c := 1; c < in; c++) {\n\
   \                   out := f0 + f1; f0 := f1; f1 := out\n\
   \              }\n\
   \          }; \n\
   \write out \n\
  \}"
  
fibonacciAST :: Program
fibonacciAST = 
    Block [("in",It), ("out",It)]
          [ Read "in",  Assign "out" (Const (I 0))
          , If (Op (Var "in") Ge (Const(I 0))) 
               (Block [("f0",It), ("f1",It), ("c",It)]
                           [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                            Assign "out" (Const (I 1)),
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
                            ]
                )
          , Write (Var "out")
          ]

