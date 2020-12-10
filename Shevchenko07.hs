{-# OPTIONS_GHC -Wall #-}
module Shevchenko07 where

import Data.Char(isSpace, isDigit, isLetter) 

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Çàäà÷à 1 -----------------------------------------
spaces :: String -> String
spaces = dropWhile (isSpace)  
-- Çàäà÷à 2 ----------------------------------------- 
manyT, value, manyN :: String -> (String,String)
n:: Char -> Bool
v:: Char -> Bool
t:: Char -> Bool

manyT ss = (takeWhile t ss, dropWhile t ss) 
t x = x /= '<' && x /= '>'
value ss = (takeWhile v ss, dropWhile v ss)    
v x = x /= '"'                  
manyN ss = (takeWhile n ss, dropWhile n ss) 
n x = x == '.'||isDigit x || isLetter x || x == '-'

-- Çàäà÷à 3 -----------------------------------------
name, text, fullValue :: String ->  Maybe(String,String) 
name s =
  case manyN s of
    ("", _) -> Nothing
    (lex, str) -> Just (lex, str)
text s =
  case manyT s of
    ("", _) -> Nothing
    (lex, str) -> Just (lex, str)
fullValue ('"':str) =
  case value str of
    (lex, ('"':r)) -> Just (lex, r) 
    _ -> Nothing


-- Çàäà÷à 4 -----------------------------------------
attrib :: String -> Maybe ((String, String), String)
attrib str =
  case name (spaces str) of
    Just (_, "") -> Nothing
    Just (atn, rest) -> case value rest of    
        (eq, other) -> case spaces eq of     
            ('=':_) -> case fullValue other of
                Just (atv, r) -> Just ((atn, atv), r)
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing

manyAtt :: String -> Maybe (Attributes,String)
manyAtt s = Just (f s)

f :: String -> (Attributes, String)
f s = case attrib s of
    Just (a, str) -> (a:attr, lefted)
     where (attr, lefted) = f str
    Nothing -> ([], s)

-- Çàäà÷à 5 -----------------------------------------
begTag :: String -> Maybe ((String, Attributes), String)
begTag string = case spaces string of
    ('<':rest) -> case name rest of
        Just (nm, other) -> case spaces other of
            ('=':_) -> Nothing
            _ -> case manyAtt other of
                Just (attr, r) ->Just ((nm, attr), tail (spaces r))
                _ -> Nothing
        _ -> Nothing
    _ -> Nothing

endTag :: String -> Maybe (String, String)
endTag string =
  case spaces string of
    ('<':'/':other) ->case name other of
        Just (nm, r) ->case spaces r of
            '>':rem -> Just (nm, rem)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
-- Çàäà÷à 6 -----------------------------------------
element :: String -> Maybe (XML,String) 
element s = case begTag s of
            Just ((nm, attr), other) -> case manyXML other of
                Just (someXML, lef) -> case endTag lef of
                    Just(end, st) -> if nm == end then Just ((Element nm attr someXML), st) else Nothing
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing

xml :: String -> Maybe (XML,String)
xml st = case element st of
         Just (xs, rst1) -> Just (xs, rst1)
         Nothing -> case text st of
                    Just (tx, rst2) -> Just (Text tx, rst2)
                    _ -> Nothing 

manyXML :: String -> Maybe ([XML],String)
manyXML s | null xmls = 
  if endTag st == Nothing then Nothing else Just (xmls, st)           
    | otherwise = case last xmls of
                  Text _  ->  if endTag st == Nothing then Nothing else Just (xmls, st)
                  _  -> Just (xmls, st)
    where (xmls, st) = fxml s

fxml :: String -> ([XML], String)
fxml s = case xml s of
            Just (x, lefted) -> (x:xmls, other)
             where (xmls, other) = fxml lefted
            _ -> ([], s)

-- Çàäà÷à 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML s = case element (spaces s) of  
            Just (xm,s1) -> if null (spaces s1) then Just xm else Nothing 
            Nothing -> Nothing  

-- Òåñòîâ³ äàí³ -------------------------------------------
-- Ïðîñò³ òåñòè XML-îá'ºêò³â (áåç ïðîì³æê³â)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 

-- Ðåçóëüòàòè àíàë³çó ïîïåðåäí³õ XML-îá'ºêò³â
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

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
