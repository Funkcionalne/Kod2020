module Priprava_CV11 where

import Mparser

data LExp = LAMBDA String LExp | 
            ID String | 
            APL LExp LExp |
            CON String | CN Int
            deriving(Show, Read, Eq)

-- L -> (L L) | \id.L | id
lambda   :: Parser LExp
lambda   =  do { open; m <- lambda; space; n <- lambda; close; return (APL m n) }
            +++
            do { char '\\'; id <- identifier; char '.'; m <- lambda; return (LAMBDA id m) }
            +++
            do { id <- identifier; return (ID id) }

----------------------
-- B -> 0B | 1B | eps
-- B -> B0 | B1 | eps !!!! ZLE
binConst :: Parser Int
binConst = do { x <- binConst; char '0'; return (2*x) }
           +++
           do { x <- binConst; char '1'; return (2*x+1) }
           +++
           return 0

---------------------
-- S -> aSa | bSb | a | b | eps
-- 'a'..'z'

pali :: Parser String
pali = do { char 'a'; x <- pali; char 'a'; return ('a':(x++"a")) }
       `plus`
       do { char 'b'; x <- pali; char 'b'; return ('b':(x++"b")) }
       `plus`
       do { char 'a'; return ("a") }
       `plus`       
       do { char 'b'; return ("b") }
       `plus`       
       return ""
       
         
palindrom    :: Parser String
palindrom = foldr (\p -> \ps -> p `plus` ps) 
                  (return "")
                  (
                   [ do { char i; xs <- palindrom; char i; return ([i] ++ xs ++ [i]) } | i <- ['a'..'z']]
                    ++
                   [ do {c <- letter; return [c]} ]
                  )       
       
-----------------
morse   :: Parser String
morse   =   do { string ".-"; return "A"}
             `plus`
             do { string "-..."; return "B"}
            `plus`
             do { string "-.-."; return "C"}  
             `plus`
             do { string "-.."; return "D"}

-------

morseCodes = [(".-","A"), ("-...", "B"), ("-.-.","C"), ("-..","D"), (".","E"), ("..-.","F"),
              ("--.","G"), ("....","H"), ("..","I"), (".---","J"), ("-.-","K"), (".-..","L"), 
              ("--","M"), ("-.","N"), ("---","O"), (".--.","P"), ("--.-","Q"), (".-.","R"),
              ("...","S"), ("-","T"), ("..-","U"), ("...-","V"), (".--","W"), ("-..-","X"),
              ("-.--","Y"), ("--..","Z")
           -- ,("-----","0"), (".----","1"), ("..---","2"), ("...--","3"), ("....-","4"),
           -- (".....","5"), ("-....","6"), ("--...","7"), ("---..","8"), ("----.","9")
             ]
morse2    :: Parser String
morse2 =  foldr (\p -> \ps -> p `plus` ps) 
               (return "")
               [ do { string' code; xs <- morse2; return (chr ++ xs)} | (code, chr) <- morseCodes] 
