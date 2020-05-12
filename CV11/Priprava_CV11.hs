module Priprava_CV11 where

import Mparser

data LExp = LAMBDA String LExp | 
            ID String | 
            APL LExp LExp |
            CON String | CN Int
            deriving(Show, Read, Eq)

-- L -> (L L) | \id.L | id
lambda   :: Parser LExp
lambda   =  do { id <- identifier; return (ID id) }
            +++
            do { integ <- integer; return (CN integ) }
            +++
            do { open; m <- lambda; space; n<-lambda; close; return (APL m n) }
            +++
            do { char '\\'; id<-identifier; char '.'; m<-lambda; return (LAMBDA id m) }

-- B -> 0B | 1B | eps
binConst :: Parser Int
binConst = do { char '0'; x <- binConst; return (2*x) }
           +++
           do { char '1'; x <- binConst; return (2*x+1) }
           +++
           return 0
           
-----------------
morse   :: Parser String
morse   =   (char '.' >>= \_ -> char '.' >>= \_ -> return "A")
            `plus`
            (char '.' >>= \_ -> char '-' >>= \_ -> return "B")
            `plus`
            (char '.' >>= \_ -> return "C")
            `plus`
            (char '-' >>= \_ -> return "D")

morse1   :: Parser String
morse1   =   do { string ".-"; return "A"}
             `plus`
             do { string "-..."; return "B"}
            `plus`
             do { string "-.-."; return "C"}  
             `plus`
             do { string "-.."; return "D"}

-------
-- S -> aSa | bSb | a | b 

pali :: Parser String
pali = 
       do { char 'a'; x <- pali; char 'a'; return ('a':(x++['a'])) }
       `plus`
       do { char 'b'; x <- pali; char 'b'; return ('a':(x++['b'])) }
       `plus`
       do { char 'a'; return "a" }
       `plus`
       do { char 'b'; return "b" }
       `plus`
       return ""


pali1 :: Parser String
pali1 = 
       do { ch <- item; x <- pali1; char ch; return (ch:(x++[ch])) }
       `plus`
       do { char 'a'; return "a" }
       `plus`
       do { char 'b'; return "b" }
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
               [ do { string' code; xs <- morse; return (chr ++ xs)} | (code, chr) <- morseCodes] 

             
             
morseAlphabetScislami = [ (".-","A"),("-...","B"),("-.-.","C"),("-..","D"),(".","E"),("..-.","F"),("--.","G"),("....","H"),("..","I"),(".---","J"),("-.-","K"),(".-..","L"),("--","M"),("-.","N"),("---","O"),(".--.","P"),("--.-","Q"),(".-.","R"),("...","S"),("-","T"),("..-","U"),("...-","V"),(".--","W"),("-..-","X"),("-.--","Y"),("--..","Z"), (".----","1"),("..---","2"),("...--","3"),("....-","4"),(".....","5"),("-....","6"),("--...","7"),("---..","8"),("----.","9"),("-----","0")]

morseAlphabet = [ (".-","A"),("-...","B"),("-.-.","C"),("-..","D"),(".","E"),("..-.","F"),("--.","G"),("....","H"),("..","I"),(".---","J"),("-.-","K"),(".-..","L"),("--","M"),("-.","N"),("---","O"),(".--.","P"),("--.-","Q"),(".-.","R"),("...","S"),("-","T"),("..-","U"),("...-","V"),(".--","W"),("-..-","X"),("-.--","Y"),("--..","Z")]             