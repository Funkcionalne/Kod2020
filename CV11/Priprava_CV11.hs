module Priprava_CV11 where

import Mparser

data LExp = LAMBDA String LExp | 
            ID String | 
            APL LExp LExp |
            CON String | CN Int
            deriving(Show, Read, Eq)

lambda   :: Parser LExp
lambda   =  do { id <- identifier; return (ID id) }
            +++
            do { integ <- integer; return (CN integ) }
            +++
            do { open; m <- lambda; space; n<-lambda; close; return (APL m n) }
            +++
            do { char '\\'; id<-identifier; char '.'; m<-lambda; return (LAMBDA id m) }

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

palindrom    :: Parser String
palindrom = foldr (\p -> \ps -> p +++ ps) 
                  (return "")
                  (
                   [ do { char i; xs <- palindrom; char i; return ([i] ++ xs ++ [i]) } | i <- ['0'..'9']]
                    ++
                   [ do {c <- digit; return [c]} ]
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