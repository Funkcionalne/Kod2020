module Lopaty1 where

import Text.ParserCombinators.Parsec 

----------------------------------------------
run  :: Show a => Parser a -> String -> IO()
run p input = case (parse p "" input) of
      Left err -> do { putStr "parse error at" ; print err }    
      Right x -> print x


--L -> e|(L1|[L2|{L3 ...
--L1 -> [L12|{L13|)L
-- ...
--nazov neterminalu hovori ktore lopaty su momentalne pozicane
--napr. L123 znamena, ze lopaty 1,2,3 su pozicane.

lopaty :: Parser String
lopaty = do{ char '('; x<-lopaty1; return ("("++x) } <|>
         do{ char '['; x<-lopaty2; return ("["++x) } <|>
         do{ char '{'; x<-lopaty3; return ("{"++x) } <|> 
         do{ oneOf ")]}";  fail "parser lopaty" } <|>
         return ""
         
         
lopaty1 :: Parser String
lopaty1 = do{ char ')'; x<-lopaty; return (")"++x) } <|>
          do{ char '['; x<-lopaty12; return ("["++x) } <|>
          do{ char '{'; x<-lopaty13; return ("{"++x) } <|> 
          do{ oneOf "(]}";  fail "parser lopaty1" } 

lopaty2 :: Parser String
lopaty2 =  do{ char ']'; x<-lopaty; return ("]"++x) } <|> 
           do{ char '('; x<-lopaty12; return ("("++x) } <|>
           do{ char '{'; x<-lopaty23; return ("{"++x) } <|>
           do{ oneOf "[)}";  fail "parser lopaty2" } 

lopaty3 =  do{ char '}'; x<-lopaty; return ("}"++x) } <|> 
           do{ char '('; x<-lopaty13; return ("("++x) } <|>
           do{ char '['; x<-lopaty23; return ("["++x) } <|>
           do{ oneOf "{)]";  fail "parser lopaty3" } 
           
lopaty12 :: Parser String                    
lopaty12 = do{ char ']'; x<-lopaty1; return ("]"++x) } <|> 
           do{ char ')'; x<-lopaty2; return (")"++x) } <|>
           do{ char '{'; x<-lopaty123; return ("{"++x) } <|>
           do{ oneOf "[(}";  fail "parser lopaty12" } 
           
lopaty13 :: Parser String                    
lopaty13 = do{ char '}'; x<-lopaty1; return ("}"++x) } <|> 
           do{ char ')'; x<-lopaty3; return (")"++x) } <|>
           do{ char '['; x<-lopaty123; return ("["++x) } <|>
           do{ oneOf "{(]";  fail "parser lopaty13" } 
         
lopaty23 :: Parser String                    
lopaty23 = do{ char ']'; x<-lopaty3; return ("]"++x) } <|> 
           do{ char '}'; x<-lopaty2; return ("}"++x) } <|>
           do{ char '('; x<-lopaty123; return ("("++x) } <|>
           do{ oneOf "[{)";  fail "parser lopaty23" } 

lopaty123 :: Parser String                    
lopaty123 = do{ char ']'; x<-lopaty13; return ("]"++x) } <|> 
            do{ char ')'; x<-lopaty23; return (")"++x) } <|>
            do{ char '}'; x<-lopaty12; return ("}"++x) } <|>
            do{ oneOf "[({";  fail "parser lopaty123" } 

t1 = run lopaty "()"
t2 = run lopaty "([)]"
t3 = run lopaty "[(])"
t4 = run lopaty "][()"
t5 = run lopaty ")("
t6 = run lopaty "["
t7 = run lopaty "(("
t8 = run lopaty "(())"
t9 = run lopaty "{]"

tA = run lopaty "([{])}"
tB = run lopaty "([}{])"
tC = run lopaty "([{)}]"
tD = run lopaty "([{)}]]"
tE = run lopaty "([{)}]"
