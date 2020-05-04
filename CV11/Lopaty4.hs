module Lopaty where

import Text.ParserCombinators.Parsec 

run  :: Show a => Parser a -> String -> IO()
run p input = case (parse p "" input) of
      Left err -> do { putStr "parse error at" ; print err }    
      Right x -> print x

lopata' :: Parser (String, Int, Int, Int)
lopata' = do {  char '('
             ; (xs,g,h,k) <- lopata'
             ; if g == 0 then fail "chyba (" else return ('(':xs, 0, h, k)
            }
         <|>
         do {  char '['
             ; (xs,g,h,k) <- lopata'
             ; if h == 0 then fail "chyba [" else return ('[':xs, g, 0, k)
            }
         <|>
         do {  char '{'
             ; (xs,g,h,k) <- lopata'
             ; if k == 0 then fail "chyba {" else return ('{':xs, g, h, 0)
            }
         <|>
         do {  char ')'
             ; (xs,g,h,k) <- lopata'
             ; if g /= 0 then fail "chyba )" else return (')':xs, 1, h, k)
            }
         <|>
         do {  char ']'
             ; (xs,g,h,k) <- lopata'
             ; if h /= 0 then fail "chyba ]" else return (']':xs, g, 1, k)
            }
         <|>
         do {  char '}'
             ; (xs,g,h,k) <- lopata'
             ; if k /= 0 then fail "chyba }" else return ('}':xs, g, h, 1)
            }
         <|>
         return ("", 0, 0, 0)

lopata :: Parser String
lopata = do { (xs,g,h,k) <- lopata'; 
                if g == 0 && h == 0 && k == 0 then return xs else fail " nesedi to"
            }  

t1 = run lopata "()"
t2 = run lopata "([)]"
t3 = run lopata "[(])"
t4 = run lopata "][()"
t5 = run lopata ")("
t6 = run lopata "["
t7 = run lopata "(("
t8 = run lopata "(())"
t9 = run lopata "{]"

tA = run lopata "([{])}"
tB = run lopata "([}{])"
tC = run lopata "([{)}]"
tD = run lopata "([{)}]]"
tE = run lopata "([{)}]"
