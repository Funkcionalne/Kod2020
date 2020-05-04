module Lopaty2 where

import Text.ParserCombinators.Parsec 
----------------------------------------------
run  :: Show a => Parser a -> String -> IO()
run p input = case (parse p "" input) of
      Left err -> do { putStr "parse error at" ; print err }    
      Right x -> print x

lopata3 = "{}"
lopata2 = "[]"
lopata1 = "()"

preprocess  xs = 
                (filter ( `elem` lopata1) xs) 
                ++ 
                (filter ( `elem` lopata2) xs) 
                ++ 
                (filter ( `elem` lopata3) xs)

parselopata3 :: Parser String        -- "{}"
parselopata3    = string lopata3
parselopata2 :: Parser String        -- "[]"
parselopata2    = string lopata2
parselopata1 :: Parser String        -- "()"
parselopata1    = string lopata1

-- treba docitat cely vstup...
lopataParser = sequence [many parselopata1, many parselopata2, many parselopata3] <|> fail "error" 
lopata = run lopataParser . preprocess
               
prva = lopata "([{][][}{][)(][}{][][}{][)(][}{][)(][}{][)(}{)(}{][)}]"
druha = lopata "([{][][][)(}{)(][][][}{)()(}{)(][][)(}{}{][}{)(][}])"
tretia = lopata "([{][)(][}{)(][}{][}{)(}{)(][}{}{)(][}{)(]})"


t1 = lopata "()"
t2 = lopata "([)]"
t3 = lopata "[(])"
t4 = lopata "][()"
t5 = lopata ")("
t6 = lopata "["
t7 = lopata "(("
t8 = lopata "(())"
t9 = lopata "{]"

tA = lopata "([{])}"
tB = lopata "([}{])"
tC = lopata "([{)}]"
tD = lopata "([{)}]]"
tE = lopata "([{)}]"


