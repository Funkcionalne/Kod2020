module Lopaty3 where

import Text.ParserCombinators.Parsec 
----------------------------------------------
run  :: Show a => Parser a -> String -> IO()
run p input = case (parse p "" input) of
      Left err -> do { putStr "parse error at" ; print err }    
      Right x -> print x

lopaty = lop (0,0,0)

lop :: (Int, Int, Int) -> Parser String
lop (a,b,c) = do {
                    char '(';
                    if a==0 then do { x <- lop (1,b,c); return ("("++x); }
                    else fail "(";
                }<|> do {
                    char ')';
                    if a==1 then do {
                    x <- lop (0,b,c);
                    return (")"++x); }
                    else fail ")";
                }<|> do {
                    char '[';
                    if b==0 then do {
                    x <- lop (a,1,c);
                    return ("["++x); }
                    else fail "[";
                }<|> do {
                    char ']';
                    if b==1 then do {
                    x <- lop (a,0,c);
                    return ("]"++x); }
                    else fail "]";
                }<|> do {
                    char '{';
                    if c==0 then do {
                    x <- lop (a,b,1);
                    return ("{"++x); }
                    else fail "{";
                }<|> do {
                    char '}';
                    if c==1 then do {
                    x <- lop (a,b,0);
                    return ("}"++x); }
                    else fail "}";
                }<|> 
                do {
                    if a== 0 && b == 0 && c == 0 then return ("") else fail ""
                }

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
