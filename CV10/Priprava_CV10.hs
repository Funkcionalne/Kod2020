module Cvicenie08  where

import Prelude hiding ((<*>), (<|>), (*>), (<*), sequence)
import Data.Char

type Parser symbol result = [symbol] -> [([symbol],result)]

symbola :: Parser Char Char
symbola [] = [] -- ak nie je nič na vstupe
symbola (x:xs) | x=='a' = [ (xs, 'a') ] -- ak je 'a' na vstupe
               | otherwise= [] -- ak nie je 'a' na vstupe

-- to iste ale parametrizovane
symbol :: Eq s => s -> Parser s s
symbol a [] = []
symbol a (x:xs) | a==x = [ (xs, x) ]
                | otherwise= []

-- inak zapisane
symbol' :: Eq s => s -> Parser s s
symbol' a [] = []
symbol' a (x:xs) = [ (xs, a) | a == x ]

-- odreze prefix ak je rovnaky
token :: Eq s => [s] -> Parser s [s]
token k xs | k == take n xs = [ (drop n xs, k)]
           | otherwise = []
   where n = length k

k0 = []
k1 = ["kuk"]
k2 = ["ahoj", "kuk"]
k3 = ["ahoj"]

k4 = "kuk"
k5 = "ku"
k6 = "u"

--vseobecnejsi symbol, 
satisfy :: (s -> Bool) -> Parser s s
satisfy p [] = []
satisfy p (x:xs) = [ (xs, x) | p x ]

------------------------------------------------------------- Cvicenia z prednasky
--symbol inak, cv. 1.
symbol'' a = satisfy (\x->x==a)
symbol''' a = satisfy (==a)

--cv. 2, a 3.
digit10 :: Parser Char Char
digit10 = satisfy isDigit

hexa :: Parser Char Char
hexa = satisfy (\x->elem x "0123456789ABCDEF")

hexa' = satisfy (\x-> isDigit x || elem x ['A'..'F'])
hexa'' =satisfy isHexDigit
-------------------------------------------------
epsilon :: Parser s () -- () je ako typ void
epsilon xs = [ ( xs, () ) ] -- () hodnota typu (), ako null

failp :: Parser s r
failp xs = []

succeed :: r -> Parser s r
succeed v xs = [ (xs, v) ]

infixr 6 <*> -- sekvenčné zreťazenie analyzátorov
infixr 4 <|>

(<*>) :: Parser s a -> Parser s b -> Parser s (a,b)
(p1 <*> p2) xs = [ (xs2, (v1,v2))
                    | (xs1, v1) <- p1 xs,
                      (xs2, v2) <- p2 xs1
                 ]
(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs = p1 xs ++ p2 xs

-- cv.4
-- lebo zlozitost (++) je umerna dlzke prveho argumentu

-- cv.5
yes = token "YES"
no = token "NO"
yesno = yes <|> no

-- toto asi nie, kvoli typom
-- no' = symbol 'N' <*> symbol 'O' -- <*> succeed "NO"
-- yes' = symbol 'Y' <*> symbol 'E' <*> symbol 'S' -- <*> succeed "YES"
-- yesno' = yes' <|> no'

sp :: Parser Char a -> Parser Char a
sp p = p . dropWhile (==' ')

just :: Parser s a -> Parser s a
just p = filter (null.fst) . p

------- 
-- Cv. 5'
justYESNO = just yesno

--cv.6
just' p xs = [x | x <- p xs, null $ fst x]

infix 5 <@ -- infixová notácia pre aplikátor
(<@) :: Parser s a -> (a->b) -> Parser s b
(p <@ f) xs = [ (ys, f v) | (ys, v) <- p xs ]

infixr 6 <*
(<*) :: Parser s a -> Parser s b -> Parser s a
p <* q = p <*> q <@ fst
 
infixr 6 *>
(*>) :: Parser s a -> Parser s b -> Parser s b
p *> q = p <*> q <@ snd

list :: (a, [a]) -> [a]
list (x, xs) = x:xs

infixr 6 <:*>
(<:*>) :: Parser s a -> Parser s [a] -> Parser s [a]
p <:*> q = p <*> q <@ list

-- cv 7.
infixr 6 <:**>
(<:**>) :: Parser s a -> Parser s [a] -> Parser s [a]
-- p <:**> q = p <*> q  <@ (\x->(fst x):(snd x))
-- alternativa, kto pozna uncurry
p <:**> q = p <*> q <@ uncurry (:)
-- alebo aj takto
-- p <:**> q xs = [(ys, z:zs) | (xs2,z) <- p xs, (ys, zs) <- q xs2]

single :: a -> [a]
single x = [x]
single' = \x -> [x]

-- {p}
option :: Parser s a -> Parser s [a]
option p = p <@ single <|> succeed []

-- {p}^*
-- manyp -> epsilon | P manyP
many :: Parser s a -> Parser s [a]
many p = p <:*> (many p) <|> succeed []

-- {p}^+
-- many1p -> P manyP
many1 :: Parser s a -> Parser s [a]
many1 p = p <:*> many p

sequence :: [Parser s a] -> Parser s [a]
sequence = foldr (<:*>) (succeed [])

--cv 8.
sequence' [] = succeed []
sequence' (x:xs) = x <:*> (sequence' xs)

--cv 8', v subore s kodom z prednasky
token' :: Eq s => [s] -> Parser s [s]
token' str = sequence (map symbol str)

-- rafinovanejsie
token'' :: Eq s => [s] -> Parser s [s]
token'' = sequence' . map symbol

--cv 9.
mobileNumber  :: Parser Char [Char]
mobileNumber = just $ sequence' ([symbol '0', symbol '9'] ++ (replicate 8 digit10))

psc  :: Parser Char [Char]
psc = just $ sequence' ((replicate 3 digit10) ++ [symbol ' '] ++ (replicate 2 digit10))

choice :: [Parser s a] -> Parser s a
choice = foldr (<|>) failp

-- cv. 10
year  :: Parser Char [Char]
year = sequence' (replicate 4 digit10)

month  :: Parser Char (Char,Char)
month = symbol '0' <*> digit10
        <|>
        symbol '1' <*> (symbol '0' <|> symbol '1' <|> symbol '2')
        
month'  :: Parser Char [Char]
month' = sequence' [ symbol '0', digit10]
         <|>
         sequence' [symbol '1', (symbol '0' <|> symbol '1' <|> symbol '2') ]        

day  :: Parser Char (Char,Char)
day = symbol '0' <*> digit10
        <|>
        symbol '1' <*> digit10
        <|>
        symbol '2' <*> digit10
        <|>
        symbol '3' <*> (symbol '0' <|> symbol '1')
        
day'  :: Parser Char [Char]
day' = sequence' [ (symbol '0' <|> symbol '1' <|> symbol '2'), digit10]
        <|>
         sequence' [symbol '3', (symbol '0' <|> symbol '1') ]
         
day''  :: Parser Char [Char]
day'' = sequence' [ choice [symbol '0' , symbol '1' , symbol '2' ], digit10]
        <|>
         sequence' [symbol '3', (symbol '0' <|> symbol '1') ]         
         
--------------------
-- Determinsm


determ :: Parser a b -> Parser a b
determ p xs  |  null r     =  []
             |  otherwise  =  [head r]
                     where r = p xs

compulsion = determ . option

greedy = determ . many

greedy1 = determ . many1
  
-- Some common special cases


identifier  ::  Parser Char String
identifier   =  satisfy isAlpha <:*> greedy (satisfy isAlphaNum)

digit       ::  Parser Char Int
digit        =  satisfy isDigit  <@  f
         where  f c = ord c - ord '0'


natural     ::  Parser Char Int
natural      =  greedy1 digit  <@  foldl f 0
         where  f a b = a*10 + b

integer     ::  Parser Char Int
integer      =  option (symbol '-') <*> natural  <@  f
         where  f ([],n) =  n
                f (_ ,n) =  -n
                
--------------------
-- cv. 11         
natural'  :: Parser Char Int
natural' = (many digit) <@ foldr (\y -> \x -> 10*x + y) 0 . reverse
         
         
-- cv.12

(<?@)        ::  Parser s [a] -> (b,a->b) -> Parser s b
p <?@ (b,g)   =  p <@ f
          where  f []  = b
                 f [h] = g h               
  
{-
"?: " fixed "12.3456"
[("",12.3456),(".3456",12.0)]
-}

fixed       ::  Parser Char Float
fixed        =  (integer <@ fromIntegral)
                <*> 
                (option (symbol '.' *> fractpart)  <?@  (0.0,id))
                <@  uncurry (+)

{-
"?: " fractpart "3123123"
[("",0.3123123)]
-}

fractpart   ::  Parser Char Float
fractpart    =  greedy digit  <@  foldr f 0.0
         where  f d n = (n +  fromIntegral d)/10.0
       
{-
"?: " fixed "12.3456"
[("",12.3456),(".3456",12.0)]
-}
float       ::  Parser Char Float
float        =  fixed 
                <*> 
                (option (symbol 'E' *> integer) <?@ (0,id) )
                <@ f
         where  f (m,e)  =  m * power e
                power e | e<0       = 1.0 / power (-e)
                        | otherwise = fromInteger(10^e)
         
         
         

parserV :: Parser Char [Char]
parserV = ( symbol 'a' *> parserV <* symbol 'a' ) <@ (\x -> ('a':(x ++ "a")))
          <|>
          succeed []
                          
justparserV :: Parser Char [Char]
justparserV = just $ parserV
                                          
--------------------
{-- zle riesenie}               
parserS :: Parser Char [Char]
parserS = ( parserS <*> parserS ) <@ (\(x,y) -> (x++y))
          <|>
          succeed []
---}

parserS :: Parser Char [Char]
parserS = ( symbol 'a' <*> parserS ) <@ (\(x,y) -> (x : y))
          <|>
          succeed []

          
justparserS :: Parser Char [Char]
justparserS = just $ parserS

--------------------
                
parserR :: Parser Char [Char]
parserR = (
            symbol 'a' <*> parserR <*> symbol 'a'
            <|>
            symbol 'b' <*> parserR <*> symbol 'b'
            <|>
            symbol 'c' <*> parserR <*> symbol 'c'
        ) <@ (\(x, (y,z)) -> (x:(y ++ [z])))
          <|>
          succeed []
                          
justparserR :: Parser Char [Char]
justparserR = just $ parserR
                          