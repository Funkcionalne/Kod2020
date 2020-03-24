module Parser where 

import Terms
import Data.Char
import Data.List

instance Show LExp where 
    show (ID v)  = v
    show (LAMBDA v b) = "\\" ++ v ++ "." ++ (show b)
    show (APP left right) = "(" ++ (show left) ++ " " ++ (show right) ++ ")"
-- type Var = String
-- data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  deriving(Eq)

{- 
    zjednodusujuce predpoklady: 
    - premenna je jednopismenkova...
    - aplikacia (M N)   -- medzera je prave jedna 
    - abstrakcia \\x.B  -- pozor na dvojite lomitko, v retazci...
-}
parser :: String -> (LExp, String)
parser (x:xs) | isAlpha x = (ID [x], xs)                        -- pozor x :: Char
              | x == '\\'  = let (variable:bodka:bodyrest) = xs           -- \x.B
                                 (body, rest) = parser bodyrest 
                                 in (LAMBDA [variable] body, rest)
              | x == '('   = let (left, medzera:rest1) = parser xs       
                                 (right, rpar:rest2) = parser rest1  
                                 in (APP left right, rest2)
                                
lt1 = parser "x"
lt2 = parser "(x x)"
omega = parser "\\x.(x x)"
omegaNaOmega = parser "(\\x.(x x) \\x.(x x))"
omegaNaOmegaPlusNieco = parser "(\\x.(x x) \\x.(x x))y"