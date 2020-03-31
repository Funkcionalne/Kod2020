module Utorok where

--import Terms
import Data.List
import Data.Char

-- identifikator premennej je String 
type Var = String

-- lambda termy
data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  deriving(Eq, Show)

-- porovnat dva termy, ci su rovnake rozcvicka
-- Napíšte do modulu Rovnake funkciu rovnake
-- modul Rovnake where
-- import TypyLExp
-- -- type Var = String
-- -- data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp
-- rovnake :: LExp -> LExp -> Bool
-- rovnake = ...
-- ktora porovna dva lambda termy,ci su rovnake.
-- Príklady:
-- rovnake (ID "x") (ID "x") vráti True
-- rovnake (ID "x") (ID "y") vráti False
-- rovnake (LAMBDA "x" (ID "x")) (LAMBDA "x" (ID "x")) vrati True
-- rovnake (LAMBDA "x" (ID "x")) (LAMBDA "y" (ID "y")) vrati False

rovnake :: LExp -> LExp -> Bool
rovnake (ID v1) (ID v2) = v1 == v2
rovnake (APP m1 n1) (APP m2 n2) = rovnake m1 m2 && rovnake n1 n2
rovnake (LAMBDA v1 b1) (LAMBDA v2 b2) = v1 == v2 && rovnake b1 b2
rovnake _ _ = False


-- nahradi premennu za premennu
nahrad ::  LExp -> String -> String -> LExp
nahrad (ID v) x y = ID (if v == x then y else v)
nahrad (APP m n) x y = (APP (nahrad m x y) (nahrad n x y))
nahrad t@(LAMBDA v b) x y = if x == v then t else (LAMBDA v (nahrad b x y))

{-
nahrad (LAMBDA "x" (APP (ID "x") (ID "x"))) "x" "y"
\x->(x x)
nahrad (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "z"
\x->(x z)
nahrad (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "x"
\x->(x x)
:-(
nahrad (LAMBDA "xx" (APP (ID "xx") (ID "y"))) "y" "x"
LAMBDA "xx" (APP (ID "xx") (ID "x"))
:-)
-}

free :: String -> LExp -> Bool
free x (ID v) = x == v
free x (APP m n) = free x m || free x n
free x (LAMBDA v b) = free x b && x /= v


-- nahradi premennu za premennu, druhy pokus
nahrad' ::  LExp -> String -> String -> LExp
nahrad' (ID v) x y = ID (if v == x then y else v)
nahrad' (APP m n) x y = (APP (nahrad' m x y) (nahrad' n x y))
nahrad' t@(LAMBDA v b) x y = if x == v then 
                                t 
                             else 
                                if free v (ID y) && free x b then
                                   let newv = v++v
                                       newb = nahrad' b v newv
                                   in 
                                       (LAMBDA newv (nahrad' newb x y))    
                                else
                                  (LAMBDA v (nahrad' b x y))


{-
"?: " nahrad' (LAMBDA "x" (APP (ID "x") (ID "x"))) "x" "y"
\x->(x x)
"?: " nahrad' (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "z"
\xx->(xx z)
"?: " nahrad' (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "x"
\xx->(xx x)
:-)
-}


alpha :: LExp -> LExp -> Bool
alpha (ID x) (ID y) = x == y
alpha (APP m1 n1) (APP m2 n2) = alpha m1 m2 && alpha n1 n2
alpha (LAMBDA v1 b1) (LAMBDA v2 b2) = alpha b1 (nahrad' b2 v2 v1)
alpha _ _ = False


priklady1 = [
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "x" (APP (ID "x") (ID "y")))  ,
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "y" (APP (ID "y") (ID "y")))  ,
  alpha (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "y" (APP (ID "z") (ID "y")))  ]

alphaEq :: LExp -> LExp -> Bool
alphaEq a b = alphaEq' a b (Just []) /= Nothing

-- data = Maybe t = Just t | Nothing
--alphaEq' :: LExp -> LExp -> [(Var,Var)]-> [(Var,Var)]

alphaEq' :: LExp -> LExp -> Maybe [(Var,Var)]-> Maybe [(Var,Var)]
alphaEq' _ _ Nothing = Nothing
alphaEq' (ID x) (ID y) subst@(Just pairs) = if elem (x,y) pairs then subst  
                                            else Just ((x,y):pairs)
alphaEq' (APP m1 n1) (APP m2 n2) subst = alphaEq' n1 n2 (alphaEq' m1 m2 subst)
alphaEq' (LAMBDA v1 b1) (LAMBDA v2 b2) subst@(Just pairs) = if v1 == v2 then alphaEq' b1 b2 subst
                                               else  alphaEq' b1 b2 (Just ((v1,v2):pairs))






priklady2 = [
  alphaEq (ID "x") (ID "x"),
  alphaEq (ID "x") (ID "y"), 
  alphaEq (APP (ID "x") (ID "y")) (APP (ID "x") (ID "z")),
  alphaEq (APP (ID "u") (ID "y")) (APP (ID "x") (ID "z")),
  alphaEq (APP (ID "x") (ID "x")) (APP (ID "x") (ID "y")),
  alphaEq (LAMBDA "x" (APP (ID "x") (ID "x"))) (LAMBDA "x" (APP (ID "x") (ID "y")))
  ]


{-
free "x" (LAMBDA "x" (APP (ID "x") (ID "x")))
free "y" (LAMBDA "x" (APP (ID "x") (ID "x")))
free "y" (LAMBDA "x" (APP (ID "x") (ID "y")))

free "y" (parse "\\x.(x x)")

-}
ypsilon = parse "\\f.((f (x x)) (f (x x)))"


parse  :: String -> LExp
parse str = e  where (e,_) = parser str

parser  :: String -> (LExp, String)
parser (x:xs)   | isAlpha x  = (ID [x], xs)
                    | x == '('  = let (e1, rest1) = parser xs in let (e2, rest2) = parser (tail rest1) in                         
                                                    ((APP e1 e2), tail rest2)
                    | x == '\\' = let (e, rest1) = parser (tail (tail xs)) in                       
                                                    ((LAMBDA [head xs] e), rest1)
parser  xs      = error ("syntax error: " ++ xs)                                                   
 





