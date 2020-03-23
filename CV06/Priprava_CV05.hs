module Cvicenie5 where

import Data.List
import Data.Char
import Terms

instance Show LExp where
  show (LAMBDA v e) = '\\' : v ++ "->" ++ show e
  show (ID v) = v
  show (APP e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  
--foldLambda :: (String -> LExp -> LExp) -> (String -> LExp) -> (LExp -> LExp -> LExp) -> LExp -> LExp
foldLambda :: (String -> t -> t) -> (String -> t) -> (t -> t -> t) -> LExp -> t
foldLambda lambda var apl (LAMBDA str exp)  = lambda str (foldLambda lambda var apl exp)
foldLambda lambda var apl (ID str)          = var str
foldLambda lambda var apl (APP exp1 exp2)   = apl (foldLambda lambda var apl exp1) 
                                                  (foldLambda lambda var apl exp2)

vars  :: LExp -> [String]
vars = foldLambda (\x -> \y->y) (\x->[x]) (++) 

show' :: LExp -> String
show' = foldLambda (\x y->"(\\"++x++"->"++y++")") (\x->x) (\x y->"("++x++" "++y++")") 
                                                  
ione =    (APP isucc izero)
itwo =    (APP isucc (APP isucc izero))
ifour =   (APP isucc (APP isucc (APP isucc (APP isucc izero))))
ieight =  (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc izero))))))))
--ithree =  (APP (APP iplus itwo) ione)
--inine =   (APP (APP itimes ithree) ithree)
--isixteen = (APP (APP ipower itwo) ifour)

izero = (LAMBDA "f" (LAMBDA "x" (ID "x")))
omega = (LAMBDA "x" (APP (ID "x") (ID "x")))
isucc = (LAMBDA "n" 
          (LAMBDA "f" 
            (LAMBDA "x" (APP (ID "f") (APP (APP (ID "n") (ID "f")) (ID "x"))) )))
-- iplus =  fromString "?m.?n.?f.?x.((m f) ((n f) x))" 
-- itimes = fromString "?m.?n.?f.?x.((m (n f)) x)"
-- ipower = fromString "?m.?n.(n m)"  

-- najst vsetky podtermy termu
-- priamociaro
podtermy :: LExp -> [LExp]
podtermy = nub . podtermy'

podtermy' :: LExp -> [LExp]
podtermy' t@(LAMBDA v e1) = t : podtermy' e1
podtermy' t@(APP e1 e2) = t : (podtermy' e1 ++ podtermy' e2)
podtermy' t@(ID _) = [t]

{--
podtermy (LAMBDA "x" (APP (ID "x") (ID "x")))
[\x->(x x),(x x),x]
--}

-- akumulatorom
podtermy1 :: LExp -> [LExp]
podtermy1 = nub . (podtermy1' [])

podtermy1' :: [LExp] -> LExp -> [LExp]
podtermy1' vys (LAMBDA v e1) = podtermy1' (e1:vys) e1
podtermy1' vys (APP e1 e2) = podtermy1' (podtermy1' vys e2) e1
podtermy1' vys v = v:vys

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
 
-- riesenie rozcvicky
rovnake :: LExp -> LExp -> Bool
rovnake (LAMBDA v1 e1) (LAMBDA v2 e2) = (v1 == v2) && rovnake e1 e2
rovnake (ID v1) (ID v2) = v1 == v2
rovnake (APP e11 e21) (APP e12 e22) = (rovnake e11 e12) && (rovnake e12 e22)
rovnake _ _ = False

----

fromString  :: String -> (LExp, String)
fromString (x:xs)   | isAlpha x  = (ID [x], xs)
                    | x == '('  = let (e1, rest1) = fromString xs in let (e2, rest2) = fromString (tail rest1) in                         
                                                    ((APP e1 e2), tail rest2)
                    | x == '\\' = let (e, rest1) = fromString (tail (tail xs)) in                       
                                                    ((LAMBDA [head xs] e), rest1)
fromString  xs      = error ("syntax error: " ++ xs)                                                   
 
