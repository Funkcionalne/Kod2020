module Cvicenie04 where

data Exp = Const Int                -- konstanta
          | Var String                -- premenna
          | Add Exp Exp                -- plus
          | Sub Exp Exp
          | Mul Exp Exp
      deriving (Eq, Read)

instance Show Exp where
    show (Const x) = show x
    show (Var x) = x
    show (Add l p)  = "(" ++ (show l) ++ "+" ++ (show p) ++ ")"
    show (Sub l p) = "(" ++ (show l) ++ "-" ++ (show p) ++ ")"
    show (Mul l p) = "(" ++ (show l) ++ "*" ++ (show p) ++ ")"
        
-- (x+x) * (x-1) + x
e1 :: Exp        
e1 = Add 
        (Mul (Add (Var "x")(Var "x") ) (Sub (Var "x")(Const 1))        )
        (Var "x")

-- (x+x) * (x-1)
e2 :: Exp        
e2 = Mul (Add (Var "x") (Var "x")) (Sub (Var "x") (Const 1) )

type Substitucia = String -> Exp                
s :: Substitucia
s = (\var -> case var of
                                  "x" -> Const 2
                                  "y" -> Const 6
    )

eval :: Exp -> Substitucia -> Exp
eval  x@(Const c) s = x
eval (Var x) s = s x
eval (Add l p) s = makeAdd (eval l s) (eval p s)
              
derive :: Exp -> String -> Exp
derive (Const c) dx = Const 0
derive (Var x) dx = if x==dx then Const 1 else Const 0
derive (Add l p) dx = makeAdd (derive l dx) (derive p dx)

-- jemne zjedodusujuci konstruktor suctu
makeAdd :: Exp -> Exp -> Exp
makeAdd (Const c1) (Const c2) = Const (c1+ c2)
makeAdd e (Const 0) = e

-- jemne zjedodusujuci konstruktor rozdielu
makeSub :: Exp -> Exp -> Exp
makeSub = undefined

-- jemne zjedodusujuci konstruktor sucinu
makeMul :: Exp -> Exp -> Exp
makeMul = undefined

simply :: Exp -> Exp
simply   = undefined

fix :: (Exp -> Exp) -> Exp -> Exp
fix  = undefined
