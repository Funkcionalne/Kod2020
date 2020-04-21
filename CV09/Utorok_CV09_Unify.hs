module Streda_CV07_Unify where
import Data.Char
data Term = Var String | 
            CN Int | 
            Functor String [Term]  -- deriving(Show)

instance Show Term where
    show (CN n)    = show n
    show (Var name) = name
    show (Functor f args) = f ++ "(" ++ concat [show a++","| a<-args] ++ ")"


type Constraint = (Term, Term)       -- term1 == term2
type Constraints = [Constraint]

unify :: Constraints -> Maybe Constraints
unify [] = Just []
unify (((Functor f fArgs),(Functor g gArgs)):cs) = undefined -- dorobte
unify ((Var vName, t):cs) = if occurs vName  t then Nothing else add2Maybe (Var vName, t) (unify (substitute'' vName t cs))
unify ((t, Var vName):cs) = if occurs vName  t then Nothing else add2Maybe (Var vName, t) (unify (substitute'' vName t cs))
unify ((CN a, CN b):cs) = if a /= b then Nothing else Just cs
unify ((_, _):cs) = Nothing

-- zopar konstant na testovanie
e1 = Functor "f" [ Var "X", Var "X" ]               -- f (x, x)
e2 = Functor "f" [ CN 5,    Var "Y" ]               -- f (5, y)
e3 = Functor "f" [ CN 5,    CN 6 ]                  -- f (5, 6)
e4 = Functor "f" [ (Functor "g" [Var "Y"]),    Var "Y" ]                  -- f (g(y), y)
e5 = Functor "f" [ (Functor "g" [Var "Z"]),    Var "Y" ]                  -- f (g(z), y)
e6 = Functor "f" [ (Functor "g" [Var "Z"]),   (Functor "g" [CN 7]) ]      -- f (g(z), g(7))

ee = [
      (Var "U", Functor "f" [Var "V", Var "V"]),     -- U = f(V,V)
      (Var "Z", Functor "f" [Var "U", Var "U"]),     -- Z = f(U,U)
      (Var "Y", Functor "f" [Var "Z", Var "Z"]),     -- Y = f(Z,Z)
      (Var "X", Functor "f" [Var "Y", Var "Y"])      -- X = f(Y,Y)
      ]

-- unify [(e1, e2)] = Just [(x,5),(y,5)]
-- unify [(e1, e3)] = Nothing
-- unify [(e1, e4)] = Nothing
-- unify [(e1, e5)] = Just [(x,g(z)),(y,g(z))]
-- unify [(e1, e6)] = Just [(x,g(z)),(z,7)]
-- unify ee

occurs :: String -> Term -> Bool
occurs name (CN _) = False
occurs name (Var x) = x==name
occurs name (Functor f args) = or (map (occurs name) args) 


substitute :: String -> Term -> Term -> Term   -- var t t1 = t2, ak t1[x:t]
substitute name t1 (CN x) = CN x
substitute name t1 (Var x) = if name==x then t1 else (Var x)
substitute name t1 (Functor f args) = Functor f (map (substitute name t1) args)


substitute' :: String -> Term -> Constraint -> Constraint
substitute' name t1 (t2,t3) = (substitute name t1 t2, substitute name t1 t3) 

substitute'' :: String -> Term -> Constraints -> Constraints
substitute'' name t1 cs = map (substitute' name t1) cs 

-- prida prvok do Maybe List
add2Maybe :: Constraint -> Maybe Constraints -> Maybe Constraints
add2Maybe c1 Nothing = Nothing
add2Maybe c1 (Just cs) = Just (c1:cs) 
 

-- neprazdny zoznam termov oddelenych ciarkou 
-- <Args> := Term { , Term}^* 
fromStringArgs  :: String -> ([Term], String)
fromStringArgs xs = let (a, xs') = fromString xs in 
                       if not (null xs') &&  head xs' == ',' then 
                          let (as,xs'') = fromStringArgs (tail xs') in 
                             ((a:as), xs'')
                       else ([a], tail xs')


-- <Term> := digit | Variable | symbol [( <Args> )]
fromString  :: String -> (Term, String)
fromString (x:xs)   | isDigit x  = (CN (ord x-48), xs)
fromString [x]      | isAlpha x && isLower x  = (Functor [x] [], [])
fromString (x:y:xs) | isAlpha x && isLower x && y == '(' = let (args, xs') = 
                           fromStringArgs xs in (Functor [x] args, xs')
fromString (x:xs)   | isAlpha x && isUpper x  = (Var [x], xs)
fromString  xs      = error ("syntax error: " ++ xs)                                                   
