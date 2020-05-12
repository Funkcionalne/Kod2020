module Priprava_CV12_Functor where
import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property (forAllShrink)
import Control.Monad
import Control.Monad.State

{-
-- Functors abstract the idea of mapping a function over each element of a structure

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instancia triedy Functor musi definovat fmap, ktory splna:

fmap id      = id
fmap (p . q) = (fmap p) . (fmap q)
-}

{---------------------------------------------- Maybe
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)
-}

-- vyhodnotte
x1 = fmap (+1) Nothing
x2 = fmap (*2) (Just 3)
x3 = fmap not (Just False)

-- vyhodnotte  
inc :: (Functor f) => f Int -> f Int
inc = fmap (+1)

x4 = inc (Just 1)
x5 = inc (1,2)
x6 = inc [1,2,3,4,5]

------------------------------------  bin.strom s hodnotami v listoch
data BinTree a = Leaf a | Branch (BinTree a) (BinTree a) deriving (Show)
instance Functor BinTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

x7 = fmap length (Leaf "abc")
x8 = fmap even (Branch (Leaf 1) (Leaf 2))
x9 = inc (Branch (Leaf 1) (Leaf 2))

----------------------------------------- bin.strom s hodnotami vo vnutornych vrcholoch
data BVS a = Nil | Node (BVS a) a (BVS a) deriving (Show, Eq)
instance Functor BVS where
    fmap f Nil = Nil
    fmap f (Node left key right) = Node (fmap f left) (f key) (fmap f right)

e1 :: BVS Int
e1 = (Node (Node Nil 1 Nil) 2 (Node Nil 1 Nil))
y1 = fmap (+1) e1
y1' = inc e1

e2 :: BVS String
e2 = (Node (Node Nil "1" Nil) "2" (Node Nil "1" Nil))
y2 = fmap (\s -> s++s) e2

instance Arbitrary a => Arbitrary (BVS a) where
  arbitrary = frequency 
              [
                (1, return Nil )
              , (1, liftM3 Node arbitrary arbitrary arbitrary)
              ]
-- generate (arbitrary::Gen (BVS Int))
 
-- overenie miesto dokazu ...              
qch1 = quickCheck((\t -> fmap id t == t)::BVS Int->Bool)

qch2 = quickCheck((\t -> \f -> \g -> fmap (f.g) t == ((fmap f) . (fmap g)) t)
                                    ::BVS Int->(Int->Int)->(Int->Int)->Bool)

-------------------------------------------------------- zoznam
data MyList a = Null | Cons a (MyList a) deriving (Show)       -- [a]
instance  Functor MyList  where
       fmap f Null = Null
       fmap f (Cons x xs) = Cons (f x)(fmap f xs) 

--instance  Functor []  where
fmap' f [] = []
fmap' f (x:xs) = fmap' f xs ++ [f x]
              
y3 = fmap (+1) [1..10]
y4 = fmap' (+1) [1..10]

qch3 = quickCheck((\xs -> fmap id xs == xs)::[Int]->Bool)        
qch4 = quickCheck((\xs -> \f -> \g -> fmap (f.g) xs == fmap f (fmap g xs))
            ::[Int]->(Int->Int)->(Int->Int)->Bool)        
            
qch3' = quickCheck((\xs -> fmap' id xs == xs)::[Int]->Bool)        
qch4' = quickCheck((\xs -> \f -> \g -> fmap' (f.g) xs == fmap' f (fmap' g xs))
            ::[Int]->(Int->Int)->(Int->Int)->Bool)        
            
-------------------------------------------------- Rhododendron
data RoseTree a = Rose a [RoseTree a]    deriving (Show)
instance Functor RoseTree where
    fmap f (Rose a bs) = Rose (f a) (map (fmap f) bs)

r1 :: RoseTree Int
r1 = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 [] ] ]
y5 = inc r1

r2 :: RoseTree [Int]
r2 = Rose [1..3] [Rose [2..5] [], Rose [3] [Rose [4..44] [], Rose [5..55] [] ] ]
y6 = fmap length r2

{-
instance Arbitrary a => Arbitrary (RoseTree a) where
 ???
-}
  