module Streda_CV03_Tree where
import Test.QuickCheck
import Tree

e'::BVS Int
e' = Node (Node Nil 1 Nil) 2 (Node Nil 3 Nil)

-- toto bolo na prednaske a neplatilo
qch2P = quickCheck((\x -> \tree -> ((find x tree) == (elem x (flat tree))))::Int->BVS Int->Bool)
qch3 = quickCheck((\x -> \tree -> (isBVS tree) ==> ((find x tree) == (elem x (flat tree))))::Int->BVS Int->Property)

-- velkost stromu je pocet Node uzlov
size :: BVS t -> Int
size Nil = 0
size (Node left val right) = 1 + size left + size right

-- ak je isBVS, potom find x tree je ako hladanie x v splostenom zozname
qch4 = quickCheckWith stdArgs{ maxSuccess = 100000 } ((\x -> \tree -> (isBVS tree) ==> ((find x tree) == (elem x (flat tree))))::Int->BVS Int->Property)         

insert :: (Ord t) => t -> BVS t -> BVS t
insert val Nil = Node Nil val Nil
insert val (Node left x right) | x == val = Node left x right
                               | x > val = (Node (insert val left) x right)
                               | otherwise = (Node left x (insert val right))

qOnSize = quickCheck((\x -> \tree -> (not (find x tree)) ==> 1 + (size tree) == size (insert x tree))
        ::Int->BVS Int->Property)


-- e = Node Nil 4 (Node Nil 7 Nil)                                                       
-- insert 1 e = Node (Node Nil 1 Nil) 4 (Node Nil 7 Nil)
-- insert 5 e = Node Nil 4 (Node (Node Nil 5 Nil) 7 Nil)
-- insert 9 e = Node Nil 4 (Node Nil 7 (Node Nil 9 Nil))

-- x sa po inserte urcite v strome nachadza
-- qch5 = quickCheckWith stdArgs{ maxSuccess = 100000 } 


-- velkost stromu po inserte je o jedna vacsia, asi neplati, ak sa x tam uz nachadza
-- qch6 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ...  )
-- Failed! Falsifiable (after 1 test):

-- ak sa x v strome nenachadza a insertneme ho tam, tak bude o 1 vacsi
-- qch7 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ...  )

-- aj ked ho tam 2x insertneme, strom sa zvacsi len o 1
-- qch8 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ... )

-- maximalny v strome, ale musi byt isBVS
maxBVS                        :: BVS t -> t
maxBVS Nil = undefined
maxBVS (Node _ val Nil) = val
maxBVS (Node left val right) = maxBVS right  

-- delete v strome, ale musi byt isBVS
delete :: (Ord t) => t -> BVS t -> BVS t
delete = undefined

{-                                
e = Node Nil 4 (Node Nil 7 Nil)
delete 4 e = Node Nil 7 Nil
delete 7 e = Node Nil 4 Nil
delete 1 e = Node Nil 4 (Node Nil 7 Nil)
-}

-- ak sa x nachadza v strome, po delete bude o jeden uzol mensi                             
-- qch9 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ... )

-- ak sa x nenachadza v strome po delete
-- qch10 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ... )

          
u = Node Nil 4 (Node Nil 7 Nil)                
u1 = Node Nil 4 (Node Nil 4 Nil)                

-- v strome su len rovnake hodnoty                                
isUnival :: (Eq t) => BVS t -> Bool  
isUnival Nil = True
isUnival (Node left x right) = 
        (left == Nil || let (Node _ lx _) = left in x == lx)
        &&
        (right == Nil || let (Node _ rx _) = right in x == rx)
        && isUnival left && isUnival right

same [] = True
same (x:xs) = all ( == x) xs

isUnival' tree = same(flat(tree))

qOnUnival = quickCheck((\tree -> isUnival tree == isUnival' tree)
        ::BVS Int->Bool)
        
        
uniques :: (Ord t) => BVS t -> Int        
uniques t = fst(uniquesAux t)

uniquesAux :: (Ord t) => BVS t -> (Int, Bool)        
uniquesAux Nil = (0, True)
uniquesAux (Node l x r) = ( u, isunique)
        where (uniquesl, isuniquel) = uniquesAux l
              (uniquesr, isuniquer) = uniquesAux r
              isunique = isuniquel && isuniquer &&
                    (l == Nil || let (Node _ lx _) = l in x == lx)
                    &&
                    (r == Nil || let (Node _ rx _) = r in x == rx)
              u=   uniquesl +  uniquesr + (if  isunique then 1 else 0)

