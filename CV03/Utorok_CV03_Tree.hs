module Streda_CV03_Tree where
import Test.QuickCheck
import Tree

-- toto bolo na prednaske a neplatilo
-- qch3 = quickCheck((\x -> \tree -> (isBVS tree) ==> ((find x tree) == (elem x (flat tree))))::Int->BVS Int->Property)

-- ak je isBVS, potom find x tree je ako hladanie x v splostenom zozname
qch4 = quickCheckWith stdArgs{ maxSuccess = 100000 } ((\x -> \tree -> (isBVS tree) ==> ((find x tree) == (elem x (flat tree))))::Int->BVS Int->Property)         

insert :: (Ord t) => t -> BVS t -> BVS t
insert = undefined

-- e = Node Nil 4 (Node Nil 7 Nil)                                                       
-- insert 1 e = Node (Node Nil 1 Nil) 4 (Node Nil 7 Nil)
-- insert 5 e = Node Nil 4 (Node (Node Nil 5 Nil) 7 Nil)
-- insert 9 e = Node Nil 4 (Node Nil 7 (Node Nil 9 Nil))

-- x sa po inserte urcite v strome nachadza
-- qch5 = quickCheckWith stdArgs{ maxSuccess = 100000 } 

-- velkost stromu je pocet Node uzlov
size :: BVS t -> Int
size = undefined

-- velkost stromu po inserte je o jedna vacsia, asi neplati, ak sa x tam uz nachadza
-- qch6 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ...  )
-- Failed! Falsifiable (after 1 test):

-- ak sa x v strome nenachadza a insertneme ho tam, tak bude o 1 vacsi
-- qch7 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ...  )

-- aj ked ho tam 2x insertneme, strom sa zvacsi len o 1
-- qch8 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ... )

-- maximalny v strome, ale musi byt isBVS
maxBVS                        :: BVS t -> t
maxBVS = undefined

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
