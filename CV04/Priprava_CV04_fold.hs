module Priprava_CV03_fold where
import Test.QuickCheck

-- foldl f z [a1,...,an] = (... (f (f (f z a1) a2) a3 ... an)

-- foldl1 je reduce z Pythonu, pozrite si reduce.py
-- foldl1 f [a1,...,an] = (... (f (f a1 a2) a3 ... an)
-- foldl1 f [a1] = a1
-- foldl1 f [] = undefined
-- priklad
-- foldl1 (/) [64,4,2,8] = 1.0
-- py: print(reduce((lambda x, y: x / y), [64,4,2,8]))

-- definujte vlastny foldl1 pomocou foldl
foldl1' f (x:xs) = foldl f x xs


--------------------------------------------------
-- foldr f z [a1,...,an] = (f a1 (f a2 ... (f a_n-1 f(an z))))

-- foldr1 f [a1,...,an] = (f a1 (f a2 ... (f a_n-1 an)))
-- foldr1 f [a1] = a1
-- foldr1 f [] = undefined
-- priklad
-- foldr1 (/) [8,12,24,4] = 4.0

-- definujte vas vlastny foldr pomocou foldr1
foldr''' f z xs = foldr1 f (xs ++ [z])

-------------------------------------------
-- scanl je to co foldl, ale so vsetkymi medzivysledkami
-- scanl f z [a1, ..., an] = [z, f z a1, f (f z a1) a2, ...]
-- scanl (\x -> \y -> 2*x + y) 4 [1,2,3] =  [4,9,20,43] 

-- definujte scanl pomocou foldl
-- definujte scanl pomocou foldl efektivnejsie (napr. pomocou reverse)
scanl' = undefined

-- definujte foldl pomocou scanl
foldl'' = undefined

-- definujte scanr pomocou foldr
scanr' = undefined

-- definujte foldr pomocou scanr
foldr'''' = undefined


-----------------------------------------------------------------------------------------------------

-- pocet nul vo vektore/matici pomocou foldr/l
pocetNul :: [Int] -> Int
pocetNul xs = foldl (\a -> \b -> if b == 0 then a + 1 else a) 0 xs

druhyNajvacsi :: [Int] -> Int        
druhyNajvacsi [] = error "prazdny zoznam"
druhyNajvacsi [x] = 0
druhyNajvacsi (x:y:xs) = snd $ foldl f (max x y, min x y) xs
                         where f (x, y) z | z < y = (x, y)
                                          | z >= x = (z, x)
                                          | otherwise = (x, z)

maximalny :: [Int] -> Int
maximalny (x:xs) = foldl (\a -> \b -> max a b) x xs


-- rozdiel max a minimalneho prvku vo vektore/matici
maxmin = undefined

maxmin' = undefined

{- --------------------------------------------------------------------
      definujte funkciu priemer :: [Float] -> Float, ktora vypocita aritmeticky zoznamu [Float]
      priemer len pouzitim foldr
-}
priemer :: [Float] -> Float
priemer xs = ssum / cc
    where (cc, ssum) = foldr f (0,0) xs
          f z (c,sum) = (c+1, sum+z)
          
priemer2 :: [Float] -> Float
priemer2 xs = uncurry (/) $ foldr f (0,0) xs
                          where f z (sum, c) = (sum+z, c+1)          

{-
foldl/r na matici
-}                  

-- priemer prvkov v matici
-- vrati sucet a pocet prvkov vo vektore

priemer'      :: [[Float]] -> Float
priemer' xss = priemer $ foldr (\row -> \y -> (priemer row) : y) [] xss

priemer''      :: [[Float]] -> Float
priemer'' xss = uncurry (/) $ foldr (\row -> \(s,c) -> let p = sucpoc row 
                                        in (s + fst p , c + snd p)) (0.0, 0.0) xss
                                         
priemer'''      :: [[Float]] -> Float
priemer''' xss = uncurry (/) $ foldr (\row -> \(s,c) -> let (rows, rowp) = sucpoc row 
                                                        in (s + rows , c + rowp)) (0.0, 0.0) xss                                         
      
sucpoc :: [Float] -> (Float, Float)
sucpoc xs = foldr f (0,0) xs
            where f z (sum, c) = (sum+z, c+1)                 
      
-------------------------------------------------------------------------------------

-- prejde zoznam a prvky rozdeli podla index `mod` 3, a kazdy treti zahodi
-- [x0, x1, ... xn-1] -> ([x1,x4,x7,...], [x2,x5,x8,...])
kazdyTretiDoKosa    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa    xs    = (reverse vs, reverse us)
                          where
                          (_, vs, us) =    foldl ( \(i, as, bs) -> \x -> if i `mod` 3 == 0 then (i+1, as, bs)
                                                                         else if i `mod` 3 == 1 then (i+1, x:as, bs)
                                                                         else (i+1, as, x:bs)
                                ) (0, [], []) xs

-- kazdyTretiDoKosa [2..20] = ([18,15,12,9,6,3],[19,16,13,10,7,4])

-- to iste pomocou foldr
kazdyTretiDoKosa'    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa'    xs    = (vs, us)
                          where
                          (_, vs, us) =    foldr ( \x -> \(i, as, bs) -> if i `mod` 3 == 0 then (i+1, as, bs)
                                                                         else if i `mod` 3 == 1 then (i+1, x:as, bs)
                                                                         else (i+1, as, x:bs)
                                              ) (0, [], []) xs
-- bohuzial, foldr pocita od konca...
-- kazdyTretiDoKosa' [2..20] = ([4,7,10,13,16,19],[3,6,9,12,15,18])

kazdyTretiDoKosa''    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa''    xs    = (vs, us)
                          where
                          (_, vs, us) =    foldr ( \x -> \(i, as, bs) -> if i `mod` 3 == 0 then (i+1, as, bs)
                                                                         else if i `mod` 3 == 1 then (i+1, x:as, bs)
                                                                         else (i+1, as, x:bs)
                                              ) (0, [], []) (reverse xs)

-- kazdyTretiDoKosa'' [2..20] = ([18,15,12,9,6,3],[19,16,13,10,7,4])
                                                                
kazdyTretiDoKosa'''    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa'''    xs    = foldl (\(as, bs) -> \(i,x) -> if  i `mod` 3 == 0 then (as, bs)
                                                             else if i `mod` 3 == 1 then (x:as, bs)
                                                             else (as, x:bs)
                                ) ([],[]) poleAkoVPythone
                        where poleAkoVPythone = zip [0..] (reverse xs)
                                
-- kazdyTretiDoKosa''' [2..20] = ([4,7,10,13,16,19],[3,6,9,12,15,18])

---------------------------------------------------------------------------------------------------------

-- suciny vrati pre zoznam cisel [...xi...] vysledny zoznam [...yi...], kde kazde yi je sucin vsetkych 
-- prvkov vstupneho zoznamu okrem xi

suciny :: [Int] -> [Int]
suciny xs = [ product xs `div` x | x <- xs] 
{-
suciny [1..6] = [720,360,240,180,144,120]
[1..6] = [1,2,3,4,5,6]
-}

--- ale nesmiete delit
suciny' :: [Int] -> [Int]
suciny' xs = [ product [ xs!!j | j <- [0..length xs-1], j /= i] | i <- [0..length xs-1] ] 

qch1 = quickCheck(\xs -> suciny xs == suciny' xs)
{-
*Priprava_CV03_fold> qch1
*** Failed! Exception: 'divide by zero' (after 6 tests and 2 shrinks):
[0]
-}


-- riesenie suciny' je kvadraticke, skuste to urobit linearne
suciny'' :: [Int] -> [Int]
suciny'' xs = zipWith (*) (sufixy xs) (reverse (sufixy (reverse xs)))

sufixy :: [Int] -> [Int]
sufixy [] = []
sufixy [_] = [1]
sufixy (x:xs) = let pom = sufixy xs in (head xs * head pom) : pom

{-
 suciny'' [1..6]
[720,360,240,180,144,120]
-}

qch2 = quickCheck(\xs -> suciny'' xs == suciny' xs)


sufixy' :: [Int] -> [Int]
sufixy' [] = []
sufixy' xs = tail $ scanr (*) 1 xs

qch3 = quickCheck(\xs -> sufixy xs == sufixy' xs)

suciny''' :: [Int] -> [Int]
suciny''' xs = zipWith (*) (sufixy' xs) (reverse (sufixy' (reverse xs)))

qch4 = quickCheck(\xs -> suciny''' xs == suciny' xs)

