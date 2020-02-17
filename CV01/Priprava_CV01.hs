{- 
Ciel:
Prve cvicenie, ako bolo avizovane, chce byt venovane List-comprehension.
Pre istotu ale zacne jednoduchymi ciselnymi funkciami.
V celom cviceni UMYSELNE nepouzijeme ani raz rekurziu (... raz...), len list-comprehension

zjednodusena syntax HASKELL  
[ vyraz |  { premenna <- zoznam alebo test }* ]

syntax PYTHON
[ vyraz { for for premenna <- zoznam alebo if test }* ]
-}

-- Definujte vlastne delitele cisla, [Int] je zoznam intov, 
delitele    :: Int -> [Int]
delitele n = [ d | d <- [2..n-1], n `mod` d == 0]
-- *Main> delitele 24
-- [2,3,4,6,8,12]


-- Definujte test na prvocislo
prvocislo   :: Int -> Bool
--prvocislo n = null (delitele n)    -- zatvorky tu musia byt
prvocislo n = null $ delitele n    -- dolar notacia...

prvocislaMensieAko1000 ::[Int]
prvocislaMensieAko1000 = [ i | i<-[2..999], prvocislo i]
-- *Main> prvocislaMensieAko1000
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997]

prvych100Prvocisel  :: [Int]
prvych100Prvocisel = take 100 [ i | i<-[2..], prvocislo i]
-- *Main> prvych100Prvocisel
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541]

-------------------------------------------------------------------- DOST NA UVOD :)
-- zaroven budeme pisat matiku, Haskell a Python

-- Definujte predikat byt pozmnozina
-- math: A \subset B iff \forall x \in A: x \in Bool

isSubset :: [Int] -> [Int] -> Bool
isSubset as bs = and [ elem x bs | x <- as ]
-- *Main> isSubset [3..9] [1..100]  
-- True

-- Definjte kartezsky sucin dvoch celociselnych mnozin
-- math: { (a, b) | a in A, b in B }

cart2  :: [Int] -> [Int] -> [ (Int,Int) ]
cart2 as bs = [ (a, b) | a <- as, b <- bs ]
-- *Main> cart2 [1..3] [7..9] 
-- [(1,7),(1,8),(1,9),(2,7),(2,8),(2,9),(3,7),(3,8),(3,9)]

-- skusme, ci nas polymorfizmus zabije ?

cart2p :: [t1] -> [t2] -> [(t1,t2)]
cart2p as bs = [ (a, b) | a <- as, b <- bs]

-- *Main> cart2p [1..3] ["janko","marienka"]
-- [(1,"janko"),(1,"marienka"),(2,"janko"),(2,"marienka"),(3,"janko"),(3,"marienka")]


-- Definjte kartezsky sucin troch mnozin
-- math: { (a, b, c) | a in A, b in B, c in C }

cart3p :: [a] -> [b] -> [c] -> [(a,b,c)]
cart3p as bs cs = [ (a,b,c) | (a,(b,c)) <- cart2p as $ cart2p bs cs]

cart3p' :: [a] -> [b] -> [c] -> [(a,b,c)]
cart3p' as bs cs = [ (a,b,c) | a <- as, b <- bs, c <- cs]

-- *Main> cart3p [1..3] ["janko","marienka"] [True, False]
-- [(1,"janko",True),(1,"janko",False),(1,"marienka",True),(1,"marienka",False),(2,"janko",True),(2,"janko",False),(2,"marienka",True),(2,"marienka",False),(3,"janko",True),(3,"janko",False),(3,"marienka",True),(3,"marienka",False)]

-- Definujte kartezsky sucin mnoziny mnozin
-- math: Nech M = {M1, ..., Mn} = { (x1, ... xn) | xi \in Mi }

cart :: [[t]] -> [[t]]
cart [] = [[]]
cart (m:ms) = [ x:xs | x <- m, xs <- cart ms]

-- *Main> cart [['a','b'], ['c', 'd', 'e'], [ 'f']]
-- ["acf","adf","aef","bcf","bdf","bef"]

-- a co ak je niektora nekonecna ?
-- skuste
-- cart [['a','b'], ['a'..]]
-- alebo
-- *Main> take 100 $ cart [['a','b'], ['a'..]]
-- ["aa","ab","ac","ad","ae","af","ag","ah","ai","aj","ak","al","am","an","ao","ap","aq","ar","as","at","au","av","aw","ax","ay","az","a{","a|","a}","a~","a\DEL","a\128","a\129","a\130","a\131","a\132","a\133","a\134","a\135","a\136","a\137","a\138","a\139","a\140","a\141","a\142","a\143","a\144","a\145","a\146","a\147","a\148","a\149","a\150","a\151","a\152","a\153","a\154","a\155","a\156","a\157","a\158","a\159","a\160","a\161","a\162","a\163","a\164","a\165","a\166","a\167","a\168","a\169","a\170","a\171","a\172","a\173","a\174","a\175","a\176","a\177","a\178","a\179","a\180","a\181","a\182","a\183","a\184","a\185","a\186","a\187","a\188","a\189","a\190","a\191","a\192","a\193","a\194","a\195","a\196"]
-- ako to opravit, aby tam bolo raz aj b ? (Hint: diagonalizacia)


-- co ak robime kart.sucin nekonecne vela mnozin, ale konecnych ?
-- viete to urobit (prerobit cart) ? (Hint: asi to pojde...)
-- Priklad:
-- cart [[1,2,3] | i <- [1..] ]

-- co ak robime kart.sucin nekonecne vela mnozin, ale nekonecnych ?
-- viete to urobit ? (Hint: sa ozvite, ak to date, ideme spolu na Fieldsovu medailu)
-- Priklad:
-- cart [[1..] | i <- [1..] ]


-- Definujte mnozinu podmozin mnoziny M (potencna, powerSet)
-- math: P(M) = { A | A subset M }  -- co je ale hodne nekonstruktivna maticka definicia
-- math: druhy pokus
-- Nech M = {x1, ..., xn}. Potom P(M) = P({x2, ..., xn}) \union { M+x1 | M \in P({x2, ..., xn})} 

powerSet :: [t] -> [[t]]
powerSet [] = [[]]
powerSet (x:xs) = smaller ++ [(x:ys) | ys <- smaller] where smaller = powerSet xs

-- *Main> powerSet [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-- toto posledne je uz klasicka rekurzia, ktorej sme sa cele cvicenie vyhybali
-- vie to niekto prepisat bez rekurzie
-- alebo pomocou list-comprehension ?

-- toto je asi verzia pre paradigmatikov, skuste to preluskat. Nie je to principialne pre ostatnych, v 1.tyzdni :)
powerSet' :: [t] -> [[t]]
powerSet' xs = foldr (\x -> \smaller -> smaller ++ [(x:ys) | ys <- smaller]) [[]] xs

-- *Main> powerSet' [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]


-- frajerinka, zmizol explicitny argument
powerSet'' :: [t] -> [[t]]
powerSet'' = foldr (\x -> \smaller -> smaller ++ [(x:ys) | ys <- smaller]) [[]]
-- *Main> length $ powerSet'' [1..10]
-- 1024

