module Priprava_CV02 where
import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort,nub)

-- slova dlzky k nad abecedou
--slova :: [Char] -> Int -> [String]
slova :: String -> Int -> [String]
slova abeceda 0 = [ [] ]   -- dovod bol na prednaske
slova abeceda n = [ ch:w | ch <- abeceda, w <- slova abeceda (n-1) ]   -- dovod bol na prednaske

-- slova "01" 3 = ["000","001","010","011","100","101","110","111"]
-- slova dlzky k nad abecedou su vlastne variacie s opakovanim

pocetSlova abeceda k = (length abeceda) ^ k

qchSlova = quickCheck(\abeceda -> \k -> (k >= 0 && k <= 10 && length abeceda < 6) 
                    ==> length (slova abeceda k) == pocetSlova abeceda k)


-- slova dlzky najviac k nad abecedou, zle riesenie
-- slova dlzky najviac k
slovaNajviac :: [Char] -> Int -> [String]
slovaNajviac abeceda 0  = [[]]
slovaNajviac abeceda k = slovaNajviac abeceda (k-1) ++ 
                         [ ch:w | w <-slovaNajviac abeceda (k-1), ch <- abeceda]

--length $ slovaNajviac "ABCDEF" 2 = 49 != 1+6+36 = 43
--slova' 2 = ["","A","B","C","D","E","F","A","B","C","D","E","F","AA","BA","CA","DA","EA","FA","AB","BB","CB","DB","EB","FB","AC","BC","CC","DC","EC","FC","AD","BD","CD","DD","ED","FD","AE","BE","CE","DE","EE","FE","AF","BF","CF","DF","EF","FF"]

--slovaNajviac "ABCDEF" 2 = 43
--BTW. kolko je 1+6+36+...+6^k (slova dlzky najviac k) ?


pocetSlovaNajviac [_] k = k+1
pocetSlovaNajviac abeceda k = (size^(k+1)-1) `div` (size-1) where size = length abeceda
-- explicit "ABCDEF" 2 = 43

qchNajviac = quickCheck(\abeceda -> \k -> (k >= 0 && k <= 10 && 
                                     1 < length abeceda && length abeceda < 6 &&
                                     length abeceda == length ( nub abeceda)) ==> 
                    length ( nub (slovaNajviac abeceda k)) == pocetSlovaNajviac abeceda k)

qchNajviacABCDEF = quickCheck(\k -> (k >= 0 && k <= 7 ==> 
                    length ( nub (slovaNajviac "abcdef" k)) == pocetSlovaNajviac "abcdef" k))


-- dobre riesenie
slovaNajviac' abeceda k = concat [ slova abeceda dlzka | dlzka <- [0..k] ]
-- length $ slovaNajviac' "abcdef" 2

qchNajviac' = quickCheck(\abeceda -> \k -> (k >= 0 && k <= 10 && 
                                     1 < length abeceda && length abeceda < 6 &&
                                     length abeceda == length ( nub abeceda)) ==> 
                    length ( slovaNajviac' abeceda k) == pocetSlovaNajviac abeceda k)



-- slova nad abecedou dlzky k, kde susedne pismenka su rozne
slovaSusedneRozne :: String -> Int -> [String]
slovaSusedneRozne abeceda 0 = [ [] ]
slovaSusedneRozne abeceda 1 = [ [ch] | ch <- abeceda ]
slovaSusedneRozne abeceda k = [ ch:kratsie 
                                |
                                kratsie <- slovaSusedneRozne abeceda (k-1),
                                ch <- abeceda, head kratsie /= ch ]
                                
pocetSlovaSusedneRozne abeceda 0 = 1
pocetSlovaSusedneRozne abeceda 1 = length abeceda
pocetSlovaSusedneRozne abeceda k = (length abeceda-1) * pocetSlovaSusedneRozne abeceda (k-1)

qchSlovaSusedneRozne = quickCheck(\abeceda -> \k -> (k >= 0 && k <= 10 && 
                                     1 < length abeceda && length abeceda < 6 &&
                                     length abeceda == length ( nub abeceda)) ==> 
                    length ( slovaSusedneRozne abeceda k) == pocetSlovaSusedneRozne abeceda k)


-- slova nad abecedou "ab", ktore neobsahuju aa, teda dve acka za sebou
slovaBezAA :: Int -> [String]
slovaBezAA 0 = [ [] ]
slovaBezAA 1 = [ "a", "b" ]
slovaBezAA k =  [  'b':s | s <-slovaBezAA (k-1) ] 
                ++
                [  'a':'b':s | s <- slovaBezAA (k-2) ] 
                
fibonacci 0 = 1
fibonacci 1 = 2
fibonacci k = fibonacci (k-1) + fibonacci (k-2)

qchSlovaBezAA = quickCheck(\k -> k >= 0 && k <= 10 ==>
                            (length (slovaBezAA k) == fibonacci k))

-- slova nad abecedou "a...", ktore neobsahuju aa, teda dve acka za sebou
slovaBezAA' :: String -> Int -> [String]
slovaBezAA' abeceda 0 = [ [] ]
slovaBezAA' abeceda 1 = [ [ch] | ch <- abeceda ]
slovaBezAA' abeceda k = [  ch:s | s <- slovaBezAA' abeceda (k-1), ch <- abeceda, ch /= 'a' ] 
                        ++
                        [  'a':ch:s | s <- slovaBezAA' abeceda (k-2), ch <- abeceda, ch /= 'a' ] 


fibonacci' abeceda 0 = 1
fibonacci' abeceda 1 = length abeceda
fibonacci' abeceda k = (length abeceda-1) * (fibonacci' abeceda (k-1) + fibonacci' abeceda (k-2))
                
qchSlovaBezAA' = quickCheck(\k -> k >= 0 && k <= 10 ==>
                                    (length (slovaBezAA' "abcd" k) == fibonacci' "abcd" k))


-- definujte predikat pre usporiadany/rastuci/nerastuci zoznam
ordered :: [Int] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:xs@(y:ys)) = x<=y && ordered xs

ordered' :: [Int] -> Bool
ordered' [] = True
ordered'  xs = all (\(x,y) -> x <= y) $ zip (init xs) (tail xs)


-- definujte a overte nejaku vlastnost funkcie ordered

qch1 = quickCheck( \xs -> ordered xs == ordered' xs )
qch1v = verboseCheck(\xs -> ordered $ sort xs  )
qch1v' = verboseCheck(\xs -> ordered' $ sort xs  )

qch1r = verboseCheck(\xs -> (length xs > 1) ==> not $ ordered $ reverse $ sort xs  )

-- naprogramuje jeden prechod bubble sort algoritmu
bubble :: [Int] -> [Int]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) = if x>y then y:(bubble(x:xs)) else x:(bubble(y:xs))

-- pouzite bubble na bubbleSort
bubbleSort  :: [Int]->[Int]
bubbleSort xs = (iterate bubble xs)!!(length xs)
e = bubbleSort [4,3,4,6,7,4,3,1,1,2,3,4,5,6,7,8,9,0,5,3,2,3,2,3,4,5,6,7,1,2,2,0,9,12,11]
--[0,0,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,6,7,7,7,8,9,9,11,12]

-- definujte a overte nejaku vlastnost funkcie bubbleSort
qch2 = quickCheck( \xs -> bubbleSort xs == sort xs)
qch2v = undefined -- verboseCheck( \xs -> cond ==> proposition )

powerSet :: [t] -> [[t]]
powerSet [] = [[]]
powerSet (x:xs) = let ps = powerSet xs in ps ++ [x:ys | ys <- ps]

qch3 = quickCheck((\xs -> (length xs < 10) ==> (length $ powerSet xs) == (2^(length xs)))::[Int]->Property)

qch3' = verboseCheck((\xs -> (length xs < 10) ==> 
        (length $ nub $ powerSet xs) == (2^(length xs)))::[Int]->Property)

qch3'' = verboseCheck((\n -> (n >= 0 && n < 10) ==> 
        (length $ nub $ powerSet [1..n]) == (2^n))::Int->Property)

subset :: (Eq t) => [t] -> [t] -> Bool
subset xs ys = and [ elem x ys | x<-xs]

subset' :: (Eq t) => [t] -> [t] -> Bool
subset' xs ys = all  (\x -> elem x ys) xs

qch3''' = quickCheck((\n -> (n >= 0 && n < 10) ==> 
                        and [ subset m [1..n] | m <- powerSet [1..n] ])::Int->Property)

---------------------------------
-- kompozícia zoznamu funkcií,  :: [a->a] -> (a->a)

-- zaciatocnicka definicia cez zoznamovu rekurziu
kompozicia  :: [a->a] -> (a->a)
kompozicia [] = id
kompozicia (f:fs) = (\x -> f (kompozicia fs x))

-- definicia haskellistu, co si nasiel operator $
kompozicia''  :: [a->a] -> (a->a)
kompozicia'' [] = id
kompozicia'' (f:fs) = \x -> f $ kompozicia'' fs x

qchf = quickCheck((\f -> \g -> \x -> (kompozicia [f,g]) x == (kompozicia'' [f,g]) x)::
            (Int->Int) -> (Int->Int) -> Int -> Bool)

-- definicia haskellistu, co si este prehodil x na lavu stranu
kompozicia'''''  :: [a->a] -> (a->a)
kompozicia''''' [] x      = x
kompozicia''''' (f:fs) x  = f $ kompozicia''''' fs x

-- jemne pokrocily haskellista, ktory bol na prednaske
kompozicia'  :: [a->a] -> (a->a)
kompozicia' [] = id
kompozicia' (f:fs) = f . kompozicia' fs

-- haskellista, co si pamata, ze skladanie funkcii je asociativne ale nepamata, ze nie je komutativne
kompozicia''''  :: [a->a] -> (a->a)
kompozicia'''' [] = id
kompozicia'''' (f:fs) = kompozicia'''' fs . f

-- haskellista, co bude volit lavicu
kompoziciaLeft  :: [a->a] -> (a->a)
kompoziciaLeft = foldl (.) id

-- haskellista, co bude volit neexistujucu pravicu
kompoziciaRight  :: [a->a] -> (a->a)
kompoziciaRight = foldr (.) id

zoznamfcii = [(+7),(*11),(`mod` 1234567),(`div` 10),(^4),(+1),(*2),(^3)]

{-
*Main> kompozicia      zoznamfcii 1
95
*Main> kompozicia''    zoznamfcii 1
95
*Main> kompozicia''''' zoznamfcii 1
95
*Main> kompozicia'     zoznamfcii 1
95
*Main> kompozicia''''  zoznamfcii 1
550158565384
*Main> kompoziciaLeft  zoznamfcii 1
95
*Main> kompoziciaRight zoznamfcii 1
95

-- kompozicia funkcii nie je komutativna

*Main> kompozicia      (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''    (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''''' (reverse zoznamfcii) 1
550158565384
*Main> kompozicia'     (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''''  (reverse zoznamfcii) 1
95
*Main> kompoziciaLeft  (reverse zoznamfcii) 1
550158565384
*Main> kompoziciaRight (reverse zoznamfcii) 1
550158565384

-- evidentne definicia kompozicia'''' je zla, kedze predpokladala komutativnost (.)
-}
