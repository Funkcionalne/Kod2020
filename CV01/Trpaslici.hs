module Trpaslici where

moznosti' :: Int -> [(Int,Int,Int)]
moznosti' n = [ (a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a*b*c == n ]

moznosti :: Int -> [(Int,Int,Int)]
moznosti n = [ (a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], a*b*c == n ]

pocetMoznosti :: Int -> Int
pocetMoznosti n = length (moznosti n)

{-
*Trpaslici> length (moznosti 36)
8
*Trpaslici> length (moznosti 1024)
-}

