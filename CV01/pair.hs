-- takto nejako je definovana dvojica v Haskelli
dvojka a b = (a,b)
first (a,b) = a         -- funkcia sa vola fst
second (a,b) = b        -- funkcia sa vola snd

-- ale rozmysliet ste si mali tuto implementaciu

dvojica :: p1 -> p2 -> ((p1 -> p2 -> p3) -> p3)
dvojica a b = pair
              where pair(f) = f a b
                
dvojica' :: t1 -> t2 -> ((t1 -> t2 -> t3) -> t3)
dvojica' a b = \f -> f a b

prvy p = undefined

druhy p =  undefined


{-
*Main> prvy (dvojica 3 4)
3
*Main> druhy (dvojica 3 4)
4
-}
















