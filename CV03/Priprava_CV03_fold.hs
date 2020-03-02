module Priprava_CV03_fold where

-- [x0, x1, ... xn-1] -> ([x1,x4,x7,...], [x2,x5,x8,...])
kazdyTretiDoKosa    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa    xs    = (reverse vs, reverse us)
                          where
                          (_, vs, us) =    foldl ( \(i, as, bs) -> \x -> if i `mod` 3 == 0 then (i+1, as, bs)
                                                                         else if i `mod` 3 == 1 then (i+1, x:as, bs)
                                                                         else (i+1, as, x:bs)
                                ) (0, [], []) xs

-- kazdyTretiDoKosa [2..20] = ([18,15,12,9,6,3],[19,16,13,10,7,4])

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

