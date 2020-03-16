module Huffman where

data HTree = Leaf (Int, String) | Node HTree Int HTree deriving (Eq) -- deriving (Show, Eq)

instance Show HTree where
      show (Leaf (v,p)) = p
      show (Node l v r) = "(" ++ (show l) ++ "," ++ (show r) ++ ")"

instance Ord HTree where 
                        t1 < t2 = weight t1 < weight t2
                        t1 <= t2 = weight t1 <= weight t2
                --      t1 == t2 = weight t1 == weight t2

weight :: HTree -> Int
weight (Leaf (n, s)) = n
weight (Node l n r) = n       

-- vsun prvok a::t do utriedeneho zoznamu xs::[t]
insert :: (Ord t) => t -> [t] -> [t]
insert a [] = [a]
insert a (x:xs)
    | a < x = a:x:xs
    | a >= x = x:(insert a xs)	

-- zober najmensie dva prvky (prve dva), urob z nich HTree a vsun do zoznamu...
combine :: [HTree] -> [HTree]
combine (x:y:xs) = insert new xs
    where new = (Node x ((weight x) + (weight y)) y)

isSingle :: [HTree] -> Bool
isSingle xs = length xs == 1

-- naprv vyrob (map Leaf ft), opakuj combine, ak kym neplati isSingle...
huffman :: [(Int, String)] -> HTree
huffman ft = head $ until isSingle combine (map Leaf ft)

-- ft je utriedena vzostupne
ft' :: [(Int, String)]
ft' = [ (5,"b"), (10, "a"), (17,"m")]

-- ft je utriedena vzostupne
ft :: [(Int, String)]
ft = [ (5,"b"), (10, "a"), (17,"m"), (19, "k"), (26,"e")]