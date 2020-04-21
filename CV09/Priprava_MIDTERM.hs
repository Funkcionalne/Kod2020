data Tree t = Node t [Tree t]  -- deriving(Show)

tr = Node "a" [Node "b" [],Node "c" [],Node "d" []] 
tr2 = Node "1" [tr,Node "2" [],Node "2" []] 

instance (Show t) => Show (Tree t) where
    show (Node root []) = show root 
    show (Node root sons) = (show root) ++ "(" ++ (concat [ show son ++ "," | son <- sons]) ++ ")"
    
preorder :: Tree t -> [t]
preorder (Node root sons) = root : (concat [preorder son | son <- sons])

data BTree t = BNode (BTree t) t (BTree t) | Nil  deriving(Show)

convert (BNode Nil value Nil) = Node value []
convert (BNode Nil value right) = Node value [convert right]
convert (BNode left value Nil) = Node value [convert left]
convert (BNode left value right) = Node value [convert left, convert right]

bt1 = BNode Nil 4 Nil
bt2 = BNode Nil 5 Nil
bt3 = BNode bt1 6 bt2


