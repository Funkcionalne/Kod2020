module Tree where

data Tree t = Nil | Node (Tree t) t (Tree t) -- deriving(Show)

instance (Show t) => Show (Tree t) where    
    show Nil = "."
    show (Node l v r) = "(" ++ (show l) ++ "," ++ (show v) ++ "," ++ (show r) ++ ")"
    

e = Node Nil 5 Nil
e2 = Node e 7 e
e1 = Node (Node (Node Nil 1 Nil) 3 (Node Nil 4 Nil)) 6 (Node (Node Nil 7 Nil) 9 (Node Nil 15 Nil))

{-
      6 
     / \
    /   \
   /     \
  3       9
 / \     / \
1   4   7  15
-}

preorded :: Tree t -> [t]
preorded Nil = []
preorded (Node l v r) = v : (preorded l ++ preorded r)

preorded' :: Tree t -> [t] -> [t]
preorded' Nil acc = acc
preorded' (Node l v r) acc = preorded' r (preorded' l (v:acc))

inorder :: Tree t -> [t]
inorder Nil = []
inorder (Node l v r) = inorder l ++ (v:inorder r)

postorder :: Tree t -> [t]
postorder Nil = []
postorder (Node l v r) = postorder l ++ (postorder r) ++ [v]


e22 = Node (Node (Node Nil 'd' Nil) 'b' (Node Nil 'e' Nil)) 'a' (Node (Node Nil 'f' Nil) 'c' (Node Nil 'g' Nil))
{-
    a
   / \
  b   c
 / \ / \
d  e f  g
-}

e33 = Node (Node (Node Nil 'c' Nil) 'b' (Node Nil 'd' Nil)) 'a' (Node Nil 'e' (Node Nil 'f' Nil))
{-
    a
   / \
  b   e
 / \   \
c   d   f
-}

reconstruct :: (Eq t) => [t] -> [t] -> Tree t
reconstruct jankoLog marienkaLog = undefined


