{-# LANGUAGE RankNTypes #-}
module Cvicenie6 where
import Unsafe.Coerce 

-- zdroj http://techtipshoge.blogspot.sk/2011/06/church-number-with-ski-combinators.html
-- http://www.angelfire.com/tx4/cus/combinator/birds.html
-- http://dkeenan.com/Lambda/
-- https://books.google.sk/books/about/To_Mock_a_Mockingbird.html?id=wklinRKAIgQC&redir_esc=y

-- https://play.google.com/books/reader?id=NyF1kvJhZbAC&hl=sk&pg=GBS.PT171

-- λx.x  
i = \x -> x

-- λx.λy.x  
k = \x -> \y -> x

{-
co, ak by sme si nevsimli, ze to je K....
λx.λy.x = "K"
λx.(K x) = "S"
S (λx.K) (λx.x) = "I"
S (λx.K) I = "K"
S (K K) I

I x = x
K c x = c
S f g x = ((f x) (g x))
---
λx.λy.x  = K
λx.(K x) = S
S (λx.K) (λx.x) = K, I
S (K K) I = 
inak zapisane K
-}

{-
K = true
false = λx.λy.y
λx.λy.y = λx.I = K I

-}

-- λx.λy.λz.((x z) (y z))
s = \x -> \y -> \z -> ((x z) (y z))


{- Church zero
\f.\x.x    -- skoro ako K ale nie je to K...
\f.\x.x = "I" 
\f.I = "K"
K I
-}
zero  = k i  

{- Church one
λf.λx.(f x) -> SKI

na prednaske bolo λx.λy.(y x)- takze to asi bude podobne...

λf.λx.(f x) = ... S
λf. ((S (λx.f)) (λx.x)) = I
λf. ((S (λx.f)) I) = ... K
λf. ((S (K f)) I) = ... 
S  (λf.(S (K f)) λf.I) = ... K I
S  (λf.(S (K f)) (K I)) = ... 

λf.λx.(f x) = S
λf.(S(λx.f)) (λx.x) = I
λf.(S (λx.f)) I = I
S  (λf.(S (λx.f))) (λf.I) = K
S  (λf.(S (λx.f))) (K I) = K
S  (S (λf.S) (λf.(λx.f))) (K I) = K
S  (S (K S) (λf.(λx.f))) (K I) = K
S  (S (K S) (λf.(K f))) (K I) = S
S  (S (K S) (S (λf.K) (λf.f))) (K I) = K, I
S  (S (K S) (S (K K) I)) (K I)

---- ale ved tu sa to dalo kratsie
S  (S (λf.S) (λf.(λx.f))) (K I) = ...
S  (S (K S) K) (K I) 

-}
type Church = (Int -> Int) -> Int -> Int  

one = \f -> \x -> f x  

one' :: Church
one'    = s  (s (k s) (s (k k) i)) (k i)

one'' :: Church
one''   = s  (s (k s) k) (k i) 

one'''  = ((s (k (s ((s k) k)))) k)         -- ((S(K(S((SK)K))))K)   a b 

one''''   = s (s k)                         -- S (S K) a b


one''''' :: Church
one'''''   = i    -- (I f) a = f a


-- inak to je church's one
apply'   = s (s k)                            -- λx.λy.x y 
apply''  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
apply'''  = i

two   = \f -> \x -> f (f x)
two'   = (s (s (k s) k)) i  
three = (s (s (k s) k)) (s (s (k s) k) i)  
four  = (s (s (k s) k)) ((s (s (k s) k)) (s (s (k s) k) i))  

-- integer -> church  
church n = \f -> \x -> iterate f x !! n  

-- n++  (succ z prednasky)
incr = \n -> \f -> \x -> f (n f x)     
incr' n = \f -> \x -> f (n f x)     

add = \m -> \n -> \f -> \x -> m f (n f x)     
add' m n = \f -> \x -> m f (n f x)     
  
-- m*n  
mult = \m -> \n -> \f -> \x -> m (n f) x  
  
-- m^n  
expt = \m -> \n -> n (mult m) one  
   
true  x y = x
false x y = y

-- AND := λx.λy. x y FALSE := λxy.x y FALSE
-- OR := λx.λy. x TRUE y := λxy.x TRUE y
ch_and x y   = x y false
ch_or x y    = x true y
ch_not x     = x false true
ch_xor       = undefined

{- XOR
xy ^
00 0
01 1
10 1
11 0

xor x y = x (not y) y
TRUE (not y) y = not y
FALSE (not y) y = y

-}
-- ch_xor'  x y     = ch_or (ch_and (ch_not x) y) (ch_and (ch_not y) x)
-- ch_xor'' x y = x (ch_not y) y

--PAIR := λx.λy.(λc. c x y) := λxyc. c x y
-- LEFT := λx.x TRUE
-- RIGHT := λx.x FALSE

ch_pair x y  = \c -> c x y
ch_left = \x -> x true
ch_right = \x -> x false

{-
Nil = λz.z TRUE FALSE FALSE
Cons= λx.λy.λz.z FALSE x y

head= λp.p (λx.λy.λz.y)
tail= λp.p (λx.λy.λz.z)
isNil= λp.p (λx.λy.λz.x)
-}
nil       = \z -> z true false false
cons x y  =  \z -> z false x y
ch_head p  = p (\x y z -> y)
ch_tail p  = p (\x y z -> z)
ch_isNil p  = p (\x y z -> x)

-- omega = \x -> (x x)
-- bigomega = omega omega
-- ypsilon = \f x -> (f (x x) f (x x))

{-
len :: (forall t. ((t2 -> t1 -> t2) -> (t3 -> t4 -> t4) -> (t5 -> t6 -> t6) -> t)
     -> t) -> (t->t) -> t -> t        
len lst  =
     ifte (ch_isNil lst)
          zero
          (incr (len (ch_tail lst)))
-}  
  
-- isZero
isZero n = n (\_ -> false) true

decr n =
    n (\m f x-> f (m incr zero))
    zero
    (\x->x)
    zero
    
ifte  c t e = c t e
    
fact :: (forall a. (a->a)->a->a) -> (a->a) -> a -> a        
fact n  =
     ifte (isZero n)
          one
          (mult n (fact (decr n)))

{-
fib :: (forall a. (a->a)->a->a) -> (a->a) -> a -> a        
fib n  =
     ifte (isZero n)
          (ch_pair one one)
          (let pom = fib (decr n) in (ch_pair (ch_right pom) (add (ch_left pom) (ch_right pom)) ))
-}

    
unChurch n = n (+1) (0)  
            
a = church 2  
b = church 10   
  
main = do print $ unChurch $ add a b  
          print $ unChurch $ incr a  
          print $ (isZero $  incr a) "T" "F"  -- false
          print $ (isZero zero) "T" "F"  -- true
          print $ unChurch $ mult a b  
          print $ unChurch $ expt a b 
          print $ unChurch $ i a
          print $ unChurch $ k a b
          print $ unChurch zero  
          print $ unChurch one  
          print $ unChurch one'
          print $ unChurch two  
          print $ unChurch three  
          print $ unChurch four  
          print $ (ch_and true true)"T" "F"
          print $ (ch_and true false)"T" "F"
          print $ (ch_and false false) "T" "F"
          print $ unChurch (ch_left $ ch_pair one two) 
          print $ unChurch $ ch_head (cons one nil)          
          print $ unChurch $ fact b
          print $ unsafeApply apply' (+1) 10   
          print $ unsafeApply apply'' (^2) 10   
          print $ unsafeApply apply''' (subtract 99) 10 
          
          
unsafeApply n a b = unsafeApply' (unsafeCoerce n) a b            
      where unsafeApply' :: Church -> (Int -> Int) -> Int -> Int  
            unsafeApply' n a b = n a b  

 
{-
Či existuje verzia kombinátorovej logiky aj s jedným kombinátorom ?
Je to čisto teoretická otázka, v praxi potrebujeme opak...

Nech X = λx.(x K S K)
potom vieme ukázať, že K = X X X = (X X) X
A tiež, že S = X . X X = X (X X)
Skúste si to ako cvičenie...

Iná možnosť je, ak X = λx.((x S) K)
potom K = X (X (X X))
a S = X (X (X (X X)))
Skúste si to ako cvičenie... 

Môže sa zdať, že ide o čisto teoretický výsledok, ale existuje programovací jazyk 
(Iota - pokročilé čítanie pre extrémisticky ladených nadšencov) 
používajúci X ako jedinú jazykovú konštrukciu.
-}