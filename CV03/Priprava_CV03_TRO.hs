-- cela pravda o Tail Recursion Optimisation

-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn

-- foldr je rekurzia

e = [1..10^8]

-- otvorte si Task Manager a pozerajte pamat
foldr (+) 0 e
-- *** Exception: stack overflow

foldl (+) 0 e
*** Exception: stack overflow
-- vyrobi vyraz ale je lazy... (...((0 + 1) + 2) +...) + 10^8

import Data.List

:set +set


Prelude Data.List> foldl' (+) 0 [1..10^8]
5000000050000000
(5.24 secs, 8,800,063,320 bytes)

Prelude Data.List> foldl' (+) 0 [1..10^9]
500000000500000000
(45.33 secs, 88,000,061,864 bytes)

existuju 
- lazy verzie (nevyhodnocuju vyraz, az kym fakt netreba): 
	foldl, scanl, iterate
- striktne verzie (vyhodnocuju vyrazy hned, ako java/c):
	foldl', scanl', iterate'
	
	
