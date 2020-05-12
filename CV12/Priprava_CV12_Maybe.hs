module Priprava_CV12_Maybe where
import Data.Maybe
import Control.Monad


e1 = (Just 1) >>= (\x -> Nothing) 
e2 = (Just 1) >>= (\x -> Just (1+x)) 
e3 = Nothing >>= (\x -> Just (1+x)) 

v1 = do { x<-(Just 1); Nothing } 
v2 = do { x<-(Just 1); return (1+x) } 
v3 = do { x<-(return 1); return (1+x) }  
v5 = do { x<-(return 1); return (Just (1+x)) } 
v6 = do { x<-(return 1); return [1+x] } 
v7 = do { x<-Nothing; return (1+x) } 
