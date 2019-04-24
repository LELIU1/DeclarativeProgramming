--File    : Assignment1.hs
--Author  : LELIU(1024780)
--Origin  : Fri Mar 15 12:00:00 2019
--Purpose : Pass the assignment1
--
--This code write three functions. 
--Function1 
--subst takes two values and a list, and replaces every occurrence of the first value with the second in the list. 
--Function2 interleave 
--interleave takes two lists and returns the interleaving of the two arguments. That, the result is a list in which the first, third, fifth …elements come fromt the first argument and the second, fourth, sixth …come from second. If either argument is shorter than the other, the excess elements of the longer comprise the end of the resulting list. 
--Function3 unroll
--unroll takes a list and an integer and constructs a list of the specified length made up by “unrolling” the input list as many times as needed to construct a list of that length. That is, the output consists of the input list repeated as many times as necessary to have the specified length.

module Assignment1 (subst, interleave, unroll) where

subst :: Eq t => t -> t -> [t] -> [t]
subst a b [] = []
subst a b [x]
  | x == a = [b]
  | otherwise = [x]
subst a b (x:xs) 
  | x == a = b:subst a b xs
  | otherwise = x:subst a b xs

interleave :: [t] -> [t] -> [t]
interleave [] [] = []
interleave [] (y:ys) = y:ys
interleave (x:xs) [] = x:xs
interleave [x] [y] = [x,y]
interleave (x:xs) [y] = x:y:xs
interleave [x] (y:ys) = x:y:ys
interleave (x:xs) (y:ys) = x:y:interleave xs ys

unroll :: Int -> [a] -> [a]
len [] = 0
len (x:xs) = 1 + len xs
unroll n _
  | n <= 0  = []
unroll n [x] = x:unroll (n-1) [x]
unroll n (x:xs) 
  | n <= len(x:xs) = x:unroll (n-1) xs
  | otherwise = x:xs ++ unroll (n-len(x:xs)) (x:xs)
