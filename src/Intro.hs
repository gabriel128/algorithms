module Intro where

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = mergeHalves (mergeSort firstHalf) (mergeSort secondHalf)
   where
     (firstHalf, secondHalf) = splittedArray xs

splittedArray :: [a] -> ([a], [a])
splittedArray xs = splitAt (length xs `div` 2) xs

mergeHalves :: Ord a => [a] -> [a] -> [a]
mergeHalves [] ys = ys
mergeHalves xs [] = xs
mergeHalves xss@(x:xs) yss@(y:ys)
  | x < y = x : mergeHalves xs yss
  | x >= y = y : mergeHalves xss ys

{-

mergeSort [1,3,6,5]
= mergeHalves (mergeSort [1,3]) (mergeSort [6,5])
= mergeHalves (mergeHalves (mergeSort [1]) (mergeSort [3])) ...
= mergeHalves (mergeHalves [1] [3]) (mergeHalves [6] [5])
= mergeHalves [1, 3] [5, 6]

-}
