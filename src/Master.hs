module Master where

import System.Random
import Data.List
import Data.Maybe

quickSort :: RandomGen b => Ord a => [a] -> b -> [a]
quickSort [] _ = []
quickSort [a] _ = [a]
quickSort xs seed = quickSort lessThanPivot seed' ++ [p] ++  quickSort greaterThanPivot seed'
  where
    (rand, seed') = randomR (0, length xs - 1) seed
    (lys, p:rys) = splitAt rand xs
    list = lys ++ rys
    lessThanPivot = filter (< p) list
    greaterThanPivot = filter (>= p) list

rselect :: RandomGen b => [Int] -> Int -> b -> Maybe Int
rselect [] _ _ = Nothing
rselect _ 0 _ = Nothing
rselect [a] _ _ = Just a
rselect xs index seed
      | i >= length xs = Nothing
      | j == i = Just p
      | j > i = rselect lessThanPivot index seed'
      | j < i = rselect greaterThanPivot (i-j) seed'
  where
    i = index - 1
    (rand, seed') = randomR (0, length xs - 1) seed
    (lys, p:rys) = splitAt rand xs
    list = lys ++ rys
    lessThanPivot = filter (< p) list
    greaterThanPivot = filter (> p) list
    j = length lessThanPivot

mainS =
  do
    seed <- newStdGen
    print (rselect [4,1,2,5,3,6,7] 0 seed)
    seed1 <- newStdGen
    print (rselect [4,1,2,5,3,6,7] 1 seed1)
    seed2 <- newStdGen
    print (rselect [4,1,2,5,3,6,7] 2 seed2)
    seed3 <- newStdGen
    print (rselect [4,1,2,5,3,6,7] 3 seed3)
    seed4 <- newStdGen
    print (rselect [4,1,2,5,3,6,7] 4 seed4)
    seed5 <- newStdGen
    print (rselect [4,1,2,5,3,6,7] 5 seed5)
    seed6 <- newStdGen
    print (rselect [4,1,2,5,3,6,7] 6 seed6)
    seed7 <- newStdGen
    print (rselect [4,1,2,5,3,6,7] 7 seed7)
    seed8 <- newStdGen
    print (rselect [4,1,2,5,3,6,7] 8 seed8)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery i xs = first : splitEvery i rest
  where
    (first, rest) = splitAt i xs

medianOfSorted :: [Int] -> Int
medianOfSorted [a] = a
medianOfSorted [a, _] = a
medianOfSorted xs = x
  where (_, x:_) = splitAt ((length xs - 1)  `div` 2) xs

dselect :: [Int] -> Int -> Maybe Int
dselect [] _  = Nothing
dselect _ 0  = Nothing
dselect [a] _  = Just a
dselect xs index
      | i >= length xs = Nothing
      | j == i = Just p
      | j > i = dselect lessThanPivot index
      | j < i = dselect greaterThanPivot (i-j)
      | otherwise = Nothing
  where
    i = index - 1
    medians = fmap (medianOfSorted .  sort) . splitEvery 5 $ xs
    p = fromMaybe 0 $ dselect medians ((index `div` 10) + 1)
    lessThanPivot = filter (< p) xs
    greaterThanPivot = filter (> p) xs
    j = length lessThanPivot

mainD = do
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 0)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 1)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 2)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 3)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 4)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 5)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 6)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 7)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 8)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 9)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 10)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 11)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 12)
  print (dselect [1,28,8,9,10,50,3,4,5,6,11,7] 13)

main =
  do
    seed <- newStdGen
    return (quickSort [4,1,2,4,5,2] seed)
