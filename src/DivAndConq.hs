module DivAndConq where

countAndMerge :: Ord a => [a] -> ([a], Int)
countAndMerge [] = ([], 0)
countAndMerge [a] = ([a], 0)
countAndMerge xs = (sorted, x + y + z)
   where
     (firstHalf, secondHalf) = splittedArray xs
     (firstRecurr, x) = countAndMerge firstHalf
     (secondRecurr, y) = countAndMerge secondHalf
     (sorted, z) = mergeAndCountHalves firstRecurr secondRecurr

splittedArray :: [a] -> ([a], [a])
splittedArray xs = splitAt (length xs `div` 2) xs

mergeAndCountHalves :: Ord a => [a] -> [a] -> ([a], Int)
mergeAndCountHalves []  ys = (ys, 0)
mergeAndCountHalves xs [] = (xs, 0)
mergeAndCountHalves xss@(x:xs) yss@(y:ys)
  | x <= y =
    let (zs, count) = mergeAndCountHalves xs yss
    in (x : zs, count)
  | x > y =
    let (zs, count) = mergeAndCountHalves xss ys
    in (y : zs, count + length xs)


  -- | otherwise = 0

homework :: IO Int
homework = do
  content <- readFile "./src/IntegerArray.txt"
  let array = fmap read (lines content) :: [Double]
  let (_, inversesCount) = countAndMerge array
  return inversesCount


{-
countAndMerge [3,1,2,4]

mergeAndCountHalves [5,6] [3,4]



-}
