module Solutions.RangeExtractor (solution) where

import Data.List (foldl', sort, foldl1')

mergeRange :: [[Integer]] -> Integer -> [[Integer]]
mergeRange [] i = [[i]]
mergeRange ((x:xs') : xs) i
    | x + 1 == i = (i : x : xs') : xs
mergeRange xs i = [i]:xs

showRange :: [[Integer]] -> String
showRange [] = ""
showRange xs = foldl1' (\x y -> x ++ "," ++ y) . reverse . map showRange' $ xs
  where
    showRange' [x] = show x
    showRange' [x, y] = show y ++ "," ++ show x
    showRange' l = show (last l) ++ "-" ++ show (head l)

solution :: [Integer] -> String
solution = showRange . foldl' mergeRange [] . sort
