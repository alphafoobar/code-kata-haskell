module Kata.BinarySearch.Search (chop)  where

-- Public chop method
chop            :: Int -> [Int] -> Maybe Int
chop _ []      = Nothing
chop key xs    = binarySearch xs key 0 ((length xs) - 1)

binarySearch :: [Int] -> Int -> Int -> Int -> Maybe Int
binarySearch xs key low high
-- Haskell `guards` decide which statement to execute.
    | high < low        = Nothing
    | otherwise         = binarySearch_deep xs key mid low high
   where
   mid = low + ((high - low) `div` 2)

binarySearch_deep :: [Int] -> Int -> Int -> Int -> Int -> Maybe Int
binarySearch_deep xs key mid low high
-- Use calculated mid to get value.
    | value > key   = binarySearch xs key low (mid-1)
    | value < key   = binarySearch xs key (mid+1) high
    | otherwise     = Just mid
   where
   value = xs!!mid
