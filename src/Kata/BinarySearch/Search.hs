module Kata.BinarySearch.Search (chop)  where

-- Public chop method
chop            :: Int -> [Int] -> Maybe Int
chop _ []      = Nothing
chop key xs    = binarySearch xs key 0 ((length xs) - 1)

binarySearch :: [Int] -> Int -> Int -> Int -> Maybe Int
binarySearch xs key low high
-- Haskell `guards` decide which statement to execute.
    | high < low        = Nothing
    | (xs!!mid) > key   = binarySearch xs key low (mid-1)
    | (xs!!mid) < key   = binarySearch xs key (mid+1) high
    | otherwise         = Just mid
   where
   mid = low + ((high - low) `div` 2)
