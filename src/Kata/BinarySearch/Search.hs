module Kata.BinarySearch.Search (chop)  where

import Data.Char

chop :: String -> String
chop = dropWhile isSpace . reverse . dropWhile isSpace . reverse
