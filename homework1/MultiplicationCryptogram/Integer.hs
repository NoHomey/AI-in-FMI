module MultiplicationCryptogram.Integer ( toNum ) where

toNum :: [Int] -> Int
toNum = foldl (\a d -> 10 * a + d) 0
