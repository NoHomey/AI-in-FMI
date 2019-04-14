module MultiplicationCryptogram.Integer ( toNum
                                        , toList
                                        ) where

toNum :: [Int] -> Int
toNum = foldl (\a d -> 10 * a + d) 0

toList :: Int -> [Int]
toList = reverse . toDigits
    where toDigits n = if n < 10
                         then [n]
                         else (n `mod` 10):(toDigits $ n `div` 10)