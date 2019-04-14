import Data.List
import Data.Maybe
import MultiplicationCryptogram.Problem (Problem(Problem), left, right, variables)
import MultiplicationCryptogram.Integer (toNum, toList)

directSubstitution :: Problem Char -> [(Char, Int)] -> Problem Int
directSubstitution problem values = fmap substitude problem
    where substitude variable = fromJust $ lookup variable values

compute :: [Int] -> [Int] -> Problem Int
compute left right = let leftNum = toNum left
                         rightNum = toNum right
              in Problem left right (reverse $ map (\mul -> toList $ mul * leftNum) right) (toList $ leftNum * rightNum)

solutions :: Problem Char -> [[(Char, Int)]]
solutions problem = map (zip vars) $ filter test $ permutations [1..9]
    where vars = variables problem
          test values = let direct = directSubstitution problem $ zip vars values
                            computed = compute (left direct) (right direct)
                        in direct == computed 

problem :: Problem Char
problem = Problem "ABC" "DEB" ["ABC", "IAG", "EHFA"] "EDBDFC"

main = mapM_ print $ nub $ solutions problem