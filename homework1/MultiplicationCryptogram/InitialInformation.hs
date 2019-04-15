module MultiplicationCryptogram.InitialInformation ( initialInformation ) where

import qualified MultiplicationCryptogram.Constraint as Constraint
import qualified MultiplicationCryptogram.Problem as Problem
import qualified Data.AssocList as AssocList
import Data.Maybe
import Data.List

initialInformation :: Problem.Problem Constraint.Variable -> ( [(Constraint.Constraint, [Constraint.Variable])]
                                                             , AssocList.AssocList Constraint.Variable (Int, Int, Int)
                                                             , AssocList.AssocList Constraint.Variable [Int]
                                                             )
initialInformation problem = let rangs = computeRangs $ AssocList.fromList $ map (\variable -> (variable, (0, 0, 0))) variables
                                 constraints = map (\constraint -> (constraint, Constraint.variables constraint)) $ concat [allDiffConstraints, multiplicationConstraints, sumConstraints]
                                 domains = reduceDomains $ AssocList.fromList $ map (\variable -> (variable, [1..9])) variables
                             in (constraints, rangs, domains)
    where left = Problem.left problem

          right = Problem.right problem

          reversedRight = reverse right

          intermediate = Problem.intermediate problem

          result = Problem.result problem

          variables = Problem.variables problem

          allDiffConstraints = concat $ snd $ foldl diffConstraints (tail variables, []) variables
              where diffConstraints (variables, constraints) variable = (tail variables, (map (diff variable) variables):constraints)
          
                    diff variableI variableJ = Constraint.Constraint (Constraint.Variable variableI) Constraint.Diff (Constraint.Variable variableJ)
          
          multiplicationConstraints = let resultConstraint = Constraint.Constraint (Constraint.Multiplication (number left) (number right)) Constraint.Equal (number result)
                                      in resultConstraint:(nub $ concatMap (constraints left) $ zip reversedRight intermediate)
              where constraints left (multiplier, result) = let tailsResult = init $ tails result
                                                                tempTailsLeft = init $ tails left
                                                                tailsLeft = if (length tailsResult) > (length tempTailsLeft) 
                                                                              then (head tempTailsLeft):tempTailsLeft
                                                                              else tempTailsLeft
                                                                zippedTails = zip tailsLeft tailsResult
                                                                (headLeft, headResult) = head zippedTails
                                                                zippedTailsTail = tail zippedTails
                                                                wholeMultiplication = constraint multiplier headLeft Constraint.Equal headResult
                                                                modMultiplications = fst $ foldr (modConstraint multiplier) ([], 10) zippedTailsTail 
                                                            in wholeMultiplication:modMultiplications
                    constraint multiplier left equation result = Constraint.Constraint (Constraint.Multiplication (Constraint.Variable multiplier) (number left)) equation (number result)

                    modConstraint multiplier (left, result) (constraints, n) = ((constraint multiplier left (Constraint.EqualMod n) result):constraints, 10 * n)

                    number digits = Constraint.Number $ map Right digits

          sumConstraints = let resultLength = length result
                               matrix = fst $ foldr (\row (matrix, offset) -> (((fill $ resultLength - (length row + offset)) ++ (map Right row) ++ (fill offset)):matrix, offset - 1)) ([], resultLength - (length $ last intermediate)) intermediate
                               columnsTails = init $ init $ tail $ tails $ columns matrix
                               resultTails = init $ init $ tail $ tails result
                               sumModConstraints = fst $ foldr modConstraints ([], 100) $ zip columnsTails resultTails
                               sumAllConstraint = constraint matrix Constraint.Equal $ map Right result
                           in sumAllConstraint:sumModConstraints
              where constraint numbers equation result = Constraint.Constraint (Constraint.Sum $ map Constraint.Number numbers) equation (Constraint.Number result)
                  
                    modConstraints (columns, result) (constraints, n) = ((constraint (matrixFromColumns columns) (Constraint.EqualMod n) (map Right result)):constraints, 10 * n)

                    columns matrix = if all null matrix
                                       then []
                                       else (map head matrix):(columns $ map tail matrix)

                    matrixFromColumns [column] = map (\element -> [element]) column
                    matrixFromColumns (head:tail) = zipWith (:) head $ matrixFromColumns tail

                    fill n = replicate n $ Left 0                   

          computeRangs rangs = let updateRangsInfo = [(left, 1), (right, 2)] ++ (map (\intermediate -> (intermediate, 3)) intermediate) ++ [(result, 4)]
                                   updatedRangs = foldl (\rangs (variables, rang) -> updateRangs rangs variables rang) rangs updateRangsInfo
                                   fillsCount = sum $ map length $ [left, right, result] ++ intermediate
                               in AssocList.fromList $ map (fmap (\(count, rang, index) -> (fillsCount - count, rang, index))) $ AssocList.list updatedRangs
              where updateRangs rangs variables rang = fst $ foldr (updateRangsWithVariable rang) (rangs, 1) variables
          
                    updateRangsWithVariable rang variable (rangs, index) = let (variableCount, variableRang, variableIndex) = fromJust $ AssocList.find variable rangs
                                                                               updated = if variableCount == 0
                                                                                           then (1, rang, index)
                                                                                           else (variableCount + 1, variableRang, variableIndex)
                                                                               updatedRangs = AssocList.update variable updated rangs
                                                                           in (updatedRangs, index + 1)

          reduceDomains domains = let reducedDomains = reduceDomainOfLastFromLeftIfPossible
                                  in maybe reducedDomains (reduceWhenFoundOne reducedDomains) findOne
              where findOne = fmap fst $ find ((left ==) . snd) $ zip reversedRight intermediate
                   
                    reduceDomainOfLastFromLeftIfPossible = let lastLeft = last left
                                                           in if any (\(x, y) -> x == y) $ zip reversedRight $ map last intermediate
                                                                then AssocList.update lastLeft [1, 3, 5, 6, 7, 9] domains
                                                                else domains

                    reduceWhenFoundOne domains one = let reduceDomainsToRightVariables = map fst $ filter (condition one) $ zip reversedRight intermediate
                                                         reduceDomainsToVariables = if null reduceDomainsToRightVariables then [] else (head left):reduceDomainsToRightVariables
                                                         reducedDomains = foldr reduceDomain domains reduceDomainsToVariables
                                                     in AssocList.update one [1] reducedDomains
                    condition one (variable, intermediate) = (length left == length intermediate) && (variable /= one) && (head left /= one)

                    reduceDomain variable domains = AssocList.update variable [2, 3, 4] domains
          