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
                                 constraints = map (\constraint -> (constraint, Constraint.variables constraint)) $ allDiffConstraints ++ multiplicationConstraints
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

          computeRangs rangs = let updateRangsInfo = [(left, 1), (right, 2), (result, 4)] ++ (map (\intermediate -> (intermediate, 3)) intermediate)
                                   updatedRangs = foldr (\(variables, rang) rangs -> updateRangs rangs variables rang) rangs updateRangsInfo
                                   fillsCount = sum $ map length $ [left, right, result] ++ intermediate
                               in AssocList.fromList $ map (fmap (\(count, rang, index) -> (fillsCount - count, rang, index))) $ AssocList.list updatedRangs
              where updateRangs rangs variables rang = fst $ foldr (updateRangsWithVariable rang) (rangs, 1) variables
          
                    updateRangsWithVariable rang variable (rangs, index) = let (variableCount, variableRang, variableIndex) = fromJust $ AssocList.find variable rangs
                                                                               updated = if variableCount == 0
                                                                                           then (1, rang, index)
                                                                                           else (variableCount + 1, variableRang, variableIndex)
                                                                               updatedRangs = AssocList.update variable updated rangs
                                                                           in (updatedRangs, index + 1)

          reduceDomains domains = maybe domains reduceWhenFoundOne findOne
              where findOne = let lastLeft = last left
                              in if right == map last intermediate
                                   then Just lastLeft
                                   else fmap fst $ find ((left ==) . snd) $ zip reversedRight intermediate

                    reduceWhenFoundOne one = let reduceDomainsToRightVariables = map fst $ filter ((length left ==) . length . snd) $ zip reversedRight intermediate
                                                 reduceDomainsToVariables = if null reduceDomainsToRightVariables then [] else (head left):reduceDomainsToRightVariables
                                                 reducedDomains = foldr reduceDomain domains reduceDomainsToVariables
                                             in AssocList.update one [1] reducedDomains

                    reduceDomain variable domains = AssocList.update variable [2, 3, 4] domains
          