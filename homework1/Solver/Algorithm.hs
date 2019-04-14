module Solver.Algorithm (solveCSP) where

import qualified Solver.Map as SolverMap
import qualified Solver.Constraint as Constraint
import Solver.ArcConsistancy as ArcConsistancy
import Data.Maybe
import Data.List

solveCSP :: ( Ord variable
            , Ord rang
            , Eq value
            , SolverMap.Map link
            , SolverMap.Map evaluation
            , Constraint.Constraint constraint variable value
            )
         => (problem -> ([(constraint, [variable])], link variable rang, link variable [value]))
         -> problem
         -> [evaluation variable value]
solveCSP initial problem = let (constraints, rangs, domains) = initial problem
                           in solutions constraints rangs domains SolverMap.empty
    where solutions constraints rangs domains evaluation = let (singleConstraints, restConstraints) = partitionConstraintsOnVariableCount 1 constraints
                                                               constrainedDomains = constrainDomains singleConstraints domains evaluation
                                                               continueSolve = solutionsFromSingleConsistancy restConstraints rangs evaluation
                                                           in maybe [] continueSolve constrainedDomains

          constrainDomains []                domains _          = Just domains
          constrainDomains (constraint:rest) domains evaluation = let variable = head $ snd constraint
                                                                      constraintToCheck = fst constraint
                                                                      domain = SolverMap.value variable domains
                                                                      constrainedDomain = filter (\value -> Constraint.check constraintToCheck $ SolverMap.insert variable value evaluation) domain
                                                                  in if null constrainedDomain
                                                                       then Nothing
                                                                       else constrainDomains rest (SolverMap.update variable constrainedDomain domains) evaluation

          solutionsFromSingleConsistancy constraints rangs evaluation domains = let variables = SolverMap.keys rangs
                                                                                    twoVariableConstraints = fst $ partitionConstraintsOnVariableCount 2 constraints
                                                                                    arcConstraints = map (\(constraint, [variableI, variableJ]) -> (Constraint.bindValues constraint evaluation, ArcConsistancy.Link variableI variableJ)) twoVariableConstraints
                                                                                    constrainedDomains = ArcConsistancy.arcConsistancy variables domains arcConstraints evaluation
                                                                                    continueSolve = nextVariableToInstantiate constraints rangs evaluation
                                                                                in maybe [] continueSolve constrainedDomains

          nextVariableToInstantiate constraints rangs evaluation domains = if SolverMap.null rangs
                                                                             then [evaluation]
                                                                             else let rankedVariables = SolverMap.pairMap (\(variable, rang) -> (variable, (length $ SolverMap.value variable domains, rang))) rangs
                                                                                      bestVariable = fst $ minimumBy (\l r -> compare (snd l) (snd r)) rankedVariables
                                                                                      domain = SolverMap.value bestVariable domains
                                                                                      updatedRangs = SolverMap.delete bestVariable rangs
                                                                                      updatedConstraints = map (fmap (delete bestVariable)) constraints
                                                                                      instantiate = instantiateVariable updatedConstraints updatedRangs domains evaluation bestVariable
                                                                                  in concatMap instantiate domain

          instantiateVariable constraints rangs domains evaluation variable value = solutions constraints rangs domains $ SolverMap.insert variable value evaluation
          
          partitionConstraintsOnVariableCount count = partition (\constraint -> (length $ snd constraint) == count)