module Main where

import qualified Solver.Map as SolverMap
import qualified Data.AssocList as AssocList
import qualified MultiplicationCryptogram.Constraint as Constraint
import qualified MultiplicationCryptogram.Problem as Problem
import MultiplicationCryptogram.InitialInformation (initialInformation)
import Solver.Algorithm (solveCSP)

instance SolverMap.Map AssocList.AssocList where
    empty = AssocList.empty
    insert = AssocList.insert
    valueOf = AssocList.find
    delete = AssocList.delete
    fromList = AssocList.fromList
    toList = AssocList.list
    keys = AssocList.keys
    null = AssocList.null

solve :: Problem.Problem Constraint.Variable -> [AssocList.AssocList Constraint.Variable Int]
solve = solveCSP initialInformation 

problem :: Problem.Problem Constraint.Variable
problem = Problem.Problem { Problem.left = [Constraint.A, Constraint.A]
                          , Problem.right = [Constraint.B, Constraint.B]
                          , Problem.intermediate = [ [Constraint.B, Constraint.B] 
                                                   , [Constraint.B, Constraint.B]
                                                   ]
                          , Problem.result = [Constraint.B, Constraint.C, Constraint.B]
                          }

mainProblem :: Problem.Problem Constraint.Variable
mainProblem = Problem.Problem { Problem.left = [Constraint.A, Constraint.B, Constraint.C]
                              , Problem.right = [Constraint.D, Constraint.E, Constraint.B]
                              , Problem.intermediate = [ [Constraint.A, Constraint.B, Constraint.C]
                                                       , [Constraint.I, Constraint.A, Constraint.G] 
                                                       , [Constraint.E, Constraint.H, Constraint.F, Constraint.A]
                                                       ]
                              , Problem.result = [Constraint.E, Constraint.D, Constraint.B, Constraint.D, Constraint.F, Constraint.C]
                              }

main = do
         mapM_ print $ solve problem
         --mapM_ print $ solve mainProblem
