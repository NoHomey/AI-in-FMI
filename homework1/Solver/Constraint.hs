{-# LANGUAGE MultiParamTypeClasses #-}

module Solver.Constraint ( Constraint
                         , bindValues
                         , check
                         ) where

import qualified Solver.Map as SolverMap

class Constraint constraint variable value where
    bindValues :: (Ord variable, SolverMap.Map evaluation)
               => constraint -> evaluation variable value -> constraint
               
    check :: (Ord variable, SolverMap.Map evaluation)
          => constraint -> evaluation variable value -> Bool