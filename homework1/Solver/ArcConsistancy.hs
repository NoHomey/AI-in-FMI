module Solver.ArcConsistancy ( Link(..)
                             , arcConsistancy
                             ) where

import qualified Solver.Map as SolverMap
import qualified Solver.Constraint as Constraint

data Link variable = Link variable variable

arcConsistancy :: ( Ord variable
                  , Eq value
                  , SolverMap.Map link
                  , SolverMap.Map evaluation
                  , Constraint.Constraint constraint variable value
                  )
               => [variable]
               -> link variable [value]
               -> [(constraint, Link variable)]
               -> evaluation variable value
               -> Maybe (link variable [value])
arcConsistancy _         domains []          _          = Just domains
arcConsistancy variables domains constraints evaluation = let graph = graphFromArcConstraints variables constraints
                                                          in arcConsistancyFromGraph graph domains evaluation
    where arcConsistancyFromGraph graph domains evaluation = runQueue graph domains evaluation $ initialQueue graph

          initialQueue graph = SolverMap.pairMap constraintLinks graph
          
          constraintLinks (variableI, links) = SolverMap.pairMap (\(variableJ, constraint) -> (variableI, variableJ, constraint)) links     

runQueue :: ( Ord variable
            , Eq value
            , SolverMap.Map link
            , SolverMap.Map evaluation
            , Constraint.Constraint constraint variable value
            )
         => link variable (link variable constraint)
         -> link variable [value]
         -> evaluation variable value
         -> [[(variable, variable, constraint)]]
         -> Maybe (link variable [value])
runQueue graph domains evaluation queue = run domains queue
    where run domains []                                                = Just domains
          run domains ([]:queue)                                        = run domains queue
          run domains (((variableI, variableJ, constraint):rest):queue) = let domainI = SolverMap.value variableI domains
                                                                              domainJ = SolverMap.value variableJ domains
                                                                              constrainedDomainI = constrainDomain constraint (variableI, domainI) (variableJ, domainJ)
                                                                          in if null constrainedDomainI
                                                                               then Nothing
                                                                               else if constrainedDomainI == domainI
                                                                                      then run domains $ rest:queue
                                                                                      else let updatedDomains = SolverMap.update variableI constrainedDomainI domains
                                                                                               tail = constraintsForAffectedVariables variableJ variableI
                                                                                            in run updatedDomains $ rest:(queue ++ [tail])

          constrainDomain constraint (variableI, domainI) (variableJ, domainJ) = filter hasSupportingValue domainI
              where hasSupportingValue valueI = let evaluationI = SolverMap.insert variableI valueI evaluation
                                                in any (\valueJ -> Constraint.check constraint $ SolverMap.insert variableJ valueJ evaluationI) domainJ

          constraintsForAffectedVariables variableJ variableI = let affectedLinks = SolverMap.delete variableJ $ SolverMap.value variableI graph
                                                                in SolverMap.pairMap (\(variableK, constraint) -> (variableK, variableI, constraint)) affectedLinks

graphFromArcConstraints :: (Ord variable, SolverMap.Map link)
                        => [variable]
                        -> [(constraint, Link variable)]
                        -> link variable (link variable constraint)
graphFromArcConstraints variables constraints = SolverMap.fromKeys links variables                     
    where links variable = SolverMap.fromFoldable (link variable) $ filter (isConstraintOnVariable variable) constraints

          isConstraintOnVariable variable (_, linkedVariables) = isInConstraintVariables variable linkedVariables

          isInConstraintVariables variable (Link a b) = a == variable || b == variable
          
          link variable (constraint, linkedVariables) = (linkTo variable linkedVariables, constraint)
          linkTo variable (Link a b) = if variable == a then b else a
