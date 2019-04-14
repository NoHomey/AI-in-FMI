{-# LANGUAGE MultiParamTypeClasses #-}

module MultiplicationCryptogram.Constraint ( Variable(..)
                                           , Constraint(..)
                                           , Expression(..)
                                           , Equation(..)
                                           , variables
                                           ) where

import qualified Solver.Constraint as SolverConstraint
import qualified Solver.Map as SolverMap
import MultiplicationCryptogram.Integer ( toNum )
import Data.List
import Data.Either
import Data.Maybe

data Variable = A | B | C | D | E | F | G | H | I deriving (Eq, Ord, Show)

data Expression = Constant Int | Variable Variable | Number [Either Int Variable] | Multiplication Expression Expression | Sum [Expression] deriving (Show, Eq)

data Equation = Diff | Equal | EqualMod Int deriving (Show, Eq)

data Constraint = Constraint { left :: Expression
                             , equation :: Equation
                             , right :: Expression
                             } deriving (Show, Eq)

instance SolverConstraint.Constraint Constraint Variable Int where
    bindValues (Constraint left equation right) evaluation = Constraint (bindValuesOfExpression left evaluation) equation (bindValuesOfExpression right evaluation)
        where bindValuesOfExpression expression@(Constant _)        _          = expression
              bindValuesOfExpression expression@(Variable variable) evaluation = maybe expression Constant $ SolverMap.valueOf variable evaluation
              bindValuesOfExpression (Number digits)                evaluation = let bindedDigits = map (bindValueOfDigit evaluation) digits
                                                                                 in if all isLeft bindedDigits
                                                                                      then Constant $ toNum $ map (fromLeft undefined) bindedDigits
                                                                                      else Number bindedDigits
              bindValuesOfExpression (Multiplication left right)    evaluation = let bindedLeft = bindValuesOfExpression left evaluation
                                                                                     bindedRight = bindValuesOfExpression right evaluation
                                                                                 in if isConstant bindedLeft && isConstant bindedRight
                                                                                      then Constant $ (valueOfConstant bindedLeft) * (valueOfConstant bindedRight)
                                                                                      else Multiplication bindedLeft bindedRight
              bindValuesOfExpression (Sum expressions)              evaluation = let bindedExpressions = map (\expression -> bindValuesOfExpression expression evaluation) expressions
                                                                                 in if all isConstant bindedExpressions
                                                                                      then Constant $ sum $ map valueOfConstant bindedExpressions
                                                                                      else Sum bindedExpressions

              bindValueOfDigit _          digit@(Left _)         = digit
              bindValueOfDigit evaluation digit@(Right variable) = maybe digit Left $ SolverMap.valueOf variable evaluation

              isConstant (Constant _) = True
              isConstant _            = False

              valueOfConstant (Constant constant) = constant
              valueOfConstant expression         = error $ "Expression: " ++ (show expression) ++ " is not a Constant!"

    check (Constraint left equation right) evaluation = equationFunc equation (eval left evaluation) (eval right evaluation)
        where equationFunc Diff         = (/=)
              equationFunc Equal        = (==)
              equationFunc (EqualMod n) = \a b -> (a `mod` n) == (b `mod` n)

              eval (Constant constant)         _          = constant
              eval (Variable variable)         evaluation = SolverMap.value variable evaluation
              eval (Number digits)             evaluation = toNum $ map (evalDigit evaluation) digits
              eval (Multiplication left right) evaluation = (eval left evaluation) * (eval right evaluation)
              eval (Sum expressions)           evaluation = sum $ map (\expression -> eval expression evaluation) expressions

              evalDigit evaluation digit = either id (\variable -> SolverMap.value variable evaluation) digit 

variables :: Constraint -> [Variable]
variables (Constraint left equation right) = twoExpressionsVariables left right
    where expressionVariables (Constant _)                = []
          expressionVariables (Variable variable)         = [variable]
          expressionVariables (Number digits)             = concatMap (either (const []) pure) digits
          expressionVariables (Multiplication left right) = twoExpressionsVariables left right
          expressionVariables (Sum expressions)           = nub $ concatMap expressionVariables expressions

          twoExpressionsVariables left right = nub $ (expressionVariables left) ++ (expressionVariables right)