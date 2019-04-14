{-# LANGUAGE DeriveFunctor #-}

module MultiplicationCryptogram.Problem where

import Data.List

data Problem variable = Problem { left :: [variable]
                         , right :: [variable]
                         , intermediate :: [[variable]]
                         , result :: [variable]
                         } deriving (Eq, Functor, Show)

variables :: Eq variable => Problem variable -> [variable]
variables (Problem left right intermediate result) = nub $ concat $ left:right:result:intermediate