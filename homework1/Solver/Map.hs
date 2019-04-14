{-# LANGUAGE MultiParamTypeClasses #-}

module Solver.Map where

import Data.Maybe

class Map map where
    empty :: map key value

    insert :: Ord key => key -> value -> map key value -> map key value

    update :: Ord key => key -> value -> map key value -> map key value
    update = insert
    
    delete :: Ord key => key -> map key value -> map key value

    valueOf :: Ord key => key -> map key value -> Maybe value

    hasValue :: Ord key => key -> map key value -> Bool
    hasValue key evaluation = isJust $ valueOf key evaluation

    value :: Ord key => key -> map key value -> value
    value key evaluation = fromJust $ valueOf key evaluation

    fromFoldable :: (Ord key, Foldable t) => (info -> (key, value)) -> t info -> map key value
    fromFoldable fromInfo info = foldr foldInsert empty info
        where foldInsert info map = let (key, value) = fromInfo info
                                    in insert key value map

    fromKeys :: (Ord key, Foldable t) => (key -> value) -> t key -> map key value
    fromKeys fromKey = fromFoldable (\key -> (key, fromKey key))

    fromList :: (Ord key) => [(key, value)] -> map key value

    toList :: map key value -> [(key, value)]

    keys :: map key value -> [key]

    null :: map key value -> Bool
    
    pairMap :: ((key, value) -> result) ->  map key value -> [result]
    pairMap f m = map f $ toList m
