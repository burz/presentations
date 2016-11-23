{-# LANGUAGE DeriveFunctor #-}

module AddTree where

import Algebras

data AddTree a
    = Only a
    | Add a a
    deriving (Functor, Show)

alg :: Algebra AddTree Int
alg (Only x)  = x
alg (Add x y) = x + y
