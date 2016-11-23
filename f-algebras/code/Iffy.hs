{-# LANGUAGE DeriveFunctor #-}

module Iffy where

import Algebras

data Iffy a b
    = Boolean Bool
    | And b b
    | If b a a
    deriving (Functor, Show)

alg :: Algebra (Iffy (LazyFix Iffy)) Bool
alg (Boolean b) = b
alg (And x y)   = x && y
alg (If p x y)  = eval $ if p then x else y

eval :: LazyFix Iffy -> Bool
eval = lazyCata alg
