{-# LANGUAGE DeriveFunctor #-}

module Basic where

import Algebras

data Basic a
    = Boolean Bool
    | Value Int
    | If a a a
    deriving (Functor, Show)

data Result
    = RInteger Int
    | RBoolean Bool
    | Failure
    deriving Show

alg :: Algebra Basic Result
alg (Boolean x) = RBoolean x
alg (Value x)   = RInteger x
alg (If c x y)  = case c of
    RBoolean b -> if b then x else y
    _          -> Failure

eval :: Fix Basic -> Result
eval = cata alg
