{-# LANGUAGE DeriveFunctor #-}

module Basic where

import Algebras

data Basic a
    = Boolean Bool
    | Value Int
    | If a a a
    deriving (Functor, Show)

type Result = Maybe (Either Bool Int)

alg :: Algebra Basic Result
alg (Boolean x) = Just $ Left x
alg (Value x)   = Just $ Right x
alg (If c x y)  = c >>= \e -> case e of
    Left b -> if b then x else y
    _      -> Nothing

eval :: Fix Basic -> Result
eval = cata alg
