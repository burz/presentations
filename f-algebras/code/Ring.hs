{-# LANGUAGE DeriveFunctor #-}

module Ring where

import Algebras

data Ring a
    = Value Int
    | Add a a
    | Multiply a a
    deriving (Functor, Show)

alg :: Algebra Ring Int
alg (Value x)      = x
alg (Add x y)      = x + y
alg (Multiply x y) = x * y

eval :: Fix Ring -> Int
eval = cata alg

-- eval . Fx $ Value 5 -- => 5
-- eval . Fx $ Add (Fx $ Value 3) (Fx $ Value 2) -- => 5
