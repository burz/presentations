{-# LANGUAGE DeriveFunctor #-}

module BiggerExpr where

import Algebras

data LittleExpr a b
    = Var String
    | Value Int
    | Add b b
    | Lambda String a
    | Application b b
    deriving (Functor, Show)

data BiggerExpr a
    = BVar String
    | BValue Int
    | BAdd a a
    | BLambda String a
    | BApplication a a
    | Let [String] a a
    deriving (Functor, Show)

transform :: [String]
          -> LazyFix LittleExpr
          -> LazyFix LittleExpr
          -> LazyFix LittleExpr
transform [] _ _        = undefined
transform (f : xs) e e' = Fx' $ Application right left
    where right = Fx' $ Lambda f e'
          left  = foldr (\n -> Fx' . Lambda n) e xs

-- (\f -> e') (\x -> \y -> \z -> e)

alg :: Algebra BiggerExpr (LazyFix LittleExpr)
alg (BVar s)           = Fx' $ Var s
alg (BValue v)         = Fx' $ Value v
alg (BAdd x y)         = Fx' $ Add x y
alg (BLambda x e)      = Fx' $ Lambda x e
alg (BApplication f x) = Fx' $ Application f x
alg (Let xs e e')      = transform xs e e'

eval :: Fix BiggerExpr -> LazyFix LittleExpr
eval = cata alg
