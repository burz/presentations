{-# LANGUAGE DeriveFunctor #-}

module LittleExpr where

import Data.Set

import Control.Monad.State
--
-- NOTE: this depends on mtl
--
-- This can be installed/run by the following:
--   stack install mtl
--   stack ghci
--   :load Algebras.hs LittleExpr.hs
--

import Algebras

data LittleExpr a b
    = Var String
    | Value Int
    | Add b b
    | Lambda String a
    | Application b b
    deriving (Functor, Show)

data Type
    = Integer
    | Boolean
    | Arrow Type Type
    | TypeVar Int
    | Undefined String
    deriving (Eq, Ord, Show)

type Equation = (Type, Type)

type Counter = State Int

newHandle :: Counter Int
newHandle = state (\i -> (i, i + 1))

type Equations = Set Equation
type Result = (Type, Equations)

type TypeAlg
    = MAlgebra Counter (LittleExpr (LazyFix LittleExpr)) Result

type Hypotheses = [(String, Type)]

lookupTypeVar :: String -> Hypotheses -> Type
lookupTypeVar s []             = Undefined s
lookupTypeVar s ((s', t) : xs) = if s == s'
    then t
    else lookupTypeVar s xs

twoAdd :: Equation
       -> Equation
       -> Equations
       -> Equations
       -> Equations
twoAdd e e' eq eq' = insert e . insert e' $ union eq eq'

alg :: Hypotheses -> TypeAlg
alg g (Var s)   = return (lookupTypeVar s g, empty)
alg _ (Value _) = return (Integer, empty)
alg _ (Add x y) = (\(t, e) (t', e') ->
    (Integer, twoAdd (Integer, t) (Integer, t') e e')) <$> x <*> y
alg g (Lambda n x) = newHandle >>= \v -> let h = TypeVar v
    in genTypes ((n, h) : g) x >>= \(t, e) ->
        return (Arrow h t, e)
alg _ (Application x y) = (\v (t, e) (t', e') -> let h = TypeVar v
        in (h, insert (t, Arrow t' h) $ union e e'))
    <$> newHandle <*> x <*> y

genTypes :: Hypotheses -> LazyFix LittleExpr -> Counter Result
genTypes h = lazyMCata (alg h)

typecheck :: LazyFix LittleExpr -> Bool
typecheck e = let eq = evalState (genTypes [] e) 0
    in let c = closure eq
    in isConsistent c
    where closure = id
          isConsistent _ = True

-- closure and isConsistent are out of the scope of this
-- presentation.
