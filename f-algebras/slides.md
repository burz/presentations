
# F-Algebras or: How I Learned to Stop Worrying and Love the Type System

#### Anthony Burzillo

----

TODO: Table Of Contents

---

Let's take a look at an Algebra

```Haskell
type Algebra f a = f a -> a
```

What is this doing?

----

```Haskell
type Algebra f a = f a -> a
```

It says how to combine information held
in a functor!

----

For instance consider the following

```Haskell
data AddTree a = Add a a | Only a deriving Functor
```

----

Now we can create an Algebra to do addition

```Haskell
data AddTree a = Add a a | Only a deriving Functor

alg :: Algebra AddTree Int
alg (Add x y) = x + y
alg (Only x) = x
```

Notice this assumes `Add` has only `Int`'s -- no
tree structures here!

---

Next we create this weird type in our system, the `Fix`
type and its corresponding `Fx` constructor.

```Haskell
newtype Fix f = Fx (f (Fix f))
```

----

We can also `unFix` a `Fix`'d type

```Haskell
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x
```

----

Now we are ready for catamorphisms

```Haskell
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
```

---

## Solving a simple expression tree

Suppose that we have the following type

```Haskell
data Ring a
    = Value Int
    | Add a a
    | Multiply a a
    deriving (Show, Functor)
```

This type models a value that can be added or subtracted.

----

How can we solve expressions of this type using catamorphisms?

----

Easy

```Haskell
alg :: Num a => Algebra Ring Int
alg (Value x) = x
alg (Add x y) = x + y
alg (Multiply x y) = x * y

eval :: Num a => Fix Ring a -> a
eval = cata alg
```

----

```Haskell
eval $ Fx Value 5 -- => 5
eval . Fx $ Add (Fx $ Value 3) (Fx $ Value 2)-- => 5
```

---

### What about multiple types?

----

#### Now we need to change our result type

How do we do it?

----

Suppose we have the following

```Haskell
data Basic a
    = Boolean Bool
    | Value Int
    | If a a a
```

Now we can possibly fail if the
first argument of the `If` is not a `Boolean`.

----

How do we handle this possibility of failure in
the algebra?

----

One simple way could be

```Haskell
type Result = Maybe (Either Bool Int)

alg :: Algebra Basic Result
alg (Boolean x) = Just $ Left x
alg (Value x) = Just $ Right x
alg (If c x y) = c >>= \e -> case e of
    Left b -> if b then x else y
    _ -> Nothing

eval :: Fix Basic -> Result
eval = cata alg
```

----

But this means that we always evaluate both sides of the
`If` statement!

What if we want to delay evaluation?

---

### How to delay evaluation

----

To delay evaluation of certain parts of our expressions
we must define a new type of algebra

```Haskell
newtype LazyFix f = Fx' (f (LazyFix f) (LazyFix f))
```

An `unFix`

```Haskell
lazyUnFix :: LazyFix f -> f (LazyFix f) (LazyFix f)
lazyUnFix (Fx' x) = x
```

----

And a catamorphism

```Haskell
lazyCata :: Functor (f (LazyFix f)) => Algebra (f (LazyFix f)) a -> LazyFix f -> a
lazyCata alg = alg . fmap (lazyCata alg) . lazyUnFix
```

----

Now we can solve expressions from the language

```Haskell
data Iffy a b
    = Boolean Bool
    | And b b
    | If b a a
```

very simply

```Haskell
alg :: Algebra (Iffy (LazyFix Iffy)) Bool
alg (Boolean b) = b
alg (And x y) = x && y
alg (If p x y) = eval $ if p then x else y

eval :: LazyFix Iffy -> Bool
eval = lazyCata alg
```

---

### A Brief Excursion in Typechecking

Consider the follwing type

```Haskell
data LittleExpr a b
    = Value Int
    | Add b b
    | Lambda String a
    | Application b b
    deriving (Functor, Show)
```

Note that we will be delaying the typechecking
of the `Lambda`'s expression.

----

How do we type this?

----

One common way of typechecking is through
use of the equational type system. This works
in three steps:

1. Generate equations that must be true according
to the expression.
2. Take the closure of the system of equations under
a few rules.
3. Ensure there are no contradictions.

----

More specifically, we will take the closure of the
system of equations

```
type Type = Integer | Boolean | Arrow Type Type | Var Int

type Equation = (Type, Type)
```

(More on `Var` later) under two rules

1. If `A = B` and `B = C` then `A = C`
2. If `A -> B = C -> D` then `A = C` and `B = D`.

----

Suppose that we have a function that returns the type of
an expression, `typecheck`. It is easy to see how to generate
the equation for an addition

```Haskell
typecheck (Add a b) = (typecheck a, typecheck b)
```

or more efficiently, we could add two equations

```
(Integer, typecheck a)
(Integer, typecheck b)
```

----

But how do we typecheck the lambda function? It has a variable
whose type needs to be found out from the context.

----

In this case we need to generate a type variable as a placeholder
for the type of the named variable in the lambda.

Of course, these generated type variables must be unique. How do
we do that?

----

Let's create a new form of Algebra

```Haskell
type MAlgebra m f a = f (m a) -> m a
```

----

And a two new catamorphisms

```Haskell
mcata :: Functor f => MAlgebra m f a -> Fix f -> m a
mcata alg = alg . fmap (mcata alg) . unFix

lazyMCata :: Functor (f (LazyFix f)) => MAlgebra m (f (LazyFix f)) a -> LazyFix f -> m a
lazyMCata alg = alg . fmap (lazyMCata alg) . lazyUnFix
```

Now we can thread Monads through our evaluation!

----

First let's create a counter to generate type variables

```Haskell
type Counter = State Int

doNothing :: Counter Int
doNothing = state (\i -> (i, i))

newHandle :: Counter Int
newHandle = state (\i -> (i, i + 1))
```

----

Let's define some types for the algebra

```Haskell
import Data.Set as Set

type Equations = Set.Set Equation
type Result = (Type, Equations)

type TypeAlg = MAlgebra Counter (LittleExpr (LazyFix LittleExpr)) Result
type Hypotheses = [(String, Type)]
```

----

Now we can define the algebra

```Haskell
twoAdd :: Equation -> Equation -> Equations -> Equations -> Equations
twoAdd e e' eq eq' = Set.insert e . Set.insert e' $ Set.union eq eq'

alg :: Hypotheses -> TypeAlg
alg g (Value _) = (\_ -> (Integer, Set.empty)) <$> doNothing
alg g (Add x y) = (\(t, e) (t', e') -> (Integer, twoAdd t t' e e') <$> x <*> y
alg g (Lambda n x) = newHandle >>= \v -> let h = Var v
    in genTypes ((n, h) : g) x >>= \(t, e) -> return (Arrow h t, e)
alg g (Application x y) = (\v (t, e) (t', e') -> let h = Var v
    in (h, Set.insert (t, Arrow t' h) $ Set.union e e')) <$> newHandle <*> x <*> y

genTypes :: Hypotheses -> TypeAlg
genTypes h = lazyMCata (alg h)
```
