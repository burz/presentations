
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

---

Now we are ready for catamorphisms

```Haskell
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
```

---

Let's think about how we can use these F-Algebras to help
us solve a simple expression tree.

----

Suppose that we have the following type

```Haskell
data Ring a
    = Value a
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
alg :: Num a => Algebra Ring a
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

```Haskell
data 
```
