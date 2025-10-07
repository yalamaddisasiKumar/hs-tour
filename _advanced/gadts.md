---
title: GADTs (Generalized Algebraic Data Types)
---

## Overview

**GADTs** (Generalized Algebraic Data Types) extend Haskell's type system by allowing you to specify exact return types for data constructors. They enable type-safe embedded DSLs, heterogeneous collections, and advanced type-level programming patterns.

## Core Concepts

- **Type Refinement**: Constructors specify exact return types
- **Type Safety**: Prevent invalid states at compile time
- **Pattern Matching**: Recover type information through pattern matching
- **Phantom Types on Steroids**: More powerful than phantom types

## Syntax

```haskell
{-# LANGUAGE GADTs #-}

-- Standard ADT
data Expr = I Int | B Bool | Add Expr Expr

-- GADT syntax
data Expr a where
    I    :: Int -> Expr Int
    B    :: Bool -> Expr Bool
    Add  :: Expr Int -> Expr Int -> Expr Int
    Eq   :: Eq a => Expr a -> Expr a -> Expr Bool
    If   :: Expr Bool -> Expr a -> Expr a -> Expr a
```

## Examples

### Type-Safe Expression Evaluator

```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
    LitInt  :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add     :: Expr Int -> Expr Int -> Expr Int
    Mul     :: Expr Int -> Expr Int -> Expr Int
    And     :: Expr Bool -> Expr Bool -> Expr Bool
    Or      :: Expr Bool -> Expr Bool -> Expr Bool
    Eq      :: Eq a => Expr a -> Expr a -> Expr Bool
    Lt      :: Ord a => Expr a -> Expr a -> Expr Bool
    If      :: Expr Bool -> Expr a -> Expr a -> Expr a

-- Type-safe evaluation
eval :: Expr a -> a
eval (LitInt n) = n
eval (LitBool b) = b
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (And x y) = eval x && eval y
eval (Or x y) = eval x || eval y
eval (Eq x y) = eval x == eval y
eval (Lt x y) = eval x < eval y
eval (If cond t e) = if eval cond then eval t else eval e

-- Examples
expr1 :: Expr Int
expr1 = Add (LitInt 3) (Mul (LitInt 4) (LitInt 5))

expr2 :: Expr Bool
expr2 = Eq (Add (LitInt 2) (LitInt 3)) (LitInt 5)

expr3 :: Expr Int
expr3 = If (LitBool True) (LitInt 10) (LitInt 20)

result1 = eval expr1  -- 23
result2 = eval expr2  -- True
result3 = eval expr3  -- 10

-- This won't compile (type error caught at compile time):
-- bad = Add (LitInt 5) (LitBool True)  -- Type error!
```

### Safe Database Queries

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- Database column types
data ColumnType = IntCol | StringCol | BoolCol

-- Typed column reference
data Column (t :: ColumnType) where
    ColInt :: String -> Column 'IntCol
    ColString :: String -> Column 'StringCol
    ColBool :: String -> Column 'BoolCol

-- Query expressions
data QueryExpr (t :: ColumnType) where
    Lit :: SqlType t -> QueryExpr t
    Col :: Column t -> QueryExpr t
    AddQ :: QueryExpr 'IntCol -> QueryExpr 'IntCol -> QueryExpr 'IntCol
    ConcatQ :: QueryExpr 'StringCol -> QueryExpr 'StringCol -> QueryExpr 'StringCol
    EqQ :: QueryExpr t -> QueryExpr t -> QueryExpr 'BoolCol
    AndQ :: QueryExpr 'BoolCol -> QueryExpr 'BoolCol -> QueryExpr 'BoolCol

-- Type families for conversions
type family SqlType (t :: ColumnType) where
    SqlType 'IntCol = Int
    SqlType 'StringCol = String
    SqlType 'BoolCol = Bool

-- Generate SQL
toSQL :: QueryExpr t -> String
toSQL (Lit n) = show n
toSQL (Col (ColInt name)) = name
toSQL (Col (ColString name)) = name
toSQL (Col (ColBool name)) = name
toSQL (AddQ x y) = "(" ++ toSQL x ++ " + " ++ toSQL y ++ ")"
toSQL (ConcatQ x y) = "(" ++ toSQL x ++ " || " ++ toSQL y ++ ")"
toSQL (EqQ x y) = "(" ++ toSQL x ++ " = " ++ toSQL y ++ ")"
toSQL (AndQ x y) = "(" ++ toSQL x ++ " AND " ++ toSQL y ++ ")"

-- Example query
userQuery :: QueryExpr 'BoolCol
userQuery = AndQ
    (EqQ (Col (ColString "name")) (Lit "Alice"))
    (EqQ (Col (ColInt "age")) (Lit 30))

sql = toSQL userQuery
-- "((name = Alice) AND (age = 30))"
```

### Heterogeneous Lists

```haskell
{-# LANGUAGE GADTs #-}

-- Heterogeneous list with type-level list of types
data HList (ts :: [*]) where
    HNil :: HList '[]
    HCons :: t -> HList ts -> HList (t ': ts)

-- Examples
example1 :: HList '[Int, String, Bool]
example1 = HCons 42 (HCons "hello" (HCons True HNil))

example2 :: HList '[String, Double]
example2 = HCons "pi" (HCons 3.14 HNil)

-- Head function (type-safe)
hHead :: HList (t ': ts) -> t
hHead (HCons x _) = x

-- Tail function
hTail :: HList (t ': ts) -> HList ts
hTail (HCons _ xs) = xs

-- Usage
val1 = hHead example1  -- 42 :: Int
val2 = hHead (hTail example1)  -- "hello" :: String
```

### Type-Safe State Machine

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- State types
data DoorState = Opened | Closed | Locked

-- State machine operations
data Door (s :: DoorState) where
    OpenDoor :: Door 'Closed -> Door 'Opened
    CloseDoor :: Door 'Opened -> Door 'Closed
    LockDoor :: Door 'Closed -> Door 'Locked
    UnlockDoor :: Key -> Door 'Locked -> Door 'Closed

data Key = Key

-- Initial state
initialDoor :: Door 'Closed
initialDoor = undefined  -- Would be constructed properly

-- Valid state transitions
validSequence :: Key -> Door 'Locked
validSequence key = 
    let opened = OpenDoor initialDoor
        closed = CloseDoor opened
        locked = LockDoor closed
    in locked

-- Invalid transitions won't compile:
-- invalid = LockDoor (OpenDoor initialDoor)  -- Type error!
-- Can't lock an opened door
```

### Abstract Syntax Tree with Type Information

```haskell
{-# LANGUAGE GADTs #-}

data Type a where
    TInt :: Type Int
    TBool :: Type Bool
    TString :: Type String
    TList :: Type a -> Type [a]
    TFunc :: Type a -> Type b -> Type (a -> b)

-- Typed AST
data Typed a where
    Lit :: Type a -> a -> Typed a
    Var :: String -> Type a -> Typed a
    App :: Typed (a -> b) -> Typed a -> Typed b
    Lam :: String -> Type a -> Typed b -> Typed (a -> b)

-- Type checker that returns proof of type
typeOf :: Typed a -> Type a
typeOf (Lit t _) = t
typeOf (Var _ t) = t
typeOf (App f _) = 
    case typeOf f of
        TFunc _ b -> b
typeOf (Lam _ a b) = TFunc a (typeOf b)

-- Evaluation with type safety
eval :: Typed a -> Env -> a
eval (Lit _ v) _ = v
eval (Var name t) env = lookupVar name env
eval (App f x) env = (eval f env) (eval x env)
eval (Lam var _ body) env = \v -> eval body (extendEnv var v env)
```

### Safe Indexing

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

data Nat = Zero | Succ Nat

-- Length-indexed vector
data Vec (n :: Nat) a where
    VNil :: Vec 'Zero a
    VCons :: a -> Vec n a -> Vec ('Succ n) a

-- Safe head (can't be called on empty vector)
vhead :: Vec ('Succ n) a -> a
vhead (VCons x _) = x

-- Safe tail
vtail :: Vec ('Succ n) a -> Vec n a
vtail (VCons _ xs) = xs

-- Safe indexing
data Fin (n :: Nat) where
    FZero :: Fin ('Succ n)
    FSucc :: Fin n -> Fin ('Succ n)

index :: Vec n a -> Fin n -> a
index (VCons x _) FZero = x
index (VCons _ xs) (FSucc i) = index xs i

-- Example
vec :: Vec ('Succ ('Succ ('Succ 'Zero))) Int
vec = VCons 1 (VCons 2 (VCons 3 VNil))

firstElem = index vec FZero  -- 1
secondElem = index vec (FSucc FZero)  -- 2
```

### Protocol Implementation

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- Protocol states
data ProtocolState = Initial | Connected | Authenticated | Closed

-- Protocol messages
data Message (from :: ProtocolState) (to :: ProtocolState) where
    Connect :: Message 'Initial 'Connected
    Authenticate :: Credentials -> Message 'Connected 'Authenticated
    SendData :: Data -> Message 'Authenticated 'Authenticated
    Disconnect :: Message s 'Closed

-- Protocol session
data Session (s :: ProtocolState) where
    MkSession :: Session s

-- State transitions
transition :: Session s -> Message s t -> Session t
transition _ _ = MkSession

-- Type-safe protocol implementation
secureSession :: Credentials -> Data -> Session 'Closed
secureSession creds payload =
    let initial = MkSession :: Session 'Initial
        connected = transition initial Connect
        authenticated = transition connected (Authenticate creds)
        sent = transition authenticated (SendData payload)
        closed = transition sent Disconnect
    in closed
```

## Pattern Matching and Type Refinement

```haskell
-- Pattern matching refines types
checkType :: Expr a -> String
checkType expr = case expr of
    LitInt _ -> "Integer expression"  -- Here we know a ~ Int
    LitBool _ -> "Boolean expression"  -- Here we know a ~ Bool
    Add _ _ -> "Integer addition"      -- Here we know a ~ Int
    And _ _ -> "Boolean AND"           -- Here we know a ~ Bool
    If _ t _ -> "Conditional: " ++ checkType t  -- Polymorphic

-- Equality test
eqExpr :: Expr a -> Expr a -> Bool
eqExpr (LitInt x) (LitInt y) = x == y
eqExpr (LitBool x) (LitBool y) = x == y
eqExpr (Add x1 y1) (Add x2 y2) = eqExpr x1 x2 && eqExpr y1 y2
eqExpr _ _ = False
```

## Combining with Type Families

```haskell
{-# LANGUAGE TypeFamilies #-}

type family Interp (t :: Type) :: * where
    Interp 'TInt = Int
    Interp 'TBool = Bool
    Interp 'TString = String
    Interp ('TList t) = [Interp t]
    Interp ('TFunc a b) = Interp a -> Interp b

data Value (t :: Type) where
    VInt :: Int -> Value 'TInt
    VBool :: Bool -> Value 'TBool
    VString :: String -> Value 'TString
    VList :: [Value t] -> Value ('TList t)
    VFunc :: (Value a -> Value b) -> Value ('TFunc a b)

unValue :: Value t -> Interp t
unValue (VInt n) = n
unValue (VBool b) = b
unValue (VString s) = s
unValue (VList xs) = map unValue xs
unValue (VFunc f) = unValue . f . mkValue
```

## Existential Types with GADTs

```haskell
-- Package up a value with its type
data SomeExpr where
    SomeExpr :: Expr a -> SomeExpr

-- We've "forgotten" the type parameter
example :: [SomeExpr]
example = 
    [ SomeExpr (LitInt 42)
    , SomeExpr (LitBool True)
    , SomeExpr (Add (LitInt 1) (LitInt 2))
    ]

-- When pattern matching, we recover the type locally
printExpr :: SomeExpr -> String
printExpr (SomeExpr expr) = show (eval expr)
```

## Related Patterns

- **[Type-Level Programming](type-level-programming)** - GADTs enable type-level computation
- **[Typeclasses](../basic/typeclasses)** - Often used together
- **[Phantom Types](https://wiki.haskell.org/Phantom_type)** - GADTs are more powerful

## Advantages

1. **Type Safety**: Catch more errors at compile time
2. **Expressiveness**: Encode invariants in types
3. **Documentation**: Types document valid operations
4. **Refactoring**: Changes caught by type checker

## Disadvantages

1. **Complexity**: Harder to understand and use
2. **Type Inference**: Often requires type annotations
3. **Error Messages**: Can be difficult to parse
4. **Overhead**: Some runtime representation costs

## Best Practices

1. **Use when appropriate**: Don't overuse GADTs
2. **Provide smart constructors**: Hide GADT complexity
3. **Document constraints**: Explain type relationships
4. **Combine with type families**: For maximum power
5. **Test thoroughly**: Type safety doesn't guarantee correctness

## Common Patterns

### Safe API Design

```haskell
-- Resource management
data Resource (s :: ResourceState) where
    Allocated :: Handle -> Resource 'Allocated
    InUse :: Handle -> Resource 'InUse
    Released :: Resource 'Released

acquire :: IO (Resource 'Allocated)
use :: Resource 'Allocated -> IO (Resource 'InUse)
release :: Resource 'InUse -> IO (Resource 'Released)
```

### Tagless Final with GADTs

```haskell
data Prog a where
    Return :: a -> Prog a
    Bind :: Prog a -> (a -> Prog b) -> Prog b
    Primitive :: PrimOp a -> Prog a

data PrimOp a where
    ReadInt :: PrimOp Int
    PrintInt :: Int -> PrimOp ()
```

## Further Reading

- [Fun with Phantom Types](http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf)
- [GADTs for the Working Haskell Programmer](https://www.cs.yale.edu/homes/hudak/CS429F04/AFPLectureNotes.pdf)
- [GADT - HaskellWiki](https://wiki.haskell.org/GADT)
- [GHC User's Guide - GADTs](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generalised-algebraic-data-types-gadts)
