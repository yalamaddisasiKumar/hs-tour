---
title: Type-Level Programming Patterns
---

## Overview

**Type-Level Programming** uses Haskell's type system to perform computations at compile time, encode invariants, and create highly expressive APIs. It leverages advanced GHC extensions to push computation from runtime to compile time.

## Core Concepts

- **Type Families**: Type-level functions
- **Data Kinds**: Promote types to kinds
- **Type-Level Literals**: Numbers and symbols as types
- **Dependent Types (Limited)**: Types depending on values

## Key Extensions

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
```

## Examples

### Type-Level Natural Numbers

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- Define natural numbers at type level
data Nat = Zero | Succ Nat

-- Type-level addition
type family (n :: Nat) + (m :: Nat) :: Nat where
    'Zero + m = m
    'Succ n + m = 'Succ (n + m)

-- Type-level multiplication
type family (n :: Nat) * (m :: Nat) :: Nat where
    'Zero * m = 'Zero
    'Succ n * m = m + (n * m)

-- Example types
type Two = 'Succ ('Succ 'Zero)
type Three = 'Succ Two
type Five = Two + Three
type Six = Two * Three
```

### Length-Indexed Vectors

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- Vector with length in its type
data Vec (n :: Nat) a where
    VNil :: Vec 'Zero a
    VCons :: a -> Vec n a -> Vec ('Succ n) a

-- Safe head (can't call on empty vector)
vhead :: Vec ('Succ n) a -> a
vhead (VCons x _) = x

-- Safe tail
vtail :: Vec ('Succ n) a -> Vec n a
vtail (VCons _ xs) = xs

-- Append with length tracking
vappend :: Vec n a -> Vec m a -> Vec (n + m) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (vappend xs ys)

-- Map preserves length
vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _ VNil = VNil
vmap f (VCons x xs) = VCons (f x) (vmap f xs)

-- Replicate with type-level count
vreplicate :: SNat n -> a -> Vec n a
vreplicate SZero _ = VNil
vreplicate (SSucc n) x = VCons x (vreplicate n x)

-- Singleton for runtime nat values
data SNat (n :: Nat) where
    SZero :: SNat 'Zero
    SSucc :: SNat n -> SNat ('Succ n)
```

### Type-Level Lists

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- Type-level list operations
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

-- Type-level length
type family Length (xs :: [k]) :: Nat where
    Length '[] = 'Zero
    Length (x ': xs) = 'Succ (Length xs)

-- Type-level membership
type family Elem (x :: k) (xs :: [k]) :: Bool where
    Elem x '[] = 'False
    Elem x (x ': xs) = 'True
    Elem x (y ': xs) = Elem x xs

-- Example usage
type MyList = '[Int, String, Bool]
type ListLength = Length MyList  -- 'Succ ('Succ ('Succ 'Zero))
type HasInt = Elem Int MyList    -- 'True
type HasDouble = Elem Double MyList  -- 'False
```

### Heterogeneous Lists (HList)

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- HList indexed by list of types
data HList (ts :: [*]) where
    HNil :: HList '[]
    HCons :: t -> HList ts -> HList (t ': ts)

-- Examples
example1 :: HList '[Int, String, Bool]
example1 = HCons 42 (HCons "hello" (HCons True HNil))

example2 :: HList '[Double, Char]
example2 = HCons 3.14 (HCons 'a' HNil)

-- Safe indexing with type-level natural
data Fin (n :: Nat) where
    FZero :: Fin ('Succ n)
    FSucc :: Fin n -> Fin ('Succ n)

-- Type family for indexing
type family Index (n :: Nat) (xs :: [k]) :: k where
    Index 'Zero (x ': xs) = x
    Index ('Succ n) (x ': xs) = Index n xs

-- Safe index function
index :: Fin n -> HList ts -> Index n ts
index FZero (HCons x _) = x
index (FSucc n) (HCons _ xs) = index n xs

-- Usage
val1 = index FZero example1  -- 42 :: Int
val2 = index (FSucc FZero) example1  -- "hello" :: String
```

### Type-Level Symbols (Strings)

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

import GHC.TypeLits

-- Records with named fields at type level
data Record (fields :: [(Symbol, *)]) where
    RNil :: Record '[]
    RCons :: KnownSymbol name 
          => Proxy name -> val -> Record fields 
          -> Record ('(name, val) ': fields)

-- Field access
class HasField (name :: Symbol) (fields :: [(Symbol, *)]) val | name fields -> val where
    getField :: Record fields -> val

instance {-# OVERLAPPING #-} HasField name ('(name, val) ': fields) val where
    getField (RCons _ val _) = val

instance {-# OVERLAPPABLE #-} HasField name fields val 
    => HasField name (field ': fields) val where
    getField (RCons _ _ rest) = getField rest

-- Example
person :: Record '[ '("name", String), '("age", Int), '("email", String)]
person = RCons (Proxy :: Proxy "name") "Alice"
       $ RCons (Proxy :: Proxy "age") 30
       $ RCons (Proxy :: Proxy "email") "alice@example.com"
       $ RNil

-- Type-safe field access
name = getField @"name" person    -- "Alice" :: String
age = getField @"age" person      -- 30 :: Int
```

### Type-Level State Machines

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- Door states
data DoorState = Opened | Closed | Locked

-- Valid transitions
type family ValidTransition (from :: DoorState) (to :: DoorState) :: Bool where
    ValidTransition 'Closed 'Opened = 'True
    ValidTransition 'Opened 'Closed = 'True
    ValidTransition 'Closed 'Locked = 'True
    ValidTransition 'Locked 'Closed = 'True
    ValidTransition _ _ = 'False

-- Door with state in type
data Door (s :: DoorState) where
    DoorOpen :: Door 'Opened
    DoorClosed :: Door 'Closed
    DoorLocked :: Door 'Locked

-- Operations constrained by valid transitions
open :: Door 'Closed -> Door 'Opened
open DoorClosed = DoorOpen

close :: Door 'Opened -> Door 'Closed
close DoorOpen = DoorClosed

lock :: Door 'Closed -> Door 'Locked
lock DoorClosed = DoorLocked

unlock :: Door 'Locked -> Door 'Closed
unlock DoorLocked = DoorClosed

-- Valid sequence
sequence1 = lock . close . open . DoorClosed

-- These won't compile:
-- invalid1 = lock DoorOpen  -- Can't lock an open door
-- invalid2 = open DoorLocked  -- Can't open a locked door
```

### Type-Level Proof

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

-- Type-level equality
data a :~: b where
    Refl :: a :~: a

-- Proof that addition is commutative (simplified)
data PlusComm (n :: Nat) (m :: Nat) where
    PlusCommZero :: PlusComm 'Zero m
    PlusCommSucc :: PlusComm n m -> PlusComm ('Succ n) m

-- Use the proof
symmetry :: (n + m) :~: (m + n) -> (m + n) :~: (n + m)
symmetry Refl = Refl
```

### Type-Level Constraints

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

import GHC.Exts (Constraint)

-- Conditional constraints
type family If (cond :: Bool) (t :: k) (f :: k) :: k where
    If 'True t f = t
    If 'False t f = f

-- All elements satisfy constraint
type family All (c :: * -> Constraint) (xs :: [*]) :: Constraint where
    All c '[] = ()
    All c (x ': xs) = (c x, All c xs)

-- Example: ensure all types are showable
showList :: All Show ts => HList ts -> [String]
showList HNil = []
showList (HCons x xs) = show x : showList xs
```

### Singleton Pattern

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Singleton for Nat
data SNat (n :: Nat) where
    SZero :: SNat 'Zero
    SSucc :: SNat n -> SNat ('Succ n)

deriving instance Show (SNat n)

-- Singleton for Bool
data SBool (b :: Bool) where
    SFalse :: SBool 'False
    STrue :: SBool 'True

-- Convert type to value
class SingI (n :: Nat) where
    sing :: SNat n

instance SingI 'Zero where
    sing = SZero

instance SingI n => SingI ('Succ n) where
    sing = SSucc sing

-- Use singleton to get runtime value
natToInt :: SNat n -> Int
natToInt SZero = 0
natToInt (SSucc n) = 1 + natToInt n

-- Implicitly pass singleton
getLength :: SingI n => Vec n a -> Int
getLength _ = natToInt (sing :: SNat n)
```

### Type-Level Programming for APIs

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- REST API type
type API = "users" :> Get '[JSON] [User]
      :<|> "users" :> Capture "id" Int :> Get '[JSON] User
      :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User

-- Type operators for API construction
data (path :: Symbol) :> api
data Capture (name :: Symbol) ty
data ReqBody (content :: [*]) ty
data Get (content :: [*]) ty
data Post (content :: [*]) ty
data (a :: *) :<|> (b :: *)

-- Content types
data JSON
data HTML

-- Generate server from types
serve :: HasServer api => Proxy api -> Server api -> Application
```

### Units and Dimensions

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- Physical dimensions
data Dimension = Length | Mass | Time

-- Quantities with dimensions
newtype Quantity (d :: Dimension) = Quantity Double

-- Type-safe operations
addQuantity :: Quantity d -> Quantity d -> Quantity d
addQuantity (Quantity x) (Quantity y) = Quantity (x + y)

-- Can't add different dimensions:
-- invalid = addQuantity (Quantity 5 :: Quantity 'Length) 
--                       (Quantity 10 :: Quantity 'Time)

-- Complex dimensions
data Dims = Dims Nat Nat Nat  -- length, mass, time exponents

type family MultDims (d1 :: Dims) (d2 :: Dims) :: Dims where
    MultDims ('Dims l1 m1 t1) ('Dims l2 m2 t2) = 
        'Dims (l1 + l2) (m1 + m2) (t1 + t2)

newtype Quantity' (d :: Dims) = Quantity' Double

-- Speed = Length / Time
type Speed = 'Dims ('Succ 'Zero) 'Zero ('Succ 'Zero)

-- Acceleration = Length / Time²
type Acceleration = 'Dims ('Succ 'Zero) 'Zero ('Succ ('Succ 'Zero))
```

## Advanced Patterns

### Defunctionalization

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- Type-level function symbols
data TyFun a b
type a ~> b = TyFun a b -> *

-- Apply type-level function
type family Apply (f :: a ~> b) (x :: a) :: b

-- Example: Map at type level
data MapSym :: (a ~> b) -> [a] ~> [b]

type instance Apply (MapSym f) '[] = '[]
type instance Apply (MapSym f) (x ': xs) = Apply f x ': Apply (MapSym f) xs

-- Identity function
data IdSym :: a ~> a
type instance Apply IdSym x = x

-- Test
type TestMap = Apply (MapSym IdSym) '[Int, Bool, String]
-- Result: '[Int, Bool, String]
```

### Type-Level Sorting

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- Type-level comparison
type family (n :: Nat) <= (m :: Nat) :: Bool where
    'Zero <= m = 'True
    'Succ n <= 'Zero = 'False
    'Succ n <= 'Succ m = n <= m

-- Insert into sorted list
type family Insert (x :: Nat) (xs :: [Nat]) :: [Nat] where
    Insert x '[] = '[x]
    Insert x (y ': ys) = 
        If (x <= y) 
           (x ': y ': ys)
           (y ': Insert x ys)

-- Insertion sort
type family Sort (xs :: [Nat]) :: [Nat] where
    Sort '[] = '[]
    Sort (x ': xs) = Insert x (Sort xs)

-- Test
type Unsorted = '[Three, One, Four, One, Five]
type Sorted = Sort Unsorted
-- Result: '[One, One, Three, Four, Five]
```

## Related Patterns

- **[GADTs](gadts)** - Enable type-level programming
- **[Type Families](https://wiki.haskell.org/GHC/Type_families)** - Core mechanism
- **[DataKinds](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/data_kinds.html)** - Promote data to types

## Advantages

1. **Compile-Time Guarantees**: Catch errors before runtime
2. **Documentation**: Types document invariants
3. **Performance**: No runtime checks needed
4. **Expressiveness**: Encode complex requirements

## Disadvantages

1. **Complexity**: Very advanced features
2. **Compile Time**: Can slow compilation significantly
3. **Error Messages**: Often cryptic and unhelpful
4. **Learning Curve**: Steep for most developers

## Best Practices

1. **Use judiciously**: Don't over-engineer
2. **Document types**: Explain type-level computations
3. **Provide examples**: Show typical usage
4. **Test thoroughly**: Type safety != correctness
5. **Consider alternatives**: Sometimes runtime checks are fine

## Libraries Using Type-Level Programming

- **servant**: Type-safe web APIs
- **singletons**: Comprehensive singleton support
- **type-level**: Type-level programming utilities
- **dimensional**: Type-safe physical dimensions
- **sized**: Statically sized structures

## Further Reading

- [Type-Level Programming in Haskell](https://serokell.io/blog/type-level-programming-in-haskell)
- [Thinking with Types](https://thinkingwithtypes.com/)
- [Singletons Library](https://hackage.haskell.org/package/singletons)
- [GHC User's Guide - Type-Level Features](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/type_families.html)
