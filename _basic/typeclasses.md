---
title: Typeclasses
---

## Overview

**Typeclasses** are Haskell's mechanism for ad-hoc polymorphism, allowing you to define generic interfaces that can be implemented by multiple types. They enable overloading of functions and provide a powerful way to structure code through abstraction.

## Core Concepts

- **Ad-hoc Polymorphism**: Different implementations for different types
- **Interface Definition**: Specify required operations
- **Instance Declaration**: Provide type-specific implementations
- **Constraint System**: Express type requirements

## Type Signature

```haskell
-- Defining a typeclass
class TypeClassName a where
    method1 :: a -> ReturnType
    method2 :: a -> a -> a
    
    -- Default implementations
    method3 :: a -> Bool
    method3 _ = True

-- Using constraints
function :: TypeClassName a => a -> a
```

## Examples

### Basic Typeclass Definition

```haskell
-- Defining Eq
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    
    -- Default implementations
    x /= y = not (x == y)
    x == y = not (x /= y)

-- Instance for custom type
data Color = Red | Green | Blue

instance Eq Color where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    _ == _ = False
```

### Ord Typeclass

```haskell
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    max :: a -> a -> a
    min :: a -> a -> a

-- Instance implementation
data Priority = Low | Medium | High

instance Eq Priority where
    Low == Low = True
    Medium == Medium = True
    High == High = True
    _ == _ = False

instance Ord Priority where
    compare Low Low = EQ
    compare Low _ = LT
    compare Medium Low = GT
    compare Medium Medium = EQ
    compare Medium High = LT
    compare High High = EQ
    compare High _ = GT
```

### Show and Read

```haskell
class Show a where
    show :: a -> String
    
class Read a where
    read :: String -> a

-- Custom instance
data Point = Point Double Double

instance Show Point where
    show (Point x y) = "Point(" ++ show x ++ ", " ++ show y ++ ")"

instance Read Point where
    read s = -- parsing logic
```

### Num Typeclass

```haskell
class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a

-- Complex numbers
data Complex = Complex Double Double

instance Num Complex where
    (Complex a b) + (Complex c d) = Complex (a + c) (b + d)
    (Complex a b) * (Complex c d) = Complex (a*c - b*d) (a*d + b*c)
    negate (Complex a b) = Complex (-a) (-b)
    abs (Complex a b) = Complex (sqrt (a*a + b*b)) 0
    signum z@(Complex a b) = Complex (a / r) (b / r)
        where r = abs' z
    fromInteger n = Complex (fromInteger n) 0
```

## Multi-Parameter Typeclasses

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

class Convertible a b where
    convert :: a -> b

-- Instances
instance Convertible Int Double where
    convert = fromIntegral

instance Convertible String Int where
    convert = read

-- Usage
result1 :: Double
result1 = convert (42 :: Int)  -- 42.0

result2 :: Int
result2 = convert "123"  -- 123
```

## Functional Dependencies

```haskell
{-# LANGUAGE FunctionalDependencies #-}

class Collection c e | c -> e where
    empty :: c
    insert :: e -> c -> c
    member :: e -> c -> Bool

-- The functional dependency | c -> e means that
-- the element type e is determined by the collection type c

instance Eq a => Collection [a] a where
    empty = []
    insert = (:)
    member = elem
```

## Associated Types

```haskell
{-# LANGUAGE TypeFamilies #-}

class Collection c where
    type Elem c :: *
    empty :: c
    insert :: Elem c -> c -> c
    member :: Elem c -> c -> Bool

-- Instance for lists
instance Eq a => Collection [a] where
    type Elem [a] = a
    empty = []
    insert = (:)
    member = elem

-- Instance for sets
instance Ord a => Collection (Set a) where
    type Elem (Set a) = a
    empty = Set.empty
    insert = Set.insert
    member = Set.member
```

## Common Standard Typeclasses

### Monoid

```haskell
class Semigroup a where
    (<>) :: a -> a -> a

class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mappend = (<>)
    
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

-- Examples
instance Monoid [a] where
    mempty = []
    mappend = (++)

instance Monoid (Sum Int) where
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum (x + y)
```

### Foldable

```haskell
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldMap :: Monoid m => (a -> m) -> t a -> m
    
    -- Many derived functions
    length :: t a -> Int
    null :: t a -> Bool
    elem :: Eq a => a -> t a -> Bool

-- Custom instance
data Tree a = Empty | Node a (Tree a) (Tree a)

instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Node x l r) = foldr f (f x (foldr f z r)) l
```

### Traversable

```haskell
class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA :: Applicative f => t (f a) -> f (t a)

-- Example instance
instance Traversable Tree where
    traverse f Empty = pure Empty
    traverse f (Node x l r) = 
        Node <$> f x <*> traverse f l <*> traverse f r
```

## Deriving Instances

```haskell
-- Automatic deriving
data Person = Person
    { name :: String
    , age :: Int
    } deriving (Show, Read, Eq, Ord)

-- Standalone deriving
{-# LANGUAGE StandaloneDeriving #-}
deriving instance Show SomeType

-- DerivingVia
{-# LANGUAGE DerivingVia #-}
newtype Age = Age Int
    deriving (Show, Eq, Ord, Num) via Int
```

## Advanced Patterns

### Newtype Deriving

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype UserId = UserId Int
    deriving (Show, Eq, Ord, Num)

-- Automatically derives all instances from Int
userId :: UserId
userId = UserId 42 + UserId 1  -- Works because Num is derived
```

### Default Signatures

```haskell
{-# LANGUAGE DefaultSignatures #-}

class Serialize a where
    serialize :: a -> ByteString
    
    default serialize :: (Generic a, GSerialize (Rep a)) => a -> ByteString
    serialize = gserialize . from
```

### Type Constraints in Instances

```haskell
-- Constraint on instance
instance Eq a => Eq [a] where
    [] == [] = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _ == _ = False

-- Multiple constraints
instance (Eq a, Eq b) => Eq (a, b) where
    (a1, b1) == (a2, b2) = a1 == a2 && b1 == b2
```

## Common Use Cases

### 1. Generic Programming

```haskell
class Describable a where
    describe :: a -> String

instance Describable Int where
    describe n = "Integer: " ++ show n

instance Describable String where
    describe s = "String: " ++ s

printDescription :: Describable a => a -> IO ()
printDescription = putStrLn . describe
```

### 2. Abstract Data Structures

```haskell
class Stack s where
    type Elem s :: *
    empty :: s
    push :: Elem s -> s -> s
    pop :: s -> Maybe (Elem s, s)
    peek :: s -> Maybe (Elem s)

instance Stack [a] where
    type Elem [a] = a
    empty = []
    push = (:)
    pop [] = Nothing
    pop (x:xs) = Just (x, xs)
    peek [] = Nothing
    peek (x:_) = Just x
```

### 3. Serialization

```haskell
class ToJSON a where
    toJSON :: a -> Value

class FromJSON a where
    fromJSON :: Value -> Result a

instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age" .= age
        ]

instance FromJSON Person where
    fromJSON = withObject "Person" $ \o ->
        Person <$> o .: "name" <*> o .: "age"
```

### 4. Type-Level Computation

```haskell
class KnownNat n where
    natVal :: proxy n -> Integer

-- Usage
vectorLength :: KnownNat n => Vector n a -> Int
vectorLength v = fromInteger (natVal v)
```

## Orphan Instances

```haskell
-- Avoid orphan instances (defined outside module of type or class)
-- BAD: defining in a third module
-- module MyModule where
-- instance Show TheirType where ...

-- GOOD: use newtype wrapper
newtype MyType = MyType TheirType

instance Show MyType where
    show (MyType x) = -- custom implementation
```

## Related Patterns

- **[Functor](functor)** - Specific typeclass pattern
- **[Applicative](applicative)** - Another typeclass pattern
- **[Monad](monad)** - Advanced typeclass pattern
- **[Type-Level Programming](../advanced/type-level-programming)** - Advanced typeclass usage

## Best Practices

1. **Keep typeclasses focused**: Each typeclass should represent one concept
2. **Provide laws**: Document expected behavior and invariants
3. **Use minimal complete definition**: Mark which methods must be implemented
4. **Avoid orphan instances**: Define instances in type or class module
5. **Consider coherence**: Each type should have at most one instance per class
6. **Use type families** over functional dependencies for clarity
7. **Document instances**: Explain non-obvious implementations

## Common Pitfalls

1. **Overly general constraints**: Don't add unnecessary typeclass constraints
2. **Overlapping instances**: Can lead to ambiguity
3. **Incoherent instances**: Different instances visible in different modules
4. **Too many typeclasses**: Can lead to complexity

## Further Reading

- [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia)
- [Type Classes in Haskell](https://www.schoolofhaskell.com/user/commercial/content/type-classes)
- [Advanced Overlap](https://wiki.haskell.org/GHC/AdvancedOverlap)
