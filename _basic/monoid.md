---
title: Monoid
---

## Overview

**Monoid** is a fundamental algebraic structure representing values that can be combined associatively with an identity element. It's one of the most practical and widely-used abstractions in Haskell, enabling elegant solutions for aggregation, accumulation, and parallel computation.

## Core Concepts

- **Binary Operation**: Combining two values into one
- **Associativity**: Grouping doesn't matter: `(a <> b) <> c = a <> (b <> c)`
- **Identity Element**: A neutral value: `mempty <> a = a = a <> mempty`
- **Composability**: Build complex operations from simple ones

## Type Signature

```haskell
class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mappend = (<>)  -- Usually just use (<>) from Semigroup
    
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

-- Semigroup (required superclass)
class Semigroup a where
    (<>) :: a -> a -> a
    sconcat :: NonEmpty a -> a
    stimes :: Integral b => b -> a -> a
```

### Monoid Laws

```haskell
-- Left Identity
mempty <> x = x

-- Right Identity
x <> mempty = x

-- Associativity (from Semigroup)
(x <> y) <> z = x <> (y <> z)
```

## Examples

### Basic Monoid Instances

```haskell
-- List Monoid (concatenation)
instance Monoid [a] where
    mempty = []
    mappend = (++)

example1 = [1,2] <> [3,4] <> [5,6]
-- Result: [1,2,3,4,5,6]

example2 = mconcat [[1,2], [3,4], [5,6]]
-- Result: [1,2,3,4,5,6]

-- String Monoid (specialized list)
greeting = "Hello" <> " " <> "World" <> mempty
-- Result: "Hello World"
```

### Numeric Monoids

```haskell
import Data.Monoid

-- Sum: addition with 0 as identity
newtype Sum a = Sum { getSum :: a }

instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

sumExample = getSum $ Sum 1 <> Sum 2 <> Sum 3
-- Result: 6

sumList = getSum $ mconcat [Sum 1, Sum 2, Sum 3, Sum 4]
-- Result: 10

-- Product: multiplication with 1 as identity
newtype Product a = Product { getProduct :: a }

instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

instance Num a => Monoid (Product a) where
    mempty = Product 1

productExample = getProduct $ Product 2 <> Product 3 <> Product 4
-- Result: 24
```

### Boolean Monoids

```haskell
import Data.Monoid

-- All: AND operation with True as identity
newtype All = All { getAll :: Bool }

instance Semigroup All where
    All x <> All y = All (x && y)

instance Monoid All where
    mempty = All True

allExample = getAll $ All True <> All False <> All True
-- Result: False

allValid = getAll $ mconcat [All True, All True, All True]
-- Result: True

-- Any: OR operation with False as identity
newtype Any = Any { getAny :: Bool }

instance Semigroup Any where
    Any x <> Any y = Any (x || y)

instance Monoid Any where
    mempty = Any False

anyExample = getAny $ Any False <> Any True <> Any False
-- Result: True
```

### Maybe Monoid

```haskell
-- First: first non-Nothing value
newtype First a = First { getFirst :: Maybe a }

instance Semigroup (First a) where
    First Nothing <> y = y
    x <> _ = x

instance Monoid (First a) where
    mempty = First Nothing

firstExample = getFirst $ First Nothing <> First (Just 5) <> First (Just 10)
-- Result: Just 5

-- Last: last non-Nothing value
newtype Last a = Last { getLast :: Maybe a }

instance Semigroup (Last a) where
    x <> Last Nothing = x
    _ <> y = y

instance Monoid (Last a) where
    mempty = Last Nothing

lastExample = getLast $ Last (Just 5) <> Last Nothing <> Last (Just 10)
-- Result: Just 10
```

### Endo (Function Composition)

```haskell
import Data.Monoid

-- Endo: function composition monoid
newtype Endo a = Endo { appEndo :: a -> a }

instance Semigroup (Endo a) where
    Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
    mempty = Endo id

-- Compose multiple transformations
transform :: Endo Int
transform = Endo (*2) <> Endo (+10) <> Endo (subtract 3)

result = appEndo transform 5
-- Result: 5 - 3 + 10 * 2 = (5 - 3 = 2) + 10 = 12 * 2 = 24
-- Actually: ((5 - 3) + 10) * 2 = 24
```

### Map Monoid

```haskell
import qualified Data.Map as Map

-- Map with Semigroup values
instance (Ord k, Semigroup v) => Semigroup (Map k v) where
    (<>) = Map.unionWith (<>)

instance (Ord k, Semigroup v) => Monoid (Map k v) where
    mempty = Map.empty

-- Combining maps
map1 = Map.fromList [("a", Sum 1), ("b", Sum 2)]
map2 = Map.fromList [("b", Sum 3), ("c", Sum 4)]
combined = map1 <> map2
-- Result: Map.fromList [("a", Sum 1), ("b", Sum 5), ("c", Sum 4)]
```

## Custom Monoid

```haskell
-- Shopping cart monoid
data ShoppingCart = ShoppingCart
    { totalItems :: Sum Int
    , totalPrice :: Sum Double
    , appliedCoupons :: [String]
    } deriving (Show)

instance Semigroup ShoppingCart where
    ShoppingCart i1 p1 c1 <> ShoppingCart i2 p2 c2 =
        ShoppingCart (i1 <> i2) (p1 <> p2) (c1 <> c2)

instance Monoid ShoppingCart where
    mempty = ShoppingCart mempty mempty mempty

-- Usage
cart1 = ShoppingCart (Sum 2) (Sum 50.0) ["SAVE10"]
cart2 = ShoppingCart (Sum 3) (Sum 75.0) ["FREESHIP"]
cart3 = ShoppingCart (Sum 1) (Sum 25.0) []

totalCart = cart1 <> cart2 <> cart3
-- ShoppingCart (Sum 6) (Sum 150.0) ["SAVE10", "FREESHIP"]
```

## Common Use Cases

### 1. Aggregating Data

```haskell
import Data.Foldable (foldMap)

-- Calculate statistics
data Stats = Stats
    { count :: Sum Int
    , total :: Sum Double
    , minimum :: Min Double
    , maximum :: Max Double
    } deriving (Show)

instance Semigroup Stats where
    Stats c1 t1 min1 max1 <> Stats c2 t2 min2 max2 =
        Stats (c1 <> c2) (t1 <> t2) (min1 <> min2) (max1 <> max2)

instance Monoid Stats where
    mempty = Stats mempty mempty (Min (1/0)) (Max (-1/0))

toStats :: Double -> Stats
toStats x = Stats (Sum 1) (Sum x) (Min x) (Max x)

-- Aggregate a list
values = [3.5, 7.2, 1.8, 9.3, 4.6]
stats = foldMap toStats values
-- Stats (Sum 5) (Sum 26.4) (Min 1.8) (Max 9.3)
```

### 2. Configuration Merging

```haskell
data Config = Config
    { port :: Last Int
    , host :: Last String
    , debug :: Any
    , logLevel :: First LogLevel
    } deriving (Show)

instance Semigroup Config where
    Config p1 h1 d1 l1 <> Config p2 h2 d2 l2 =
        Config (p1 <> p2) (h1 <> h2) (d1 <> d2) (l1 <> l2)

instance Monoid Config where
    mempty = Config mempty mempty mempty mempty

-- Default config
defaultConfig = Config 
    (Last (Just 8080))
    (Last (Just "localhost"))
    (Any False)
    (First (Just Info))

-- User config
userConfig = Config
    (Last (Just 3000))
    mempty
    (Any True)
    mempty

-- Merge: user overrides defaults
finalConfig = defaultConfig <> userConfig
```

### 3. Validation

```haskell
import Data.Validation

-- Accumulate all validation errors
data ValidationError = ValidationError [String]
    deriving (Show)

instance Semigroup ValidationError where
    ValidationError e1 <> ValidationError e2 = ValidationError (e1 <> e2)

instance Monoid ValidationError where
    mempty = ValidationError []

validateName :: String -> Either ValidationError String
validateName n
    | null n = Left (ValidationError ["Name is required"])
    | length n < 2 = Left (ValidationError ["Name too short"])
    | otherwise = Right n

validateAge :: Int -> Either ValidationError Int
validateAge a
    | a < 0 = Left (ValidationError ["Age cannot be negative"])
    | a > 150 = Left (ValidationError ["Age too large"])
    | otherwise = Right a

validateEmail :: String -> Either ValidationError String
validateEmail e
    | '@' `notElem` e = Left (ValidationError ["Invalid email"])
    | otherwise = Right e
```

### 4. Query Building

```haskell
-- SQL query builder
newtype Query = Query { getQuery :: String }

instance Semigroup Query where
    Query a <> Query b = Query (a ++ " " ++ b)

instance Monoid Query where
    mempty = Query ""

select :: [String] -> Query
select cols = Query $ "SELECT " ++ intercalate ", " cols

from :: String -> Query
from table = Query $ "FROM " ++ table

where_ :: String -> Query
where_ cond = Query $ "WHERE " ++ cond

-- Build query
userQuery = select ["name", "email"] <> from "users" <> where_ "age > 18"
-- Query "SELECT name, email FROM users WHERE age > 18"
```

### 5. Logging and Tracing

```haskell
import Data.Monoid (Dual(..))

-- Log entries accumulate
newtype Log = Log { getLogs :: [String] }
    deriving (Show)

instance Semigroup Log where
    Log l1 <> Log l2 = Log (l1 <> l2)

instance Monoid Log where
    mempty = Log []

logInfo :: String -> Log
logInfo msg = Log ["INFO: " ++ msg]

logError :: String -> Log
logError msg = Log ["ERROR: " ++ msg]

-- Writer monad pattern
processWithLogging :: (Int, Log)
processWithLogging =
    let (result1, log1) = (10, logInfo "Started processing")
        (result2, log2) = (result1 * 2, logInfo "Doubled value")
        (result3, log3) = (result2 + 5, logInfo "Added 5")
    in (result3, log1 <> log2 <> log3)
```

### 6. Parallel Computation

```haskell
import Control.Parallel.Strategies

-- Monoids enable easy parallelization
parallelSum :: [Int] -> Int
parallelSum = getSum . mconcat . parMap rpar Sum

-- Split work, compute in parallel, combine with monoid
parallelAggregate :: Monoid m => (a -> m) -> [a] -> m
parallelAggregate f xs = 
    let chunks = chunksOf 100 xs
        results = parMap rpar (foldMap f) chunks
    in mconcat results
```

## Derived Functions

```haskell
-- Standard library utilities using Monoid

-- Fold with Monoid
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr (<>) mempty

-- Map then fold
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (mappend . f) mempty

-- Filter and fold
filterFold :: (Foldable t, Monoid m) => (a -> Bool) -> (a -> m) -> t a -> m
filterFold p f = foldMap (\x -> if p x then f x else mempty)

-- Conditional monoid
when :: Monoid a => Bool -> a -> a
when True x = x
when False _ = mempty
```

## Monoid Transformers

```haskell
-- Lift monoid through functors
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing <> b = b
    a <> Nothing = a
    Just a <> Just b = Just (a <> b)

-- Lift monoid through pairs
instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    (a1, b1) <> (a2, b2) = (a1 <> a2, b1 <> b2)

-- Lift monoid through functions
instance Monoid b => Monoid (a -> b) where
    mempty = const mempty
    f <> g = \x -> f x <> g x
```

## Related Patterns

- **[Semigroup](https://wiki.haskell.org/Typeclassopedia#Semigroup)** - Monoid without identity
- **[Foldable](https://wiki.haskell.org/Typeclassopedia#Foldable)** - Uses Monoid for folding
- **[Functor](functor)** - Different abstraction for mapping
- **[Applicative](applicative)** - Can be built from Monoid

## Best Practices

1. **Use newtype wrappers**: For types with multiple valid monoids (Sum, Product)
2. **Prefer `<>` over `mappend`**: More concise and standard
3. **Use `foldMap`**: More efficient than `fold . fmap`
4. **Verify laws**: Ensure associativity and identity hold
5. **Choose semantics carefully**: Make the combining operation intuitive
6. **Document the monoid**: Explain what the operation means
7. **Use `mconcat`**: For combining multiple values
8. **Consider commutativity**: Note if your monoid is commutative

## Common Monoid Patterns

### Dual Monoid

```haskell
-- Reverse the operation
newtype Dual a = Dual { getDual :: a }

instance Semigroup a => Semigroup (Dual a) where
    Dual x <> Dual y = Dual (y <> x)  -- Note: reversed

instance Monoid a => Monoid (Dual a) where
    mempty = Dual mempty

-- Useful for reversing folds
rightToLeft = getDual $ foldMap Dual [1,2,3,4]
```

### Commutative Monoids

```haskell
-- Some monoids are commutative: a <> b = b <> a
-- Examples: Sum, Product, Max, Min, Any, All
-- But not: List, Endo, First, Last

-- Commutative monoids are easier to parallelize
-- and can be optimized differently
```

## Performance Considerations

```haskell
-- List concatenation is O(n) - avoid for large lists
-- Better: use difference lists or builders

-- DList: Efficient list concatenation
newtype DList a = DList { unDL :: [a] -> [a] }

instance Semigroup (DList a) where
    DList f <> DList g = DList (f . g)

instance Monoid (DList a) where
    mempty = DList id

toList :: DList a -> [a]
toList (DList f) = f []

-- O(1) concatenation
efficientBuild = toList $ foldMap (DList . (:)) [1..10000]
```

## Further Reading

- [Monoid - Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia#Monoid)
- [Data.Monoid Documentation](https://hackage.haskell.org/package/base/docs/Data-Monoid.html)
- [Monoids: Theme and Variations](https://www.youtube.com/watch?v=X-8NCkD2vOw) - Brent Yorgey
- [Monoids in Haskell](https://blog.jle.im/entry/monoids-in-haskell.html)
