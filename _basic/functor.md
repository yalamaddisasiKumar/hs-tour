---
title: Functor
---

## Overview

A **Functor** is one of the most fundamental abstractions in Haskell. It represents a computational context that can be mapped over, allowing you to apply a function to values inside a context without changing the structure of that context.

## Core Concepts

- **Mapping**: Apply a function to wrapped values
- **Structure Preservation**: The shape of the container remains unchanged
- **Composition**: Functor laws ensure predictable behavior

## Type Signature

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    
    -- Default implementation
    (<$) :: a -> f b -> f a
    (<$) = fmap . const
```

### Functor Laws

```haskell
-- Identity
fmap id = id

-- Composition
fmap (f . g) = fmap f . fmap g
```

## Examples

### Basic List Functor

```haskell
-- Mapping over a list
result1 :: [Int]
result1 = fmap (+1) [1, 2, 3, 4]
-- Result: [2, 3, 4, 5]

-- Using the infix operator
result2 :: [Int]
result2 = (*2) <$> [1, 2, 3, 4]
-- Result: [2, 4, 6, 8]
```

### Maybe Functor

```haskell
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Mapping over Maybe
result3 :: Maybe Double
result3 = fmap (*100) (safeDivide 10 2)
-- Result: Just 500.0

result4 :: Maybe Double
result4 = fmap (*100) (safeDivide 10 0)
-- Result: Nothing
```

### Custom Functor

```haskell
data Box a = Box a deriving (Show, Eq)

instance Functor Box where
    fmap f (Box x) = Box (f x)

-- Usage
unboxedValue :: Box Int
unboxedValue = fmap (+10) (Box 5)
-- Result: Box 15
```

### IO Functor

```haskell
-- Reading and transforming input
main :: IO ()
main = do
    line <- fmap (map toUpper) getLine
    putStrLn $ "You said: " ++ line
```

## Common Use Cases

### 1. Data Transformation

```haskell
-- Transform data in various contexts
prices :: [Double]
prices = [10.0, 20.0, 30.0]

pricesWithTax :: [Double]
pricesWithTax = fmap (*1.2) prices
```

### 2. Optional Value Handling

```haskell
data User = User { userId :: Int, userName :: String }

findUser :: Int -> Maybe User
findUser = undefined

getUserName :: Int -> Maybe String
getUserName id = fmap userName (findUser id)
```

### 3. Function Composition

```haskell
-- Functors allow lifting functions into contexts
processData :: [Int] -> [String]
processData = fmap show . fmap (*2) . fmap (+1)
-- Equivalent to: fmap (show . (*2) . (+1))
```

## Related Patterns

- **[Applicative](applicative)** - Extends Functor with application in context
- **[Monad](monad)** - Further extends with sequencing operations
- **[Comonad](comonad)** - The categorical dual of Functor

## Best Practices

1. **Always verify functor laws** when implementing custom instances
2. **Use `<$>` for cleaner syntax** in applicative-style code
3. **Compose functions before mapping** for efficiency: `fmap (f . g . h)` instead of `fmap f . fmap g . fmap h`
4. **Leverage functor for data pipelines** to maintain context

## Further Reading

- [Typeclassopedia - Functor](http://www.haskell.org/haskellwiki/Typeclassopedia#Functor)
- [Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
