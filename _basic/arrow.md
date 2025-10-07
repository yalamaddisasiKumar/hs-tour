---
title: Arrow
---

## Overview

**Arrows** are a generalization of functions that represent computations. They provide an abstraction for modeling side effects, state, and control flow in a composable way. Arrows offer more structure than monads for certain types of computations.

## Core Concepts

- **Computation Abstraction**: Represent computations as first-class values
- **Composition**: Combine computations in various ways
- **Structure**: More restrictive than monads, enabling optimization

## Type Signature

```haskell
class Category a => Arrow a where
    arr :: (b -> c) -> a b c
    first :: a b c -> a (b, d) (c, d)
    second :: a b c -> a (d, b) (d, c)
    (***) :: a b c -> a b' c' -> a (b, b') (c, c')
    (&&&) :: a b c -> a b c' -> a b (c, c')

-- Arrow choice for conditionals
class Arrow a => ArrowChoice a where
    left :: a b c -> a (Either b d) (Either c d)
    right :: a b c -> a (Either d b) (Either d c)
    (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
    (|||) :: a b d -> a c d -> a (Either b c) d
```

### Arrow Laws

```haskell
-- arr preserves identity and composition
arr id = id
arr (f >>> g) = arr f >>> arr g

-- first preserves identity and composition
first (arr f) = arr (first f)
first (f >>> g) = first f >>> first g

-- first and arr interact properly
first f >>> arr fst = arr fst >>> f
first f >>> arr (id *** g) = arr (id *** g) >>> first f
```

## Examples

### Function Arrow

```haskell
-- Functions are arrows
add :: Int -> Int -> Int
add = (+)

addArrow :: (->) Int Int
addArrow = arr (+1)

-- Composition
pipeline :: Int -> String
pipeline = arr (+10) >>> arr (*2) >>> arr show
-- Result: pipeline 5 = "30"
```

### Kleisli Arrow

```haskell
import Control.Arrow
import Control.Monad ((>=>))

-- Kleisli arrows wrap monadic functions
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

-- Example with Maybe
safeDivide :: Kleisli Maybe Double Double
safeDivide = Kleisli $ \x -> 
    if x /= 0 then Just (100 / x) else Nothing

safeRoot :: Kleisli Maybe Double Double
safeRoot = Kleisli $ \x -> 
    if x >= 0 then Just (sqrt x) else Nothing

-- Composing Kleisli arrows
computation :: Kleisli Maybe Double Double
computation = safeDivide >>> safeRoot

result = runKleisli computation 4
-- Result: Just 5.0
```

### State Arrow

```haskell
import Control.Arrow
import Control.Category

newtype StateArrow s a b = StateArrow { runState :: (a, s) -> (b, s) }

instance Category (StateArrow s) where
    id = StateArrow (\(x, s) -> (x, s))
    (StateArrow g) . (StateArrow f) = StateArrow $ \(x, s) ->
        let (y, s') = f (x, s)
            (z, s'') = g (y, s')
        in (z, s'')

instance Arrow (StateArrow s) where
    arr f = StateArrow $ \(x, s) -> (f x, s)
    first (StateArrow f) = StateArrow $ \((x, y), s) ->
        let (x', s') = f (x, s)
        in ((x', y), s')
```

### Parser Arrow

```haskell
import Control.Arrow
import Text.Parsec

-- Parsec parsers form arrows
parseNumber :: Parsec String () Int
parseNumber = read <$> many1 digit

parseDate :: Parsec String () (Int, Int, Int)
parseDate = proc _ -> do
    day <- parseNumber -< ()
    _ <- char '-' -< ()
    month <- parseNumber -< ()
    _ <- char '-' -< ()
    year <- parseNumber -< ()
    returnA -< (year, month, day)
```

## Arrow Notation

Arrow notation (proc notation) provides syntactic sugar:

```haskell
{-# LANGUAGE Arrows #-}

-- Arrow notation
addAndMultiply :: Arrow a => a (Int, Int) Int
addAndMultiply = proc (x, y) -> do
    sum <- arr (+) -< (x, y)
    product <- arr (*) -< (x, y)
    arr (+) -< (sum, product)

-- Desugared
addAndMultiply' :: Arrow a => a (Int, Int) Int
addAndMultiply' =
    arr (\(x, y) -> (x, y)) >>>
    (arr (+) *** arr (*)) >>>
    arr (\(sum, prod) -> sum + prod)
```

## Common Use Cases

### 1. Stream Processing

```haskell
import Control.Arrow

-- Processing data streams
processStream :: Arrow a => a Int String
processStream = proc x -> do
    doubled <- arr (*2) -< x
    incremented <- arr (+1) -< doubled
    result <- arr show -< incremented
    returnA -< result
```

### 2. Signal Functions (FRP)

```haskell
-- Reactive programming with arrows
type SF a b = Arrow a => a b

integral :: SF Double Double
integral = -- integration over time

derivative :: SF Double Double  
derivative = -- differentiation

-- Combining signal functions
controller :: SF Double Double
controller = proc input -> do
    integrated <- integral -< input
    derived <- derivative -< input
    let output = integrated + derived
    returnA -< output
```

### 3. Circuit Simulation

```haskell
import Control.Arrow

-- Digital circuit components
andGate :: Arrow a => a (Bool, Bool) Bool
andGate = arr (uncurry (&&))

orGate :: Arrow a => a (Bool, Bool) Bool
orGate = arr (uncurry (||))

notGate :: Arrow a => a Bool Bool
notGate = arr not

-- Building circuits
halfAdder :: Arrow a => a (Bool, Bool) (Bool, Bool)
halfAdder = proc (a, b) -> do
    sum <- arr (\(x, y) -> x /= y) -< (a, b)
    carry <- andGate -< (a, b)
    returnA -< (sum, carry)
```

### 4. Data Flow Programming

```haskell
-- ETL pipeline
extractTransformLoad :: Arrow a => a FilePath Stats
extractTransformLoad = proc path -> do
    raw <- readFileA -< path
    parsed <- parseDataA -< raw
    cleaned <- cleanDataA -< parsed
    stats <- calculateStatsA -< cleaned
    returnA -< stats
```

## Arrow Combinators

```haskell
-- Running arrows in parallel
(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')

-- Fanout: apply two arrows to the same input
(&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')

-- Choice: apply arrow to left or right of Either
(+++) :: ArrowChoice a => a b c -> a b' c' -> a (Either b b') (Either c c')

-- Fanin: merge two arrows
(|||) :: ArrowChoice a => a b d -> a c d -> a (Either b c) d

-- Loop with feedback
loop :: ArrowLoop a => a (b, d) (c, d) -> a b c
```

## Advantages Over Monads

1. **Static Analysis**: Arrow structure is more constrained, enabling optimization
2. **Bidirectionality**: Easier to implement bidirectional computations
3. **Resource Safety**: Better for managing resources and effects
4. **Composition**: More explicit composition operators

## Related Patterns

- **[Monad](monad)** - More flexible but less structured
- **[Applicative](applicative)** - Similar power for some computations
- **[Arrowized FRP](../advanced/arrowized-frp)** - Arrows for reactive programming

## Best Practices

1. **Use arrows for structured computations** with clear data flow
2. **Leverage arrow notation** for readable code
3. **Consider arrows for embedded DSLs** and circuit-like structures
4. **Use combinators** for composing complex behaviors
5. **Prefer monads for sequential logic** unless you need arrow features

## Further Reading

- [Generalising Monads to Arrows](http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf) - Original paper
- [Understanding Arrows](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows)
- [Programming with Arrows](http://www.cse.chalmers.se/~rjmh/Papers/arrows-jfp.pdf)
