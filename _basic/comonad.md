---
title: Comonad
---

## Overview

**Comonad** is the categorical dual of a Monad. While monads represent computations that produce values in a context, comonads represent context-dependent computations where you can extract a value from a context. They are particularly useful for representing space-like structures and context-dependent transformations.

## Core Concepts

- **Extract**: Get a value from a context
- **Extend**: Apply a context-dependent function
- **Duplicate**: Nest contexts
- **Local Computation**: Process based on surrounding context

## Type Signature

```haskell
class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    extend :: (w a -> b) -> w a -> w b
    
    -- Laws relating them
    extend f = fmap f . duplicate
    duplicate = extend id
```

### Comonad Laws

```haskell
-- Left identity
extend extract = id

-- Right identity
extract . extend f = f

-- Associativity
extend f . extend g = extend (f . extend g)
```

## Examples

### Identity Comonad

```haskell
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Comonad Identity where
    extract (Identity x) = x
    duplicate (Identity x) = Identity (Identity x)
    extend f w = Identity (f w)

-- Usage
value :: Identity Int
value = Identity 42

extracted :: Int
extracted = extract value  -- 42
```

### Non-Empty List Comonad

```haskell
data NonEmpty a = a :| [a]
    deriving (Show, Eq)

instance Functor NonEmpty where
    fmap f (x :| xs) = f x :| fmap f xs

instance Comonad NonEmpty where
    extract (x :| _) = x
    
    duplicate w@(x :| xs) = w :| case xs of
        [] -> []
        (y:ys) -> tails (y :| ys)
        where
            tails :: NonEmpty a -> [NonEmpty a]
            tails (x :| []) = []
            tails (x :| (y:ys)) = (y :| ys) : tails (y :| ys)
    
    extend f w@(_ :| xs) = f w :| case xs of
        [] -> []
        (y:ys) -> extract (extend f (y :| ys)) : extract (extend f (y :| ys))

-- Usage: Moving average
movingAverage :: NonEmpty Int -> Int
movingAverage (x :| xs) = (x + sum (take 2 xs)) `div` (1 + length (take 2 xs))

smoothed :: NonEmpty Int
smoothed = extend movingAverage (1 :| [2, 3, 4, 5])
```

### Store Comonad

```haskell
data Store s a = Store (s -> a) s

instance Functor (Store s) where
    fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
    extract (Store f s) = f s
    duplicate (Store f s) = Store (\s' -> Store f s') s
    extend g (Store f s) = Store (\s' -> g (Store f s')) s

-- Usage: Spreadsheet-like computation
type Position = (Int, Int)
type Grid a = Store Position a

neighborSum :: Grid Int -> Int
neighborSum (Store f (x, y)) = 
    f (x-1, y) + f (x+1, y) + f (x, y-1) + f (x, y+1)

-- Update all cells based on neighbors
updateGrid :: Grid Int -> Grid Int
updateGrid = extend neighborSum
```

### Traced Comonad

```haskell
newtype Traced m a = Traced { runTraced :: m -> a }

instance Functor (Traced m) where
    fmap f (Traced g) = Traced (f . g)

instance Monoid m => Comonad (Traced m) where
    extract (Traced f) = f mempty
    duplicate (Traced f) = Traced (\m -> Traced (\m' -> f (m <> m')))

-- Usage: Log-like queries
type Log = [String]
type LogQuery a = Traced Log a

hasError :: LogQuery Bool
hasError = Traced $ \log -> any (isPrefixOf "ERROR") log

filterMessages :: String -> LogQuery [String]
filterMessages prefix = Traced $ \log -> filter (isPrefixOf prefix) log
```

### Env (Product) Comonad

```haskell
data Env e a = Env e a

instance Functor (Env e) where
    fmap f (Env e a) = Env e (f a)

instance Comonad (Env e) where
    extract (Env _ a) = a
    duplicate (Env e a) = Env e (Env e a)
    extend f w@(Env e _) = Env e (f w)

-- Usage: Configuration-dependent computation
type Config = String
type WithConfig a = Env Config a

processWithConfig :: WithConfig Int -> Int
processWithConfig (Env config value) = 
    if config == "double" then value * 2 else value

result :: WithConfig Int
result = extend processWithConfig (Env "double" 21)
-- extract result == 42
```

## Conway's Game of Life Example

```haskell
{-# LANGUAGE DeriveFunctor #-}

import Control.Comonad

-- 2D grid using Store comonad
type Coord = (Int, Int)
type Grid = Store Coord

-- Cell state
data Cell = Alive | Dead deriving (Eq, Show)

-- Count living neighbors
neighbors :: [Coord]
neighbors = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

countNeighbors :: Grid Cell -> Int
countNeighbors (Store f pos) = 
    length $ filter (== Alive) $ map (\(dx, dy) -> 
        let (x, y) = pos in f (x + dx, y + dy)) neighbors

-- Game of Life rule
rule :: Grid Cell -> Cell
rule g = case (extract g, countNeighbors g) of
    (Alive, 2) -> Alive
    (Alive, 3) -> Alive
    (Dead, 3) -> Alive
    _ -> Dead

-- Step the simulation
step :: Grid Cell -> Grid Cell
step = extend rule

-- Run simulation
simulate :: Int -> Grid Cell -> Grid Cell
simulate 0 grid = grid
simulate n grid = simulate (n - 1) (step grid)
```

## Image Processing Example

```haskell
import Control.Comonad
import Data.Word (Word8)

type Pixel = Word8
type Coord = (Int, Int)
type Image = Store Coord Pixel

-- Gaussian blur kernel
gaussianBlur :: Image -> Pixel
gaussianBlur (Store f (x, y)) = 
    let kernel = 
            [ ((-1,-1), 1), ((0,-1), 2), ((1,-1), 1)
            , ((-1, 0), 2), ((0, 0), 4), ((1, 0), 2)
            , ((-1, 1), 1), ((0, 1), 2), ((1, 1), 1)
            ]
        total = sum $ map snd kernel
        weighted = sum [ w * fromIntegral (f (x + dx, y + dy)) 
                       | ((dx, dy), w) <- kernel ]
    in round (weighted / fromIntegral total)

-- Apply blur to entire image
blur :: Image -> Image
blur = extend gaussianBlur

-- Edge detection
edgeDetect :: Image -> Pixel
edgeDetect (Store f (x, y)) =
    let center = f (x, y)
        diff = [ abs (fromIntegral (f (x + dx, y + dy)) - fromIntegral center)
               | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]
    in if sum diff > 100 then 255 else 0

detectEdges :: Image -> Image
detectEdges = extend edgeDetect
```

## Common Use Cases

### 1. Cellular Automata

```haskell
type Universe = Store Int Cell

rule30 :: Universe -> Cell
rule30 (Store f pos) = 
    case (f (pos - 1), f pos, f (pos + 1)) of
        (Alive, Alive, Alive) -> Dead
        (Alive, Alive, Dead) -> Dead
        -- ... other rules
        _ -> Dead

evolve :: Universe -> Universe
evolve = extend rule30
```

### 2. Focus-Based Data Structures

```haskell
data Zipper a = Zipper [a] a [a]

instance Functor Zipper where
    fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

instance Comonad Zipper where
    extract (Zipper _ x _) = x
    duplicate z = Zipper (tail $ iterate left z) z (tail $ iterate right z)

left :: Zipper a -> Zipper a
left (Zipper (l:ls) x rs) = Zipper ls l (x:rs)

right :: Zipper a -> Zipper a
right (Zipper ls x (r:rs)) = Zipper (x:ls) r rs
```

### 3. Context-Aware Transformations

```haskell
-- Syntax highlighting based on context
type Token = String
type TokenStream = Store Int Token

highlightToken :: TokenStream -> (Token, Color)
highlightToken (Store f pos) =
    let current = f pos
        prev = f (pos - 1)
        next = f (pos + 1)
    in case (prev, current, next) of
        (_, "if", _) -> (current, Blue)
        (_, "class", _) -> (current, Blue)
        _ -> (current, Black)

highlight :: TokenStream -> Store Int (Token, Color)
highlight = extend highlightToken
```

### 4. Spreadsheet Calculations

```haskell
type CellRef = (Int, Int)
type Sheet a = Store CellRef a

-- Formula that depends on neighbors
sumAround :: Sheet Double -> Double
sumAround (Store f (row, col)) = sum
    [ f (row + dr, col + dc) 
    | dr <- [-1..1], dc <- [-1..1]
    , (dr, dc) /= (0, 0)
    ]

-- Recalculate all cells
recalculate :: Sheet Double -> Sheet Double
recalculate = extend sumAround
```

## Relationship with Monads

```haskell
-- Monad
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

-- Comonad (arrows reversed!)
class Comonad w where
    extract :: w a -> a
    extend :: (w a -> b) -> w a -> w b
    
-- Moadic bind produces effects
-- Comonadic extend consumes context
```

## Related Patterns

- **[Monad](monad)** - Categorical dual
- **[Functor](functor)** - Base requirement
- **[Zipper Data Structures](https://en.wikibooks.org/wiki/Haskell/Zippers)** - Common comonadic structure

## Best Practices

1. **Use for spatial/contextual data**: Comonads excel at context-dependent computation
2. **Consider Store for indexed lookups**: Grid-like or positional data
3. **Use NonEmpty for streams**: When you need access to neighbors
4. **Leverage extend for transformations**: Apply context-aware functions
5. **Think about focus**: Comonads are about having a "current" position

## When to Use Comonads

- **Image processing**: Operations depending on neighboring pixels
- **Cellular automata**: Cell updates based on neighbors
- **Stream processing**: Operations on current element and context
- **UI focus management**: Current widget and surroundings
- **Zipper operations**: Moving through data with focus

## Further Reading

- [Comonads in Haskell](https://www.schoolofhaskell.com/user/edwardk/cellular-automata)
- [The Comonad.Reader](http://comonad.com/reader/)
- [Conal Elliott - What is a Comonad?](http://conal.net/blog/posts/what-is-a-comonad)
- [Comonads and Day Convolution](https://bartoszmilewski.com/2017/06/06/comonads/)
