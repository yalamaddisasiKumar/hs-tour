---
title: Lenses and Prisms
---

## Overview

**Lenses** and **Prisms** are part of the optics family - composable getters and setters that provide a functional way to access and modify nested data structures. They enable elegant manipulation of immutable data and solve the problem of updating deeply nested records.

## Core Concepts

- **Lens**: Focus on a part of a data structure (product types)
- **Prism**: Focus on a variant of a sum type
- **Composition**: Combine optics to navigate deep structures
- **Laws**: Mathematical properties ensuring correctness

## Type Signatures

```haskell
-- Simplified types
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Prism s t a b = forall p f. (Choice p, Applicative f) 
                   => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

-- Common operations
view :: Lens' s a -> s -> a
set :: Lens' s a -> a -> s -> s
over :: Lens' s a -> (a -> a) -> s -> s
```

## Lens Examples

### Basic Lens Usage

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

data Person = Person
    { _name :: String
    , _age :: Int
    , _address :: Address
    } deriving (Show)

data Address = Address
    { _street :: String
    , _city :: String
    , _zipCode :: String
    } deriving (Show)

-- Generate lenses automatically
makeLenses ''Person
makeLenses ''Address

-- Using lenses
person = Person "Alice" 30 (Address "123 Main St" "Springfield" "12345")

-- View (get)
personName = view name person         -- "Alice"
personName' = person ^. name          -- infix version

-- Set
updated = set age 31 person
updated' = person & age .~ 31         -- infix version

-- Modify (over)
older = over age (+1) person
older' = person & age %~ (+1)         -- infix version

-- Composing lenses
personCity = person ^. address . city                    -- "Springfield"
relocated = person & address . city .~ "New York"
```

### Manual Lens Construction

```haskell
-- Define a lens manually
nameLens :: Lens' Person String
nameLens = lens _name (\person newName -> person { _name = newName })

ageLens :: Lens' Person Int
ageLens = lens _age (\person newAge -> person { _age = newAge })

-- Usage is identical to generated lenses
name1 = view nameLens person
person2 = set nameLens "Bob" person
```

### Nested Data Manipulation

```haskell
data Company = Company
    { _companyName :: String
    , _ceo :: Person
    , _employees :: [Person]
    } deriving (Show)

makeLenses ''Company

company = Company "TechCorp" 
    (Person "John" 45 (Address "1 CEO Lane" "Tech City" "00001"))
    [ Person "Alice" 30 (Address "10 Worker St" "Tech City" "00002")
    , Person "Bob" 28 (Address "20 Worker St" "Tech City" "00003")
    ]

-- Deep nesting access
ceoCity = company ^. ceo . address . city
-- "Tech City"

-- Deep nesting update
newCompany = company & ceo . address . zipCode .~ "00100"

-- Update all employees' cities
updatedCompany = company & employees . traverse . address . city .~ "New Tech City"
```

### Lens Operators

```haskell
-- View
(^.) :: s -> Lens' s a -> a

-- Set
(.~) :: Lens' s a -> a -> s -> s

-- Modify
(%~) :: Lens' s a -> (a -> a) -> s -> s

-- Add to numeric field
(+~) :: Num a => Lens' s a -> a -> s -> s

-- Multiply numeric field
(*~) :: Num a => Lens' s a -> a -> s -> s

-- Concatenate
(<>~) :: Monoid a => Lens' s a -> a -> s -> s

-- Examples
person & age +~ 5                    -- add 5 to age
person & name <>~ " Smith"           -- append to name
person & address . city .~ "Boston"  -- set nested city
```

## Prism Examples

### Basic Prism Usage

```haskell
import Control.Lens

data Result a
    = Success a
    | Failure String
    deriving (Show)

-- Define prisms
_Success :: Prism' (Result a) a
_Success = prism' Success $ \case
    Success a -> Just a
    _ -> Nothing

_Failure :: Prism' (Result a) String
_Failure = prism' Failure $ \case
    Failure s -> Just s
    _ -> Nothing

-- Preview (try to view)
result1 = Success 42
result2 = Failure "error"

value1 = preview _Success result1    -- Just 42
value1' = result1 ^? _Success        -- infix version

value2 = preview _Success result2    -- Nothing

-- Review (construct)
newSuccess = review _Success 100     -- Success 100
newSuccess' = _Success # 100         -- infix version

-- Modify if possible
incremented = result1 & _Success %~ (+1)    -- Success 43
unchanged = result2 & _Success %~ (+1)      -- Failure "error"
```

### Either Prisms

```haskell
-- Built-in Either prisms
value :: Either String Int
value = Right 42

-- Access right value
rightVal = value ^? _Right           -- Just 42
leftVal = value ^? _Left             -- Nothing

-- Modify
doubled = value & _Right %~ (*2)     -- Right 84

-- Pattern matching with prisms
case value of
    Right n -> print n
    Left err -> print err

-- Equivalent with prisms
case value ^? _Right of
    Just n -> print n
    Nothing -> case value ^? _Left of
        Just err -> print err
        Nothing -> return ()
```

### Custom Sum Type Prisms

```haskell
data Shape
    = Circle Double
    | Rectangle Double Double
    | Triangle Double Double Double
    deriving (Show)

-- Generate prisms with Template Haskell
makePrisms ''Shape

-- Or define manually
_Circle :: Prism' Shape Double
_Circle = prism' Circle $ \case
    Circle r -> Just r
    _ -> Nothing

_Rectangle :: Prism' Shape (Double, Double)
_Rectangle = prism' (\(w, h) -> Rectangle w h) $ \case
    Rectangle w h -> Just (w, h)
    _ -> Nothing

-- Usage
shape = Circle 5.0

radius = shape ^? _Circle                    -- Just 5.0
area = shape & _Circle %~ (*2)               -- Circle 10.0

-- Type-safe manipulation
shapes :: [Shape]
shapes = [Circle 5, Rectangle 3 4, Triangle 3 4 5]

-- Get all circle radii
radii = shapes ^.. traverse . _Circle        -- [5.0]

-- Double all circle radii
biggerCircles = shapes & traverse . _Circle %~ (*2)
```

## Advanced Optics

### Traversals

```haskell
-- Traverse multiple elements
data Department = Department
    { _deptName :: String
    , _deptEmployees :: [Person]
    } deriving (Show)

makeLenses ''Department

dept = Department "Engineering" 
    [ Person "Alice" 30 (Address "1 St" "City" "12345")
    , Person "Bob" 28 (Address "2 St" "City" "12345")
    ]

-- View all names
names = dept ^.. deptEmployees . traverse . name
-- ["Alice", "Bob"]

-- Modify all ages
olderDept = dept & deptEmployees . traverse . age +~ 1

-- Filter with traversal
adults = dept ^.. deptEmployees . traverse . filtered (\p -> p ^. age >= 18)
```

### Indexed Traversals

```haskell
import Control.Lens

list = ["a", "b", "c"]

-- Access with index
indexed = list ^@.. traversed
-- [(0,"a"), (1,"b"), (2,"c")]

-- Modify based on index
modified = list & traversed %@~ \i x -> show i ++ ":" ++ x
-- ["0:a", "1:b", "2:c"]
```

### Folds

```haskell
-- Fold over multiple values
total = company ^. employees . traverse . age . to sum
average = company ^. employees . to (avgAge)
  where avgAge es = sum (es ^.. traverse . age) / fromIntegral (length es)

-- folding combinator
sumAges = company ^. employees . folding (map (^. age)) . to sum
```

### Isos (Isomorphisms)

```haskell
-- Bidirectional conversions
celsiusToFahrenheit :: Iso' Double Double
celsiusToFahrenheit = iso (\c -> c * 9/5 + 32) (\f -> (f - 32) * 5/9)

-- Usage
temp = 100 ^. from celsiusToFahrenheit    -- 37.78
temp2 = 37.78 ^. celsiusToFahrenheit      -- 100

-- Swapping pairs
swapped :: Iso (a, b) (c, d) (b, a) (d, c)
pair = (1, "hello") ^. swapped            -- ("hello", 1)
```

## Practical Examples

### JSON Manipulation

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson.Lens

-- Navigate JSON
json = object ["user" .= object ["name" .= "Alice", "age" .= (30 :: Int)]]

userName = json ^? key "user" . key "name" . _String
-- Just "Alice"

-- Update JSON
updated = json & key "user" . key "age" . _Number .~ 31
```

### State Manipulation

```haskell
import Control.Lens
import Control.Monad.State

data GameState = GameState
    { _player :: Player
    , _score :: Int
    , _level :: Int
    } deriving (Show)

data Player = Player
    { _health :: Int
    , _position :: (Int, Int)
    } deriving (Show)

makeLenses ''GameState
makeLenses ''Player

-- Stateful operations with lenses
takeDamage :: Int -> State GameState ()
takeDamage damage = player . health -= damage

movePlayer :: (Int, Int) -> State GameState ()
movePlayer newPos = player . position .= newPos

increaseScore :: Int -> State GameState ()
increaseScore points = score += points

-- Compose state operations
gameStep :: State GameState ()
gameStep = do
    takeDamage 10
    movePlayer (5, 5)
    increaseScore 100
```

### Configuration Management

```haskell
data Config = Config
    { _database :: DatabaseConfig
    , _server :: ServerConfig
    , _logging :: LogConfig
    } deriving (Show)

data DatabaseConfig = DatabaseConfig
    { _host :: String
    , _port :: Int
    , _credentials :: Credentials
    } deriving (Show)

makeLenses ''Config
makeLenses ''DatabaseConfig

-- Easy configuration updates
config = defaultConfig
    & database . host .~ "localhost"
    & database . port .~ 5432
    & server . timeout .~ 30
    & logging . level .~ Debug
```

## Lens Laws

```haskell
-- Get-Put: You get back what you put
view l (set l v s) ≡ v

-- Put-Get: Putting back what you got changes nothing
set l (view l s) s ≡ s

-- Put-Put: The second put wins
set l v' (set l v s) ≡ set l v' s
```

## Prism Laws

```haskell
-- Review-Preview: You can preview what you review
preview l (review l b) ≡ Just b

-- Prism complement: If preview fails, the value is unchanged
case preview l s of
    Nothing -> s
    Just a -> review l a
```

## Related Patterns

- **[Typeclasses](../basic/typeclasses)** - Lenses use typeclasses heavily
- **[Functor](../basic/functor)** - Foundation for lens definitions
- **[Type-Level Programming](type-level-programming)** - Advanced lens types

## Best Practices

1. **Use Template Haskell**: Generate lenses automatically with `makeLenses`
2. **Compose optics**: Build complex operations from simple ones
3. **Use operator versions**: They're more concise for composition
4. **Prefix with underscore**: Convention for fields that will have lenses
5. **Consider performance**: Lenses have some overhead
6. **Learn operators gradually**: Start with basic `^.`, `.~`, `%~`

## Common Operators Summary

```haskell
-- View
(^.)   :: s -> Lens' s a -> a                    -- view
(^?)   :: s -> Prism' s a -> Maybe a             -- preview
(^..)  :: s -> Traversal' s a -> [a]             -- toListOf

-- Set
(.~)   :: Lens' s a -> a -> s -> s               -- set
(?~)   :: Prism' s a -> a -> s -> s              -- set if possible

-- Modify
(%~)   :: Lens' s a -> (a -> a) -> s -> s        -- over
(?~)   :: Prism' s a -> (a -> a) -> s -> s       -- over if possible

-- Numeric
(+~)   :: Num a => Lens' s a -> a -> s -> s      -- add
(-~)   :: Num a => Lens' s a -> a -> s -> s      -- subtract
(*~)   :: Num a => Lens' s a -> a -> s -> s      -- multiply

-- Monoid
(<>~)  :: Monoid a => Lens' s a -> a -> s -> s   -- append

-- State
(.=)   :: MonadState s m => Lens' s a -> a -> m ()
(%=)   :: MonadState s m => Lens' s a -> (a -> a) -> m ()
(+=)   :: (MonadState s m, Num a) => Lens' s a -> a -> m ()
```

## Further Reading

- [Lens Library](https://hackage.haskell.org/package/lens)
- [Lens Tutorial](https://github.com/ekmett/lens/wiki/Tutorial)
- [A Little Lens Starter Tutorial](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
- [Lenses In Pictures](http://adit.io/posts/2013-07-22-lenses-in-pictures.html)
- [Profunctor Optics](https://arxiv.org/abs/1703.10857) - The theory behind modern lens libraries
