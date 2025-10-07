---
title: Monad
---

## Overview

**Monad** is a powerful abstraction that allows sequencing computations where each step may depend on the results of previous steps. Monads provide a way to structure programs generically while handling effects like state, I/O, exceptions, and more.

## Core Concepts

- **Sequential Composition**: Chain operations where each depends on the previous result
- **Effect Management**: Handle side effects in a pure functional way
- **Context Propagation**: Automatically manage computational context

## Type Signature

```haskell
class Applicative m => Monad m where
    return :: a -> m a  -- same as pure
    (>>=) :: m a -> (a -> m b) -> m b  -- bind operator
    
    (>>) :: m a -> m b -> m b
    m >> n = m >>= \_ -> n
    
    fail :: String -> m a  -- deprecated
```

### Monad Laws

```haskell
-- Left Identity
return a >>= f  ≡  f a

-- Right Identity
m >>= return  ≡  m

-- Associativity
(m >>= f) >>= g  ≡  m >>= (\x -> f x >>= g)
```

## Examples

### Maybe Monad

```haskell
-- Safe division with chaining
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

safeRoot :: Double -> Maybe Double
safeRoot x 
    | x < 0 = Nothing
    | otherwise = Just (sqrt x)

-- Chaining operations
compute :: Double -> Double -> Maybe Double
compute x y = do
    result1 <- safeDivide x y
    result2 <- safeDivide result1 2
    safeRoot result2

-- Using bind operator
compute' :: Double -> Double -> Maybe Double
compute' x y = 
    safeDivide x y >>= 
    \r1 -> safeDivide r1 2 >>= 
    \r2 -> safeRoot r2
```

### List Monad

```haskell
-- Non-deterministic computation
pairs :: [Int] -> [Int] -> [(Int, Int)]
pairs xs ys = do
    x <- xs
    y <- ys
    return (x, y)

-- List comprehension equivalent
pairs' xs ys = [(x, y) | x <- xs, y <- ys]

-- Filtering with guard
import Control.Monad (guard)

pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = do
    a <- [1..n]
    b <- [a..n]
    c <- [b..n]
    guard (a^2 + b^2 == c^2)
    return (a, b, c)
```

### IO Monad

```haskell
-- Sequencing IO operations
greetUser :: IO ()
greetUser = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ "!"
    
-- File operations
processFile :: FilePath -> IO ()
processFile path = do
    contents <- readFile path
    let processed = map toUpper contents
    writeFile (path ++ ".processed") processed
```

### Either Monad

```haskell
data Error = DivByZero | NegativeRoot | ParseError String
    deriving (Show, Eq)

divide :: Double -> Double -> Either Error Double
divide _ 0 = Left DivByZero
divide x y = Right (x / y)

squareRoot :: Double -> Either Error Double
squareRoot x 
    | x < 0 = Left NegativeRoot
    | otherwise = Right (sqrt x)

parse :: String -> Either Error Double
parse s = case reads s of
    [(n, "")] -> Right n
    _ -> Left (ParseError s)

-- Chaining with error handling
compute :: String -> String -> Either Error Double
compute sx sy = do
    x <- parse sx
    y <- parse sy
    result <- divide x y
    squareRoot result
```

### State Monad

```haskell
import Control.Monad.State

type Stack = [Int]

push :: Int -> State Stack ()
push x = modify (x:)

pop :: State Stack (Maybe Int)
pop = do
    stack <- get
    case stack of
        [] -> return Nothing
        (x:xs) -> do
            put xs
            return (Just x)

-- Stack computation
stackManip :: State Stack Int
stackManip = do
    push 3
    push 5
    a <- pop
    push 8
    b <- pop
    return (fromMaybe 0 a + fromMaybe 0 b)

result = evalState stackManip []
-- Result: 13
```

## Common Use Cases

### 1. Error Handling

```haskell
validateUser :: User -> Either ValidationError User
validateUser user = do
    validateName (userName user)
    validateEmail (userEmail user)
    validateAge (userAge user)
    return user
```

### 2. Stateful Computations

```haskell
type Counter = State Int

increment :: Counter ()
increment = modify (+1)

getCount :: Counter Int
getCount = get

program :: Counter Int
program = do
    increment
    increment
    increment
    getCount
```

### 3. Optional Chaining

```haskell
data User = User { userId :: Int, name :: String }
data Post = Post { postId :: Int, authorId :: Int }
data Comment = Comment { commentId :: Int, postId :: Int }

getUser :: Int -> Maybe User
getPost :: Int -> Maybe Post
getComment :: Int -> Maybe Comment

getCommentAuthor :: Int -> Maybe User
getCommentAuthor commentId = do
    comment <- getComment commentId
    post <- getPost (postId comment)
    getUser (authorId post)
```

### 4. Parser Combinators

```haskell
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Monad Parser where
    return x = Parser $ \s -> Just (x, s)
    p >>= f = Parser $ \s -> do
        (x, s') <- parse p s
        parse (f x) s'

char :: Char -> Parser Char
char c = Parser $ \s -> case s of
    (x:xs) | x == c -> Just (c, xs)
    _ -> Nothing

string :: String -> Parser String
string str = Parser $ \s -> 
    if str `isPrefixOf` s
        then Just (str, drop (length str) s)
        else Nothing
```

## Do-Notation

Do-notation is syntactic sugar for monadic operations:

```haskell
-- Do-notation
example = do
    x <- action1
    y <- action2 x
    action3 x y

-- Desugared
example = 
    action1 >>= \x ->
    action2 x >>= \y ->
    action3 x y
```

## Monad Functions

```haskell
-- Sequencing
sequence :: Monad m => [m a] -> m [a]
sequence_ :: Monad m => [m a] -> m ()

-- Mapping
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

-- Filtering
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]

-- Folding
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b

-- Control
when :: Monad m => Bool -> m () -> m ()
unless :: Monad m => Bool -> m () -> m ()
```

## Related Patterns

- **[Functor](functor)** - Foundation for mapping
- **[Applicative](applicative)** - Independent effects
- **[Monad Transformer](monad-transformer)** - Combining multiple monads
- **[Free Monad](../advanced/free-monad)** - Separating structure from interpretation

## Best Practices

1. **Use Applicative when possible** - it's more efficient and composable
2. **Avoid monad transformers** unless necessary - they add complexity
3. **Keep monadic code focused** - separate pure and effectful code
4. **Use helper functions** like `mapM`, `sequence` for common patterns
5. **Consider alternatives** like Arrows or Free Monads for complex cases

## Further Reading

- [All About Monads](https://wiki.haskell.org/All_About_Monads)
- [You Could Have Invented Monads!](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
- [Typeclassopedia - Monad](http://www.haskell.org/haskellwiki/Typeclassopedia#Monad)
