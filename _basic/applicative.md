---
title: Applicative
---

## Overview

**Applicative** functors extend the Functor pattern by allowing functions that are themselves wrapped in a context to be applied to values in a context. This enables working with multiple independent computations in parallel.

## Core Concepts

- **Lifted Application**: Apply wrapped functions to wrapped values
- **Independent Effects**: Combine multiple independent computations
- **Sequencing**: Execute effects in a specific order

## Type Signature

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    
    -- Alternative operators
    (*>) :: f a -> f b -> f b
    (<*) :: f a -> f b -> f a
    
    -- Lift functions
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
```

### Applicative Laws

```haskell
-- Identity
pure id <*> v = v

-- Composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- Homomorphism
pure f <*> pure x = pure (f x)

-- Interchange
u <*> pure y = pure ($ y) <*> u
```

## Examples

### Basic List Applicative

```haskell
-- Applying multiple functions to multiple values
result1 :: [Int]
result1 = [(+1), (*2)] <*> [1, 2, 3]
-- Result: [2, 3, 4, 2, 4, 6]

-- Using liftA2
result2 :: [Int]
result2 = liftA2 (+) [1, 2] [10, 20]
-- Result: [11, 21, 12, 22]
```

### Maybe Applicative

```haskell
data Person = Person 
    { name :: String
    , age :: Int
    , email :: String
    } deriving (Show)

-- Constructing a Person with optional fields
makePerson :: Maybe String -> Maybe Int -> Maybe String -> Maybe Person
makePerson mName mAge mEmail = 
    Person <$> mName <*> mAge <*> mEmail

result3 :: Maybe Person
result3 = makePerson (Just "Alice") (Just 30) (Just "alice@example.com")
-- Result: Just (Person "Alice" 30 "alice@example.com")

result4 :: Maybe Person
result4 = makePerson (Just "Bob") Nothing (Just "bob@example.com")
-- Result: Nothing
```

### Validation with Applicative

```haskell
import Control.Applicative (liftA3)

validateName :: String -> Maybe String
validateName n 
    | length n > 0 = Just n
    | otherwise = Nothing

validateAge :: Int -> Maybe Int
validateAge a 
    | a >= 0 && a <= 120 = Just a
    | otherwise = Nothing

validateEmail :: String -> Maybe String
validateEmail e 
    | '@' `elem` e = Just e
    | otherwise = Nothing

createValidPerson :: String -> Int -> String -> Maybe Person
createValidPerson n a e = 
    liftA3 Person (validateName n) (validateAge a) (validateEmail e)
```

### IO Applicative

```haskell
-- Combining multiple IO actions
main :: IO ()
main = do
    let action = liftA2 (++) getLine getLine
    result <- action
    putStrLn $ "Combined: " ++ result
```

### Parser Applicative

```haskell
-- Hypothetical parser example
data Parser a = Parser (String -> Maybe (a, String))

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    (Parser pf) <*> (Parser px) = Parser $ \s -> do
        (f, s') <- pf s
        (x, s'') <- px s'
        return (f x, s'')

-- Parsing structured data
data Date = Date Int Int Int

parseDate :: Parser Date
parseDate = Date <$> parseInt <*> parseInt <*> parseInt
```

## Common Use Cases

### 1. Form Validation

```haskell
data FormData = FormData
    { username :: String
    , password :: String
    , confirmPassword :: String
    }

validateForm :: FormData -> Maybe FormData
validateForm form =
    FormData 
        <$> validateUsername (username form)
        <*> validatePassword (password form)
        <*> validateConfirm (password form) (confirmPassword form)
```

### 2. Parallel Computation

```haskell
-- Computing multiple independent values
type Computation a = IO a

computeResult :: Computation Int
computeResult = 
    (+) <$> expensiveCalc1 <*> expensiveCalc2
```

### 3. Configuration Building

```haskell
data Config = Config
    { host :: String
    , port :: Int
    , timeout :: Int
    }

loadConfig :: IO Config
loadConfig = 
    Config 
        <$> getEnv "HOST"
        <*> (read <$> getEnv "PORT")
        <*> (read <$> getEnv "TIMEOUT")
```

### 4. Combining Optional Values

```haskell
-- Combining lookup results
lookupUser :: Int -> Maybe User
lookupAddress :: Int -> Maybe Address
lookupPhone :: Int -> Maybe Phone

getUserInfo :: Int -> Maybe (User, Address, Phone)
getUserInfo id = 
    (,,) <$> lookupUser id 
         <*> lookupAddress id 
         <*> lookupPhone id
```

## Applicative Style

The applicative style allows for clean, declarative code:

```haskell
-- Applicative style
result = f <$> x <*> y <*> z

-- Equivalent monadic style
result = do
    x' <- x
    y' <- y
    z' <- z
    return (f x' y' z')
```

## Related Patterns

- **[Functor](functor)** - Base pattern for mapping
- **[Monad](monad)** - Extends with dependent sequencing
- **[Arrow](arrow)** - Alternative approach to computation

## Best Practices

1. **Use applicative style when effects are independent** - it's more efficient and clearer
2. **Prefer `<$>` and `<*>` over `liftA2`, `liftA3`** for better readability
3. **Use `*>` and `<*` to sequence effects** while discarding results
4. **Choose Applicative over Monad** when you don't need the value of one computation to determine the next

## Further Reading

- [Typeclassopedia - Applicative](http://www.haskell.org/haskellwiki/Typeclassopedia#Applicative)
- [The Essence of the Iterator Pattern](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf)
