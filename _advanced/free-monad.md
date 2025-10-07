---
title: Free Monad
---

## Overview

**Free Monads** provide a way to build monads from functors, separating the structure of computations from their interpretation. They enable you to build abstract syntax trees (ASTs) for domain-specific languages and interpret them in multiple ways.

## Core Concepts

- **Structure vs. Interpretation**: Separate what to do from how to do it
- **AST Construction**: Build computation trees
- **Multiple Interpreters**: Different meanings for same structure
- **Composability**: Combine operations freely

## Type Signature

```haskell
data Free f a
    = Pure a
    | Free (f (Free f a))

instance Functor f => Monad (Free f) where
    return = Pure
    Pure a >>= f = f a
    Free m >>= f = Free (fmap (>>= f) m)

-- Lift functor into Free monad
liftF :: Functor f => f a -> Free f a
liftF = Free . fmap Pure
```

## Examples

### Basic Free Monad

```haskell
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free

-- Define operations as a functor
data ToyF next
    = Output String next
    | Bell next
    | Done
    deriving (Functor)

type Toy = Free ToyF

-- Smart constructors
output :: String -> Toy ()
output s = liftF (Output s ())

bell :: Toy ()
bell = liftF (Bell ())

done :: Toy ()
done = liftF Done

-- Build programs
program :: Toy ()
program = do
    output "Hello"
    bell
    output "World"
    done

-- Interpreter
runToy :: Toy a -> IO ()
runToy (Pure _) = return ()
runToy (Free (Output s next)) = putStrLn s >> runToy next
runToy (Free (Bell next)) = putStrLn "DING!" >> runToy next
runToy (Free Done) = return ()
```

### File System DSL

```haskell
{-# LANGUAGE DeriveFunctor #-}

data FileSystemF next
    = ReadFile FilePath (String -> next)
    | WriteFile FilePath String next
    | DeleteFile FilePath next
    deriving (Functor)

type FileSystem = Free FileSystemF

-- Smart constructors
readFile' :: FilePath -> FileSystem String
readFile' path = liftF (ReadFile path id)

writeFile' :: FilePath -> String -> FileSystem ()
writeFile' path content = liftF (WriteFile path content ())

deleteFile' :: FilePath -> FileSystem ()
deleteFile' path = liftF (DeleteFile path ())

-- Example program
backup :: FilePath -> FilePath -> FileSystem ()
backup source dest = do
    content <- readFile' source
    writeFile' dest content

-- IO Interpreter
runIO :: FileSystem a -> IO a
runIO (Pure a) = return a
runIO (Free (ReadFile path next)) = do
    content <- readFile path
    runIO (next content)
runIO (Free (WriteFile path content next)) = do
    writeFile path content
    runIO next
runIO (Free (DeleteFile path next)) = do
    removeFile path
    runIO next

-- Pure interpreter (for testing)
runPure :: [(FilePath, String)] -> FileSystem a -> (a, [String])
runPure _ (Pure a) = (a, [])
runPure files (Free (ReadFile path next)) =
    case lookup path files of
        Just content -> 
            let (a, log) = runPure files (next content)
            in (a, ("Read: " ++ path) : log)
        Nothing -> error "File not found"
runPure files (Free (WriteFile path content next)) =
    let (a, log) = runPure files next
    in (a, ("Wrote: " ++ path) : log)
runPure files (Free (DeleteFile path next)) =
    let (a, log) = runPure files next
    in (a, ("Deleted: " ++ path) : log)
```

### Console DSL

```haskell
data ConsoleF next
    = WriteLine String next
    | ReadLine (String -> next)
    deriving (Functor)

type Console = Free ConsoleF

writeLine :: String -> Console ()
writeLine s = liftF (WriteLine s ())

readLine :: Console String
readLine = liftF (ReadLine id)

-- Interactive program
greet :: Console ()
greet = do
    writeLine "What's your name?"
    name <- readLine
    writeLine $ "Hello, " ++ name ++ "!"

-- IO interpreter
runConsoleIO :: Console a -> IO a
runConsoleIO (Pure a) = return a
runConsoleIO (Free (WriteLine s next)) = putStrLn s >> runConsoleIO next
runConsoleIO (Free (ReadLine next)) = getLine >>= runConsoleIO . next

-- Test interpreter with predefined inputs
runConsoleTest :: [String] -> Console a -> a
runConsoleTest _ (Pure a) = a
runConsoleTest inputs (Free (WriteLine _ next)) = 
    runConsoleTest inputs next
runConsoleTest (input:rest) (Free (ReadLine next)) = 
    runConsoleTest rest (next input)
runConsoleTest [] (Free (ReadLine _)) = 
    error "No more test inputs"
```

### HTTP Client DSL

```haskell
{-# LANGUAGE GADTs #-}

data HttpF next where
    Get :: URL -> (Response -> next) -> HttpF next
    Post :: URL -> Body -> (Response -> next) -> HttpF next
    Put :: URL -> Body -> (Response -> next) -> HttpF next
    Delete :: URL -> (Response -> next) -> HttpF next

instance Functor HttpF where
    fmap f (Get url k) = Get url (f . k)
    fmap f (Post url body k) = Post url body (f . k)
    fmap f (Put url body k) = Put url body (f . k)
    fmap f (Delete url k) = Delete url (f . k)

type Http = Free HttpF

-- Smart constructors
get :: URL -> Http Response
get url = liftF (Get url id)

post :: URL -> Body -> Http Response
post url body = liftF (Post url body id)

-- Example API calls
apiWorkflow :: Http (Response, Response)
apiWorkflow = do
    user <- get "https://api.example.com/user/1"
    result <- post "https://api.example.com/posts" (createBody user)
    return (user, result)

-- Real HTTP interpreter
runHttp :: Http a -> IO a
runHttp (Pure a) = return a
runHttp (Free (Get url next)) = do
    response <- httpGet url  -- actual HTTP call
    runHttp (next response)
runHttp (Free (Post url body next)) = do
    response <- httpPost url body
    runHttp (next response)

-- Mock interpreter for testing
runHttpMock :: [(URL, Response)] -> Http a -> a
runHttpMock _ (Pure a) = a
runHttpMock mocks (Free (Get url next)) =
    case lookup url mocks of
        Just response -> runHttpMock mocks (next response)
        Nothing -> error $ "No mock for: " ++ url
runHttpMock mocks (Free (Post url _ next)) =
    case lookup url mocks of
        Just response -> runHttpMock mocks (next response)
        Nothing -> error $ "No mock for: " ++ url
```

## Coproducts - Combining Free Monads

```haskell
{-# LANGUAGE TypeOperators #-}

-- Coproduct (sum) of functors
data (f :+: g) a = Inl (f a) | Inr (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl fa) = Inl (fmap f fa)
    fmap f (Inr ga) = Inr (fmap f ga)

-- Injection type class
class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

instance Functor f => f :<: f where
    inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = Inr . inj

-- Generic lift
inject :: (g :<: f) => g a -> Free f a
inject = liftF . inj

-- Combining DSLs
type App = ConsoleF :+: FileSystemF :+: HttpF

-- Use multiple DSLs together
complexProgram :: Free App ()
complexProgram = do
    inject $ WriteLine "Starting process..." ()
    content <- inject $ ReadFile "config.json" id
    response <- inject $ Post "https://api.com/data" content id
    inject $ WriteLine "Done!" ()
```

## Freer Monad (Extensible Effects)

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- Freer monad with type-level list of effects
data Freer effs a where
    Pure :: a -> Freer effs a
    Bind :: Eff eff => eff a -> (a -> Freer effs b) -> Freer effs b

-- Effect member constraint
class Member eff effs where
    inject :: eff a -> Union effs a

-- State effect
data State s a where
    Get :: State s s
    Put :: s -> State s ()

-- Reader effect
data Reader r a where
    Ask :: Reader r r

-- Combining effects
program :: (Member (State Int) effs, Member (Reader String) effs) 
        => Freer effs ()
program = do
    config <- send Ask
    count <- send Get
    send $ Put (count + 1)
```

## Church Encoding (Performance)

```haskell
-- Church-encoded Free monad (more efficient)
newtype F f a = F { runF :: forall r. (a -> r) -> (f r -> r) -> r }

instance Functor f => Monad (F f) where
    return a = F $ \pure' _ -> pure' a
    m >>= f = F $ \pure' free' ->
        runF m (\a -> runF (f a) pure' free') free'

-- Conversion
toF :: Functor f => Free f a -> F f a
toF (Pure a) = return a
toF (Free fa) = F $ \_ free' -> free' (fmap ((`runF` pure') . toF) fa)
  where pure' = \a -> runF (return a)
```

## Common Use Cases

### 1. Testing

```haskell
-- Separate business logic from effects
businessLogic :: Free FileSystemF Report
businessLogic = do
    data <- readFile' "data.csv"
    let processed = process data
    writeFile' "report.txt" processed
    return (Report processed)

-- Test with mock
testResult = runPure [("data.csv", "test data")] businessLogic

-- Production with real IO
productionResult = runIO businessLogic
```

### 2. Transaction Systems

```haskell
data DatabaseF next
    = Begin next
    | Commit next
    | Rollback next
    | Query String (Result -> next)
    deriving (Functor)

type Database = Free DatabaseF

transaction :: Database a -> Database a
transaction action = do
    begin
    result <- action `catchError` \e -> rollback >> throwError e
    commit
    return result
```

### 3. Game Logic

```haskell
data GameF next
    = GetPlayerInput (Input -> next)
    | UpdateWorld World next
    | RenderScene Scene next
    | PlaySound Sound next
    deriving (Functor)

type Game = Free GameF

gameLoop :: Game ()
gameLoop = do
    input <- getPlayerInput
    world <- updateWorld input
    renderScene world
    when (not $ gameOver world) gameLoop
```

## Related Patterns

- **[Monad](../basic/monad)** - Free monad is a monad
- **[Tagless Final](tagless-final)** - Alternative approach
- **[Algebraic Effects](algebraic-effects)** - Modern alternative
- **[Monad Transformer](../basic/monad-transformer)** - Different composition method

## Advantages

1. **Testability**: Easy to write pure tests
2. **Multiple Interpretations**: Same code, different runners
3. **Introspection**: Can analyze program structure
4. **Composability**: Easy to combine effects

## Disadvantages

1. **Performance**: Slower than direct monadic code
2. **Complexity**: Requires understanding of advanced concepts
3. **Type Signatures**: Can become complex
4. **Stack Space**: Deep recursion can cause issues

## Best Practices

1. **Use smart constructors**: Hide `liftF` details
2. **Provide multiple interpreters**: Testing and production
3. **Consider Church encoding**: For performance-critical code
4. **Use coproducts carefully**: Don't over-complicate
5. **Document interpreters**: Each should have clear semantics

## Further Reading

- [Why free monads matter](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html)
- [Purify Code Using Free Monads](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html)
- [Free Monads for Less](https://markkarpov.com/post/free-monad-considered-harmful.html)
- [Free Monad package](https://hackage.haskell.org/package/free)
