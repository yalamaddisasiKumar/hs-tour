---
title: Algebraic Effects
---

## Overview

**Algebraic Effects** provide a modular and composable way to handle computational effects. They separate effect declaration from effect handling, enabling flexible interpretation of effects and solving many problems that monad transformers face.

## Core Concepts

- **Effect Declaration**: Define what effects exist
- **Effect Handlers**: Define how effects are interpreted
- **Composability**: Combine effects freely
- **Delimited Continuations**: Control flow manipulation

## Type Signatures

```haskell
-- Effect definition
data Eff effs a where
    Val :: a -> Eff '[] a
    E :: Member eff effs => eff a -> (a -> Eff effs b) -> Eff effs b

-- Running effects
run :: Eff '[] a -> a
runM :: Monad m => Eff '[m] a -> m a

-- Handler type
type Handler eff effs = forall a. eff a -> Eff effs a
```

## Examples Using Polysemy

### Basic State Effect

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Polysemy
import Polysemy.State

-- State effect is built-in
-- data State s m a where
--     Get :: State s m s
--     Put :: s -> State s m ()

-- Using state
computation :: Member (State Int) r => Sem r Int
computation = do
    modify (+1)
    modify (*2)
    get

-- Pure interpretation
result1 = run $ runState 5 computation
-- (12, 12) - final state and result

-- IO interpretation
result2 = runM $ runStateIO 5 computation
```

### Error Handling Effect

```haskell
import Polysemy
import Polysemy.Error

data AppError
    = NotFound String
    | Unauthorized
    | ValidationError String
    deriving (Show, Eq)

-- Function using error effect
validateAge :: Member (Error AppError) r => Int -> Sem r Int
validateAge age
    | age < 0 = throw (ValidationError "Age cannot be negative")
    | age > 150 = throw (ValidationError "Age too high")
    | otherwise = return age

findUser :: Member (Error AppError) r => Int -> Sem r User
findUser userId
    | userId <= 0 = throw (NotFound "Invalid user ID")
    | otherwise = return (User userId "Alice")

-- Combining effects
processUser :: (Member (Error AppError) r, Member (State Int) r) 
            => Int -> Sem r User
processUser userId = do
    count <- get
    when (count > 100) $ throw Unauthorized
    modify (+1)
    user <- findUser userId
    validateAge (userAge user)
    return user

-- Running with error handling
result :: Either AppError User
result = run $ runError $ evalState 0 $ processUser 42
```

### Custom Effects

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

import Polysemy

-- Define a logging effect
data Log m a where
    LogInfo :: String -> Log m ()
    LogError :: String -> Log m ()
    LogDebug :: String -> Log m ()

makeSem ''Log

-- Pure interpreter (collect logs)
runLogAsList :: Sem (Log ': r) a -> Sem r ([String], a)
runLogAsList = runState [] . reinterpret \case
    LogInfo msg -> modify (++ ["INFO: " ++ msg])
    LogError msg -> modify (++ ["ERROR: " ++ msg])
    LogDebug msg -> modify (++ ["DEBUG: " ++ msg])

-- IO interpreter
runLogIO :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
runLogIO = interpret \case
    LogInfo msg -> embed $ putStrLn $ "INFO: " ++ msg
    LogError msg -> embed $ putStrLn $ "ERROR: " ++ msg
    LogDebug msg -> embed $ putStrLn $ "DEBUG: " ++ msg

-- Using the effect
program :: Member Log r => Sem r Int
program = do
    logInfo "Starting computation"
    let result = 42
    logDebug $ "Result: " ++ show result
    logInfo "Computation complete"
    return result

-- Different interpretations
test1 = run $ runLogAsList program
-- (["INFO: Starting...", "DEBUG: Result: 42", "INFO: Complete"], 42)

test2 = runM $ runLogIO program
-- Prints to console
```

### File System Effect

```haskell
data FileSystem m a where
    ReadFile :: FilePath -> FileSystem m String
    WriteFile :: FilePath -> String -> FileSystem m ()
    DeleteFile :: FilePath -> FileSystem m ()
    ListDirectory :: FilePath -> FileSystem m [FilePath]

makeSem ''FileSystem

-- Real IO handler
runFileSystemIO :: Member (Embed IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret \case
    ReadFile path -> embed $ readFile path
    WriteFile path content -> embed $ writeFile path content
    DeleteFile path -> embed $ removeFile path
    ListDirectory path -> embed $ listDirectory path

-- Mock handler for testing
type FileStore = Map FilePath String

runFileSystemPure :: Sem (FileSystem ': r) a -> Sem r (FileStore, a)
runFileSystemPure = runState Map.empty . reinterpret \case
    ReadFile path -> do
        store <- get
        case Map.lookup path store of
            Just content -> return content
            Nothing -> return ""
    WriteFile path content -> modify (Map.insert path content)
    DeleteFile path -> modify (Map.delete path)
    ListDirectory _ -> Map.keys <$> get

-- Using the effect
backup :: (Member FileSystem r, Member Log r) => FilePath -> FilePath -> Sem r ()
backup source dest = do
    logInfo $ "Backing up " ++ source
    content <- readFile source
    writeFile dest content
    logInfo "Backup complete"
```

### HTTP Effect

```haskell
data Http m a where
    HttpGet :: URL -> Http m Response
    HttpPost :: URL -> Body -> Http m Response
    HttpPut :: URL -> Body -> Http m Response
    HttpDelete :: URL -> Http m Response

makeSem ''Http

-- Real implementation
runHttpReal :: Member (Embed IO) r => Sem (Http ': r) a -> Sem r a
runHttpReal = interpret \case
    HttpGet url -> embed $ performHttpGet url
    HttpPost url body -> embed $ performHttpPost url body
    HttpPut url body -> embed $ performHttpPut url body
    HttpDelete url -> embed $ performHttpDelete url

-- Mock implementation
type HttpMock = [(URL, Response)]

runHttpMock :: HttpMock -> Sem (Http ': r) a -> Sem r a
runHttpMock mocks = interpret \case
    HttpGet url -> return $ fromMaybe (Response 404 "") (lookup url mocks)
    HttpPost url _ -> return $ fromMaybe (Response 404 "") (lookup url mocks)
    HttpPut url _ -> return $ fromMaybe (Response 404 "") (lookup url mocks)
    HttpDelete url -> return $ fromMaybe (Response 404 "") (lookup url mocks)

-- API client using multiple effects
apiClient :: (Member Http r, Member Log r, Member (Error ApiError) r) 
          => Sem r User
apiClient = do
    logInfo "Fetching user data"
    response <- httpGet "https://api.example.com/user/1"
    when (responseCode response /= 200) $ 
        throw (ApiError "Failed to fetch user")
    logInfo "User data fetched successfully"
    return (parseUser response)
```

## Effect Interaction and Handlers

### Combining Multiple Effects

```haskell
-- Complex application with multiple effects
type AppEffects = 
    '[ State AppState
     , Error AppError
     , Log
     , FileSystem
     , Http
     , Embed IO
     ]

-- Application logic
app :: Sem AppEffects ()
app = do
    logInfo "Application started"
    
    -- Read configuration
    config <- readFile "config.json"
    put (parseConfig config)
    
    -- Fetch data from API
    response <- httpGet "https://api.example.com/data"
    
    -- Process and save
    state <- get
    let processed = processData state response
    writeFile "output.txt" processed
    
    logInfo "Application completed"

-- Run the application
main :: IO ()
main = do
    result <- runM
        . runHttpReal
        . runFileSystemIO
        . runLogIO
        . runError
        . evalState emptyState
        $ app
    
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "Success!"
```

### Effect Reinterpretation

```haskell
-- Reinterpret one effect in terms of others
data Cache m a where
    CacheGet :: Key -> Cache m (Maybe Value)
    CachePut :: Key -> Value -> Cache m ()

makeSem ''Cache

-- Implement cache using State
runCacheAsState :: Sem (Cache ': r) a -> Sem (State (Map Key Value) ': r) a
runCacheAsState = reinterpret \case
    CacheGet key -> Map.lookup key <$> get
    CachePut key value -> modify (Map.insert key value)

-- Implement cache using Redis (IO)
runCacheRedis :: Member (Embed IO) r => RedisConnection -> Sem (Cache ': r) a -> Sem r a
runCacheRedis conn = interpret \case
    CacheGet key -> embed $ Redis.get conn key
    CachePut key value -> embed $ Redis.set conn key value
```

### Higher-Order Effects

```haskell
-- Effect that can catch exceptions
data Catch m a where
    Catch :: m a -> (SomeException -> m a) -> Catch m a

-- Bracket effect for resource management
data Resource m a where
    Bracket :: m a -> (a -> m b) -> (a -> m c) -> Resource m c

-- Async effect
data Async m a where
    Fork :: m a -> Async m (Async.Async a)
    Wait :: Async.Async a -> Async m a
```

## Compared to Monad Transformers

```haskell
-- Monad Transformers
type App = ReaderT Config (StateT AppState (ExceptT AppError IO))

runApp :: Config -> AppState -> App a -> IO (Either AppError (a, AppState))
runApp config state app = 
    runExceptT (runStateT (runReaderT app config) state)

-- Algebraic Effects
type AppEffects = '[Reader Config, State AppState, Error AppError, Embed IO]

runApp' :: Config -> AppState -> Sem AppEffects a -> IO (Either AppError (a, AppState))
runApp' config state app =
    runM $ runError $ runState state $ runReader config $ app
```

## Advanced Patterns

### Effect Scoping

```haskell
-- Local effect scopes
localState :: Member (State s) r => s -> Sem r a -> Sem r a
localState initialState action = do
    oldState <- get
    put initialState
    result <- action
    put oldState
    return result

-- Example usage
computation :: Member (State Int) r => Sem r Int
computation = do
    put 10
    result <- localState 0 $ do
        modify (+5)
        get  -- Returns 5
    get  -- Returns 10 (state restored)
```

### Effect Optimization

```haskell
-- Batch operations
data Batch m a where
    BatchRead :: [Key] -> Batch m [Maybe Value]
    BatchWrite :: [(Key, Value)] -> Batch m ()

-- Optimize multiple single operations into batch
optimizeBatch :: Sem (Cache ': r) a -> Sem (Batch ': r) a
optimizeBatch = undefined  -- Implementation would analyze and batch operations
```

### Parallel Effects

```haskell
import Polysemy.Async

-- Run effects in parallel
parallelFetch :: (Member Async r, Member Http r) => [URL] -> Sem r [Response]
parallelFetch urls = do
    asyncs <- traverse (\url -> async (httpGet url)) urls
    traverse await asyncs
```

## Testing with Effects

```haskell
-- Test with pure effects
testBackup :: IO ()
testBackup = do
    let (logs, (fileStore, _)) = run 
            $ runLogAsList 
            $ runFileSystemPure 
            $ backup "source.txt" "dest.txt"
    
    -- Assert on pure results
    assert (Map.member "dest.txt" fileStore)
    assert ("Backup complete" `elem` logs)

-- Property-based testing
prop_idempotent :: Property
prop_idempotent = property $ do
    value <- forAll arbitrary
    let result1 = run $ runState 0 $ put value >> get
    let result2 = run $ runState 0 $ put value >> put value >> get
    result1 === result2
```

## Related Patterns

- **[Monad](../basic/monad)** - Effects are monadic
- **[Monad Transformer](../basic/monad-transformer)** - Alternative approach
- **[Free Monad](free-monad)** - Can represent effects
- **[Tagless Final](tagless-final)** - Different encoding

## Advantages

1. **Modularity**: Effects are independent
2. **Testability**: Easy to mock effects
3. **Performance**: Can optimize handlers
4. **Composability**: Freely combine effects
5. **Order Independence**: Handler order more flexible than transformers

## Disadvantages

1. **Complexity**: Requires advanced type system features
2. **Learning Curve**: More abstract than transformers
3. **Ecosystem**: Less mature than monad transformers
4. **Error Messages**: Can be cryptic
5. **Performance**: Some overhead (though optimized)

## Effect Libraries

### Polysemy

```haskell
-- Modern, type-safe, performant
import Polysemy
import Polysemy.State
import Polysemy.Error
```

### Eff (Extensible Effects)

```haskell
-- Original implementation
import Control.Monad.Freer
import Control.Monad.Freer.State
```

### Fused-Effects

```haskell
-- High-performance implementation
import Control.Carrier.State.Strict
import Control.Carrier.Error.Either
```

## Best Practices

1. **Start small**: Begin with simple effects
2. **Use existing effects**: Don't reinvent common effects
3. **Test with pure handlers**: Makes testing easy
4. **Document effect interactions**: Some effects don't commute
5. **Consider performance**: Profile handler implementations
6. **Provide multiple handlers**: For testing and production

## Further Reading

- [Polysemy Documentation](https://hackage.haskell.org/package/polysemy)
- [Algebraic Effects for Functional Programming](https://www.microsoft.com/en-us/research/publication/algebraic-effects-for-functional-programming/)
- [Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)
- [An Introduction to Algebraic Effects and Handlers](https://www.eff-lang.org/handlers-tutorial.pdf)
