---
title: Monad Transformer
---

## Overview

**Monad Transformers** allow you to combine multiple monads into a single monad that provides the effects of all constituent monads. They solve the problem of working with multiple effects simultaneously by stacking monads on top of each other.

## Core Concepts

- **Stacking Effects**: Combine multiple computational effects
- **Lifting**: Move operations between monad layers
- **Composability**: Build complex effects from simpler ones

## Type Signature

```haskell
class MonadTrans t where
    lift :: Monad m => m a -> t m a

-- Common transformer stack
type App = ReaderT Config (StateT AppState (ExceptT Error IO))
```

## Common Transformers

### MaybeT - Optional Values

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    (MaybeT m) >>= f = MaybeT $ do
        maybeVal <- m
        case maybeVal of
            Nothing -> return Nothing
            Just val -> runMaybeT (f val)

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just
```

**Example:**

```haskell
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

-- Database lookup that might fail
lookupUser :: Int -> MaybeT IO User
lookupUser userId = do
    lift $ putStrLn $ "Looking up user " ++ show userId
    -- Simulate database lookup
    if userId > 0
        then return (User userId "John")
        else MaybeT $ return Nothing

getUser :: Int -> IO (Maybe User)
getUser = runMaybeT . lookupUser
```

### ExceptT - Error Handling

```haskell
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Monad m => Monad (ExceptT e m) where
    return = ExceptT . return . Right
    (ExceptT m) >>= f = ExceptT $ do
        result <- m
        case result of
            Left e -> return (Left e)
            Right a -> runExceptT (f a)
```

**Example:**

```haskell
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

data AppError = NotFound | InvalidInput | DatabaseError
    deriving (Show, Eq)

validateInput :: String -> ExceptT AppError IO Int
validateInput s = 
    case reads s of
        [(n, "")] | n > 0 -> return n
        _ -> throwE InvalidInput

processRequest :: String -> ExceptT AppError IO String
processRequest input = do
    lift $ putStrLn "Processing request..."
    userId <- validateInput input
    user <- findUser userId
    return $ "Found: " ++ show user

findUser :: Int -> ExceptT AppError IO User
findUser userId = 
    if userId <= 100
        then return (User userId "Alice")
        else throwE NotFound
```

### ReaderT - Configuration/Environment

```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a
    (ReaderT m) >>= f = ReaderT $ \r -> do
        a <- m r
        runReaderT (f a) r
```

**Example:**

```haskell
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

data Config = Config
    { dbConnection :: String
    , apiKey :: String
    , maxRetries :: Int
    } deriving (Show)

getConnection :: ReaderT Config IO String
getConnection = do
    config <- ask
    lift $ putStrLn "Getting connection..."
    return $ dbConnection config

performQuery :: String -> ReaderT Config IO [Result]
performQuery query = do
    conn <- getConnection
    retries <- asks maxRetries
    lift $ putStrLn $ "Running query with " ++ show retries ++ " retries"
    return []

runApp :: ReaderT Config IO a -> Config -> IO a
runApp = runReaderT
```

### StateT - Stateful Computations

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    (StateT m) >>= f = StateT $ \s -> do
        (a, s') <- m s
        runStateT (f a) s'
```

**Example:**

```haskell
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

type Counter = StateT Int IO

increment :: Counter ()
increment = modify (+1)

getCount :: Counter Int
getCount = get

logCount :: Counter ()
logCount = do
    count <- get
    lift $ putStrLn $ "Current count: " ++ show count

program :: Counter ()
program = do
    increment
    increment
    logCount
    increment
    logCount

runCounter :: Counter a -> Int -> IO (a, Int)
runCounter = runStateT
```

### WriterT - Logging/Accumulation

```haskell
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    (WriterT m) >>= f = WriterT $ do
        (a, w) <- m
        (b, w') <- runWriterT (f a)
        return (b, w `mappend` w')
```

**Example:**

```haskell
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Monoid (Sum(..))

type Logged = WriterT [String] IO

logMsg :: String -> Logged ()
logMsg msg = tell [msg]

computation :: Logged Int
computation = do
    logMsg "Starting computation"
    lift $ putStrLn "Performing IO"
    logMsg "Step 1 complete"
    let result = 42
    logMsg "Step 2 complete"
    return result

runLogged :: Logged a -> IO (a, [String])
runLogged = runWriterT
```

## Stacking Transformers

```haskell
-- Combining multiple effects
type App = ReaderT Config (ExceptT AppError (StateT AppState IO))

data Config = Config { configDb :: String }
data AppError = NetworkError | ParseError
data AppState = AppState { stateCounter :: Int }

-- Using the stack
runApp :: Config -> AppState -> App a -> IO (Either AppError (a, AppState))
runApp config state app = 
    runStateT (runExceptT (runReaderT app config)) state

-- Example usage
getDbConnection :: App String
getDbConnection = do
    config <- ask  -- From ReaderT
    count <- get   -- From StateT
    lift $ lift $ lift $ putStrLn "Connecting..."  -- Nested lift for IO
    return $ configDb config

-- Better with mtl
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

getDbConnection' :: App String
getDbConnection' = do
    config <- ask
    count <- get
    liftIO $ putStrLn "Connecting..."
    return $ configDb config
```

## MTL Style (Type Classes)

```haskell
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- Generic constraints instead of concrete transformers
fetchUser :: (MonadReader Config m, MonadIO m, MonadError AppError m) 
          => Int -> m User
fetchUser userId = do
    config <- ask
    liftIO $ putStrLn "Fetching user..."
    if userId > 0
        then return (User userId "Alice")
        else throwError NotFound

updateCounter :: (MonadState AppState m) => m ()
updateCounter = modify $ \s -> s { stateCounter = stateCounter s + 1 }

-- Works with any compatible monad stack
processUser :: (MonadReader Config m, MonadState AppState m, 
                MonadError AppError m, MonadIO m) 
            => Int -> m User
processUser userId = do
    updateCounter
    user <- fetchUser userId
    liftIO $ putStrLn $ "Processed user: " ++ show user
    return user
```

## Common Patterns

### 1. Web Application Stack

```haskell
type WebApp = ReaderT AppConfig (ExceptT AppError (LoggingT IO))

data AppConfig = AppConfig
    { httpPort :: Int
    , dbPool :: ConnectionPool
    }

data AppError = RouteNotFound | Unauthorized | ServerError String

handleRequest :: Request -> WebApp Response
handleRequest req = do
    config <- ask
    logInfo $ "Handling request: " ++ show req
    case routeRequest req of
        Nothing -> throwError RouteNotFound
        Just handler -> handler req
```

### 2. Parser with State and Errors

```haskell
type Parser = StateT String (Either ParseError)

data ParseError = UnexpectedChar Char | EndOfInput

parseChar :: Char -> Parser Char
parseChar expected = do
    input <- get
    case input of
        [] -> lift $ Left EndOfInput
        (c:cs) | c == expected -> do
            put cs
            return c
        (c:_) -> lift $ Left (UnexpectedChar c)

parseString :: String -> Parser String
parseString = mapM parseChar
```

### 3. Game State with Logging

```haskell
type Game = StateT GameState (WriterT [String] IO)

data GameState = GameState
    { player :: Player
    , enemies :: [Enemy]
    , score :: Int
    }

movePlayer :: Direction -> Game ()
movePlayer dir = do
    tell ["Player moved " ++ show dir]
    modify $ \s -> s { player = updatePosition dir (player s) }
    
attackEnemy :: Enemy -> Game ()
attackEnemy enemy = do
    state <- get
    tell ["Player attacked enemy!"]
    let newScore = score state + 10
    put $ state { score = newScore }
```

## Lifting Operations

```haskell
-- Manual lifting
doSomething :: ReaderT Config (StateT AppState IO) ()
doSomething = do
    config <- ask                          -- ReaderT operation
    state <- lift get                      -- StateT operation
    lift $ lift $ putStrLn "Hello!"        -- IO operation

-- Using MonadIO
doSomething' :: (MonadReader Config m, MonadState AppState m, MonadIO m) => m ()
doSomething' = do
    config <- ask
    state <- get
    liftIO $ putStrLn "Hello!"
```

## Related Patterns

- **[Monad](monad)** - Base abstraction
- **[Typeclasses](typeclasses)** - MTL uses typeclasses
- **[Free Monad](../advanced/free-monad)** - Alternative to transformers
- **[Algebraic Effects](../advanced/algebraic-effects)** - Modern alternative

## Best Practices

1. **Order matters**: Place transformers carefully (e.g., `ExceptT` early to short-circuit)
2. **Use MTL style** for flexibility and cleaner type signatures
3. **Avoid deep stacks**: More than 3-4 transformers becomes unwieldy
4. **Consider alternatives**: Free monads or effect systems for complex cases
5. **Provide type aliases**: Make complex stacks easier to work with
6. **Use `liftIO`**: Instead of multiple `lift` calls

## Performance Considerations

- Transformers add overhead at each layer
- Consider using specialized monads (e.g., `RWS` instead of `ReaderT + WriterT + StateT`)
- For performance-critical code, consider direct implementations

## Further Reading

- [Monad Transformers Step by Step](https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf)
- [Monad Transformers - HaskellWiki](https://wiki.haskell.org/Monad_Transformers)
- [MTL Library Documentation](https://hackage.haskell.org/package/mtl)
