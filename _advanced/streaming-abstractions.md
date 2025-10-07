---
title: Streaming Abstractions (Iteratee / Conduit / Pipes)
---

## Overview

**Streaming Abstractions** provide efficient, composable ways to process potentially infinite streams of data with constant memory usage. They solve problems like handling large files, network streams, and real-time data processing without loading everything into memory.

## Core Concepts

- **Constant Memory**: Process unbounded streams with bounded memory
- **Composability**: Build complex pipelines from simple components
- **Resource Safety**: Automatic resource management
- **Backpressure**: Consumer-driven processing

## The Three Main Libraries

### Pipes

Most theoretically pure, based on category theory.

### Conduit

Most practical, widely used in production.

### Iteratee

Original approach, more complex but powerful.

## Pipes Examples

### Basic Pipes

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Pipes
import qualified Pipes.Prelude as P

-- Producer: generates values
producer :: Producer Int IO ()
producer = each [1..10]

-- Consumer: consumes values
consumer :: Consumer Int IO ()
consumer = do
    x <- await
    lift $ print x
    consumer

-- Pipe: transforms values
doubler :: Pipe Int Int IO ()
doubler = do
    x <- await
    yield (x * 2)
    doubler

-- Running a pipeline
example1 :: IO ()
example1 = runEffect $ producer >-> doubler >-> P.print

-- Result: prints 2, 4, 6, ..., 20
```

### File Processing with Pipes

```haskell
import Pipes
import Pipes.Safe
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import System.IO

-- Read file line by line
readLines :: FilePath -> Producer ByteString (SafeT IO) ()
readLines file = do
    h <- liftIO $ openFile file ReadMode
    PB.fromHandle h
    liftIO $ hClose h

-- Word counter
wordCount :: Pipe ByteString Int IO ()
wordCount = do
    line <- await
    let count = length (words (decodeUtf8 line))
    yield count
    wordCount

-- Sum all values
sumPipe :: Consumer Int IO Int
sumPipe = P.fold (+) 0 id

-- Complete pipeline
countWordsInFile :: FilePath -> IO Int
countWordsInFile file = runSafeT $ runEffect $
    readLines file >-> wordCount >-> sumPipe
```

### Infinite Streams

```haskell
import Pipes
import qualified Pipes.Prelude as P

-- Infinite producer
naturals :: Producer Int IO ()
naturals = each [1..]

-- Take finite number from infinite stream
takeN :: Int -> Pipe a a IO ()
takeN 0 = return ()
takeN n = do
    x <- await
    yield x
    takeN (n - 1)

-- Use infinite stream safely
example2 :: IO ()
example2 = runEffect $
    naturals >-> takeN 10 >-> P.print
```

### Concurrent Processing

```haskell
import Pipes
import Pipes.Concurrent

-- Producer-consumer with bounded channel
example3 :: IO ()
example3 = do
    (output, input) <- spawn (bounded 100)
    
    -- Producer in separate thread
    _ <- forkIO $ do
        runEffect $ each [1..1000] >-> toOutput output
        atomically $ send output Nothing
    
    -- Consumer
    runEffect $ fromInput input >-> P.print
```

## Conduit Examples

### Basic Conduits

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Conduit

-- Source: produces values
source :: ConduitT () Int IO ()
source = yieldMany [1..10]

-- Conduit: transforms values
doubler :: ConduitT Int Int IO ()
doubler = awaitForever $ \x -> yield (x * 2)

-- Sink: consumes values
printer :: ConduitT Int Void IO ()
printer = mapM_C print

-- Running a pipeline
example1 :: IO ()
example1 = runConduit $ source .| doubler .| printer

-- Result: prints 2, 4, 6, ..., 20
```

### File Processing with Conduit

```haskell
import Conduit
import Data.Conduit.Binary (sourceFile, sinkFile)
import qualified Data.Conduit.List as CL

-- Process large file line by line
processFile :: FilePath -> FilePath -> IO ()
processFile input output = runConduitRes $
    sourceFile input
    .| linesUnboundedAsciiC
    .| mapC processLine
    .| unlinesAsciiC
    .| sinkFile output
  where
    processLine :: ByteString -> ByteString
    processLine = map toUpper

-- Chunked processing
chunkSum :: Int -> Conduit Int IO Int
chunkSum n = loop
  where
    loop = do
        chunk <- takeC n .| sinkList
        unless (null chunk) $ do
            yield (sum chunk)
            loop

-- Use chunked processing
example2 :: IO ()
example2 = runConduit $
    yieldMany [1..100]
    .| chunkSum 10
    .| mapM_C print
-- Prints: 55, 155, 255, ..., 955
```

### Resource Management

```haskell
import Conduit
import Data.Conduit.Binary

-- Automatic resource management
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest = runConduitRes $
    sourceFile src .| sinkFile dest

-- Multiple resources
mergeFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeFiles file1 file2 output = runConduitRes $ do
    let source1 = sourceFile file1
    let source2 = sourceFile file2
    source1
        .| interleave (sourceFile file2)
        .| sinkFile output
  where
    interleave :: ConduitT () a IO () -> ConduitT a a IO ()
    interleave other = undefined  -- Implementation details
```

### Combining Multiple Sources

```haskell
import Conduit

-- Zip two sources
zipSources :: IO ()
zipSources = runConduit $
    getZipSource (
        (,) <$> ZipSource (yieldMany [1..10])
            <*> ZipSource (yieldMany ['a'..'j'])
    )
    .| mapM_C print

-- Merge sources
mergeSources :: IO ()
mergeSources = runConduit $
    (yieldMany [1,3,5] >> yieldMany [2,4,6])
    .| mapM_C print
```

### Stateful Processing

```haskell
import Conduit
import Control.Monad.State.Strict

-- Running average
runningAverage :: ConduitT Double Double (State (Double, Int)) ()
runningAverage = awaitForever $ \x -> do
    (sum, count) <- lift get
    let newSum = sum + x
    let newCount = count + 1
    let avg = newSum / fromIntegral newCount
    lift $ put (newSum, newCount)
    yield avg

-- Use stateful conduit
example3 :: IO [Double]
example3 = return $ evalState
    (runConduit $ yieldMany [1,2,3,4,5] .| runningAverage .| sinkList)
    (0, 0)
-- Result: [1.0, 1.5, 2.0, 2.5, 3.0]
```

## Streaming Data Library Examples

### Basic Streaming

```haskell
{-# LANGUAGE RankNTypes #-}

import Streaming
import qualified Streaming.Prelude as S

-- Stream of values
stream :: Stream (Of Int) IO ()
stream = S.each [1..10]

-- Transform stream
doubled :: Stream (Of Int) IO ()
doubled = S.map (*2) stream

-- Fold stream
example1 :: IO Int
example1 = S.sum_ $ S.each [1..100]
-- Result: 5050
```

### Streaming File I/O

```haskell
import Streaming
import Streaming.ByteString as Q
import qualified Streaming.Prelude as S

-- Read file as stream of lines
readLines :: FilePath -> IO ()
readLines file = Q.withFile file ReadMode $ \h ->
    Q.hGetContents h
        & Q.lines
        & S.mapM_ (putStrLn . show)
```

### Efficient Grouping

```haskell
import Streaming
import qualified Streaming.Prelude as S

-- Group consecutive equal elements
groups :: IO ()
groups = S.print $ S.mapped S.toList $ S.group $
    S.each [1,1,2,2,2,3,3,1,1]
-- Result: [1,1], [2,2,2], [3,3], [1,1]
```

## Comparison of Libraries

### Pipes

**Pros:**
- Mathematically principled
- Clean abstractions
- Good documentation
- Excellent for understanding streaming

**Cons:**
- Less ecosystem support
- Can be verbose

**Best for:** Learning streaming concepts, pure functional code

### Conduit

**Pros:**
- Large ecosystem
- Production-ready
- Excellent resource management
- Wide adoption

**Cons:**
- More complex internals
- Less mathematically pure

**Best for:** Production applications, file processing, web servers

### Streaming

**Pros:**
- Simple and fast
- Minimal dependencies
- Good performance

**Cons:**
- Smaller ecosystem
- Less community support

**Best for:** When simplicity and performance matter

## Common Patterns

### Chunking

```haskell
-- Pipes
chunkedPipe :: Int -> Pipe a [a] IO ()
chunkedPipe n = go []
  where
    go acc = do
        if length acc == n
            then do
                yield acc
                go []
            else do
                mx <- await
                case mx of
                    Just x -> go (acc ++ [x])
                    Nothing -> when (not $ null acc) (yield acc)

-- Conduit
chunkedConduit :: Int -> ConduitT a [a] IO ()
chunkedConduit n = loop []
  where
    loop acc = do
        mx <- await
        case mx of
            Nothing -> unless (null acc) (yield acc)
            Just x ->
                let acc' = acc ++ [x]
                in if length acc' == n
                   then yield acc' >> loop []
                   else loop acc'
```

### Error Handling

```haskell
-- Pipes with exceptions
safePipe :: Pipe Int Int IO ()
safePipe = do
    x <- await
    result <- lift $ try (expensiveComputation x)
    case result of
        Left (e :: SomeException) -> lift $ putStrLn $ "Error: " ++ show e
        Right y -> yield y
    safePipe

-- Conduit with error handling
safeConduit :: ConduitT Int Int IO ()
safeConduit = awaitForever $ \x -> do
    result <- lift $ try (expensiveComputation x)
    case result of
        Left (e :: SomeException) -> return ()
        Right y -> yield y
```

### Buffering

```haskell
-- Conduit buffering
buffered :: Int -> ConduitT a a IO ()
buffered size = loop []
  where
    loop buf = do
        mx <- await
        case mx of
            Nothing -> mapM_ yield buf
            Just x ->
                let buf' = buf ++ [x]
                in if length buf' >= size
                   then mapM_ yield buf' >> loop []
                   else loop buf'
```

## Real-World Examples

### Log File Analyzer

```haskell
import Conduit
import Data.Conduit.Binary

analyzeLog :: FilePath -> IO ()
analyzeLog file = runConduitRes $
    sourceFile file
    .| linesUnboundedAsciiC
    .| filterC isError
    .| mapC parseError
    .| foldlC accumErrors Map.empty
    .| mapM_C printReport
  where
    isError line = "ERROR" `isPrefixOf` line
    parseError = undefined  -- Parse error details
    accumErrors = undefined  -- Accumulate statistics
    printReport = undefined  -- Format report
```

### CSV Processing

```haskell
import Conduit
import Data.CSV.Conduit

processCSV :: FilePath -> FilePath -> IO ()
processCSV input output = runConduitRes $
    sourceFile input
    .| intoCSV defCSVSettings
    .| mapC processRow
    .| fromCSV defCSVSettings
    .| sinkFile output
  where
    processRow :: [ByteString] -> [ByteString]
    processRow = map processField
```

### Network Streaming

```haskell
import Conduit
import Network.HTTP.Simple

streamURL :: String -> ConduitT () ByteString IO ()
streamURL url = do
    request <- parseRequest url
    response <- httpSink request $ \_ -> sinkList
    mapM_ yield response

-- Download and process
downloadAndProcess :: String -> IO ()
downloadAndProcess url = runConduitRes $
    streamURL url
    .| processData
    .| sinkFile "output.dat"
```

## Related Patterns

- **[Monad](../basic/monad)** - Streaming uses monadic composition
- **[Arrow](../basic/arrow)** - Alternative composition approach
- **[Comonad](../basic/comonad)** - Dual concept

## Advantages

1. **Memory Efficiency**: Constant memory usage
2. **Composability**: Build complex pipelines easily
3. **Resource Safety**: Automatic cleanup
4. **Performance**: Efficient processing
5. **Flexibility**: Handle various data sources

## Disadvantages

1. **Learning Curve**: New abstractions to learn
2. **Debugging**: Stack traces can be confusing
3. **Performance Overhead**: Some abstraction cost
4. **Complexity**: Can be overkill for simple tasks

## Best Practices

1. **Choose the right library**: Match library to your needs
2. **Use resource-safe combinators**: Leverage `runResourceT`
3. **Chunk appropriately**: Balance memory and performance
4. **Handle errors gracefully**: Use exception-safe combinators
5. **Test with realistic data**: Verify memory behavior
6. **Profile**: Measure actual performance

## Further Reading

- [Pipes Tutorial](https://hackage.haskell.org/package/pipes/docs/Pipes-Tutorial.html)
- [Conduit Tutorial](https://github.com/snoyberg/conduit#readme)
- [Streaming Library](https://hackage.haskell.org/package/streaming)
- [Stream Fusion](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.104.7401)
- [Comparing Streaming Libraries](https://www.schoolofhaskell.com/user/commercial/content/pipes-vs-conduit)
