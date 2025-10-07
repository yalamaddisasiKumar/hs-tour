---
title: Tagless Final
---

## Overview

**Tagless Final** (also known as Finally Tagless) is a technique for building deeply embedded Domain-Specific Languages (DSLs) in a type-safe, extensible way. It uses typeclasses to define operations and lets you provide multiple interpretations without modifying the core DSL definition.

## Core Concepts

- **Embedded DSL**: Express domain logic in Haskell
- **Multiple Interpretations**: Different meanings for the same code
- **Type Safety**: Leverage Haskell's type system
- **Extensibility**: Add operations and interpretations independently

## Type Signature

```haskell
-- Define DSL operations as a typeclass
class Language repr where
    lit :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    mul :: repr Int -> repr Int -> repr Int

-- Multiple interpretations
newtype Eval a = Eval { runEval :: a }
newtype Pretty a = Pretty { runPretty :: String }
```

## Examples

### Basic Expression Language

```haskell
-- DSL definition
class ExprSym repr where
    lit :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    mul :: repr Int -> repr Int -> repr Int

-- Evaluation interpretation
newtype Eval a = Eval { runEval :: a }

instance ExprSym Eval where
    lit n = Eval n
    add (Eval x) (Eval y) = Eval (x + y)
    mul (Eval x) (Eval y) = Eval (x * y)

-- Pretty-printing interpretation
newtype Pretty a = Pretty { runPretty :: String }

instance ExprSym Pretty where
    lit n = Pretty (show n)
    add (Pretty x) (Pretty y) = Pretty ("(" ++ x ++ " + " ++ y ++ ")")
    mul (Pretty x) (Pretty y) = Pretty ("(" ++ x ++ " * " ++ y ++ ")")

-- Using the DSL
expr :: ExprSym repr => repr Int
expr = add (lit 3) (mul (lit 4) (lit 5))

-- Different interpretations
result1 = runEval expr      -- 23
result2 = runPretty expr    -- "(3 + (4 * 5))"
```

### Lambda Calculus DSL

```haskell
-- Extended language with functions
class LambdaSym repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> repr a -> repr b

-- Evaluation
newtype R a = R { unR :: a }

instance LambdaSym R where
    int = R
    add (R x) (R y) = R (x + y)
    lam f = R (unR . f . R)
    app (R f) (R x) = R (f x)

-- Example program
program :: LambdaSym repr => repr Int
program = app (lam (\x -> add x (int 10))) (int 5)

result = unR program  -- 15
```

### Database Query DSL

```haskell
-- Query DSL
class QueryLang repr where
    table :: String -> repr Table
    select :: [String] -> repr Table -> repr Query
    where_ :: repr Predicate -> repr Query -> repr Query
    equals :: String -> String -> repr Predicate
    
-- SQL generation
newtype SQL a = SQL { toSQL :: String }

instance QueryLang SQL where
    table name = SQL name
    select cols (SQL t) = SQL $ 
        "SELECT " ++ intercalate ", " cols ++ " FROM " ++ t
    where_ (SQL pred) (SQL query) = SQL $ 
        query ++ " WHERE " ++ pred
    equals field val = SQL $ field ++ " = '" ++ val ++ "'"

-- Type-safe query construction
userQuery :: QueryLang repr => repr Query
userQuery = where_ 
    (equals "age" "25") 
    (select ["name", "email"] (table "users"))

sql = toSQL userQuery
-- "SELECT name, email FROM users WHERE age = '25'"
```

### Web Framework DSL

```haskell
-- Routing DSL
class WebDSL repr where
    get :: String -> repr Handler -> repr Route
    post :: String -> repr Handler -> repr Route
    respond :: String -> repr Response
    combineRoutes :: repr Route -> repr Route -> repr Route

-- Handler type
data Handler = Handler (Request -> IO Response)
data Route = Route [(Method, String, Handler)]

-- Server interpretation
newtype Server a = Server { runServer :: a }

instance WebDSL Server where
    get path (Server handler) = Server $ Route [(GET, path, handler)]
    post path (Server handler) = Server $ Route [(POST, path, handler)]
    respond content = Server $ Response 200 content
    combineRoutes (Server (Route r1)) (Server (Route r2)) = 
        Server $ Route (r1 ++ r2)

-- Example application
app :: WebDSL repr => repr Route
app = combineRoutes
    (get "/users" (respond "User list"))
    (post "/users" (respond "User created"))
```

## Advanced Patterns

### Optimization Interpretation

```haskell
-- Constant folding interpretation
newtype Optimize a = Optimize { runOpt :: Maybe a }

instance ExprSym Optimize where
    lit n = Optimize (Just n)
    add (Optimize (Just x)) (Optimize (Just y)) = Optimize (Just (x + y))
    add _ _ = Optimize Nothing
    mul (Optimize (Just x)) (Optimize (Just y)) = Optimize (Just (x * y))
    mul _ _ = Optimize Nothing

-- Optimized result
optimized = runOpt expr  -- Just 23 (computed at compile time)
```

### Type-Level Computation

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- Type-indexed expressions
class TypedExpr repr where
    litInt :: Int -> repr Int
    litBool :: Bool -> repr Bool
    add :: repr Int -> repr Int -> repr Int
    if_ :: repr Bool -> repr a -> repr a -> repr a

-- Typed evaluation
newtype Typed a = Typed { runTyped :: a }

instance TypedExpr Typed where
    litInt = Typed
    litBool = Typed
    add (Typed x) (Typed y) = Typed (x + y)
    if_ (Typed b) (Typed t) (Typed f) = Typed (if b then t else f)

-- Type-safe expression
safeProg :: TypedExpr repr => repr Int
safeProg = if_ (litBool True) (litInt 10) (litInt 20)
```

### Effect Tracking

```haskell
-- Track effects in types
class EffectfulLang repr where
    pure_ :: a -> repr '[] a
    readFile_ :: FilePath -> repr '[IO] String
    writeFile_ :: FilePath -> String -> repr '[IO] ()
    bind :: repr effs1 a -> (a -> repr effs2 b) -> repr (Union effs1 effs2) b

-- Interpretation that enforces effect tracking
newtype Eff effs a = Eff { runEff :: IO a }
```

### Staged Compilation

```haskell
-- Multi-stage programming
class StagedLang repr where
    lift0 :: a -> repr a
    lift1 :: (a -> b) -> repr a -> repr b
    
-- Code generation
newtype CodeGen a = CodeGen { genCode :: String }

instance StagedLang CodeGen where
    lift0 x = CodeGen (show x)
    lift1 f (CodeGen x) = CodeGen ("(" ++ show f ++ " " ++ x ++ ")")
```

## Common Use Cases

### 1. Compilers and Interpreters

```haskell
-- Source language
class Source repr where
    var :: String -> repr Var
    assign :: repr Var -> repr Expr -> repr Stmt
    while :: repr Expr -> repr Stmt -> repr Stmt

-- Multiple backends
newtype ToJavaScript a = ToJavaScript { toJS :: String }
newtype ToLLVM a = ToLLVM { toLLVM :: String }
```

### 2. Configuration DSLs

```haskell
class ConfigDSL repr where
    string :: String -> repr String
    int :: Int -> repr Int
    list :: [repr a] -> repr [a]
    object :: [(String, repr a)] -> repr (Map String a)

-- JSON serialization
newtype ToJSON a = ToJSON { toJSON :: Value }

-- Validation
newtype Validate a = Validate { validate :: Either Error a }
```

### 3. Test Specifications

```haskell
class TestDSL repr where
    describe :: String -> repr TestSuite -> repr TestSuite
    it :: String -> repr Assertion -> repr TestSuite
    expect :: repr a -> repr Matcher a -> repr Assertion

-- Run tests
newtype TestRunner a = TestRunner { runTests :: IO TestResult }

-- Generate documentation
newtype TestDocs a = TestDocs { genDocs :: String }
```

## Advantages

1. **Type Safety**: Errors caught at compile time
2. **Extensibility**: Add new operations without changing interpretations
3. **Multiple Interpretations**: Same code, different meanings
4. **Modularity**: Separate concerns cleanly
5. **Optimization**: Interpret at compile-time or runtime

## Disadvantages

1. **Learning Curve**: Requires understanding of advanced type system features
2. **Type Complexity**: Type signatures can become complex
3. **Error Messages**: Can be difficult to understand
4. **Performance**: May have overhead compared to direct implementation

## Comparison with Free Monads

| Aspect | Tagless Final | Free Monad |
|--------|---------------|------------|
| Definition | Typeclass-based | Data type-based |
| Extensibility | Easy to add operations | Easy to add interpretations |
| Type safety | High | High |
| Performance | Better | More overhead |
| Introspection | Limited | Easy |

## Related Patterns

- **[Typeclasses](../basic/typeclasses)** - Foundation of tagless final
- **[Free Monad](free-monad)** - Alternative approach
- **[GADTs](gadts)** - Can be combined with tagless final
- **[Type-Level Programming](type-level-programming)** - Advanced usage

## Best Practices

1. **Start simple**: Begin with basic DSL, add complexity gradually
2. **Use type families**: For associated types in interpretations
3. **Provide multiple interpretations**: Show flexibility of approach
4. **Document laws**: Specify expected behavior
5. **Consider performance**: Use `INLINE` pragmas where appropriate
6. **Test interpretations**: Each interpretation needs tests

## Further Reading

- [Typed Tagless Final Interpreters](http://okmij.org/ftp/tagless-final/index.html) - Oleg Kiselyov
- [Tagless Final Encoding in Haskell](https://www.parsonsmatt.org/2017/05/18/tagless_final_encoding.html)
- [Finally Tagless, Partially Evaluated](http://okmij.org/ftp/tagless-final/course/lecture.pdf)
- [Typed Final (Tagless-Final) Style](https://wiki.haskell.org/Typed_tagless-final_style)
