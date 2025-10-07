---
title: HKD (Higher-Kinded Data)
---

## Overview

**Higher-Kinded Data** (HKD) is a pattern where record fields are parameterized by a type constructor, enabling generic transformations over record structures. It's powerful for handling partial data, validation, forms, and configuration.

## Core Concepts

- **Type Constructor Parameter**: Records parameterized by `f :: * -> *`
- **Multiple Interpretations**: Same structure, different meanings
- **Generic Programming**: Transform entire records uniformly
- **Partial vs Complete**: Represent optional vs required fields

## Type Signature

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- HKD record
data Person f = Person
    { name :: f String
    , age :: f Int
    , email :: f String
    }

-- Different interpretations
type CompletePerson = Person Identity
type PartialPerson = Person Maybe
type ValidatedPerson = Person (Either ValidationError)
```

## Examples

### Basic HKD Pattern

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Functor.Identity
import GHC.Generics

-- HKD Person
data Person f = Person
    { name :: HKD f String
    , age :: HKD f Int
    , email :: HKD f String
    , address :: HKD f Address
    } deriving (Generic)

data Address f = Address
    { street :: HKD f String
    , city :: HKD f String
    , zipCode :: HKD f String
    } deriving (Generic)

-- Type family for interpretation
type family HKD f a where
    HKD Identity a = a
    HKD f a = f a

-- Complete (required) version
type CompletePerson = Person Identity

completePerson :: CompletePerson
completePerson = Person
    { name = "Alice"
    , age = 30
    , email = "alice@example.com"
    , address = Address "123 Main St" "Springfield" "12345"
    }

-- Partial (optional) version
type PartialPerson = Person Maybe

partialPerson :: PartialPerson
partialPerson = Person
    { name = Just "Bob"
    , age = Nothing
    , email = Just "bob@example.com"
    , address = Nothing
    }
```

### Form Handling

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

data FormField a
    = Empty
    | Invalid String a
    | Valid a
    deriving (Show, Eq)

type FormPerson = Person FormField

-- Empty form
emptyForm :: FormPerson
emptyForm = Person
    { name = Empty
    , age = Empty
    , email = Empty
    , address = Address Empty Empty Empty
    }

-- Partially filled form
filledForm :: FormPerson
filledForm = Person
    { name = Valid "Charlie"
    , age = Invalid "abc" 0  -- Invalid input
    , email = Valid "charlie@example.com"
    , address = Address (Valid "456 Oak St") Empty Empty
    }

-- Validate form field
validateField :: FormField a -> Either String a
validateField Empty = Left "Field is required"
validateField (Invalid err _) = Left err
validateField (Valid a) = Right a

-- Convert to validated person
validatePerson :: FormPerson -> Either [String] CompletePerson
validatePerson form = Person
    <$> validate (name form)
    <*> validate (age form)
    <*> validate (email form)
    <*> validateAddress (address form)
  where
    validate :: FormField a -> Either [String] a
    validate = either (Left . pure) Right . validateField
```

### Configuration with Defaults

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

data Config f = Config
    { serverPort :: HKD f Int
    , serverHost :: HKD f String
    , dbConnection :: HKD f String
    , maxConnections :: HKD f Int
    , logLevel :: HKD f LogLevel
    }

data LogLevel = Debug | Info | Warning | Error

-- Partial config from environment/file
type PartialConfig = Config Maybe

-- Complete config with all values
type CompleteConfig = Config Identity

-- Default values
defaultConfig :: CompleteConfig
defaultConfig = Config
    { serverPort = 8080
    , serverHost = "localhost"
    , dbConnection = "postgresql://localhost/mydb"
    , maxConnections = 100
    , logLevel = Info
    }

-- Merge partial config with defaults
mergeConfig :: PartialConfig -> CompleteConfig -> CompleteConfig
mergeConfig partial defaults = Config
    { serverPort = fromMaybe (serverPort defaults) (serverPort partial)
    , serverHost = fromMaybe (serverHost defaults) (serverHost partial)
    , dbConnection = fromMaybe (dbConnection defaults) (dbConnection partial)
    , maxConnections = fromMaybe (maxConnections defaults) (maxConnections partial)
    , logLevel = fromMaybe (logLevel defaults) (logLevel partial)
    }
```

### Generic Traversal

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Data.Functor.Compose
import Data.Functor.Identity

class GTraversable f where
    gtraverse :: Applicative m => (forall a. f a -> m (g a)) -> f x -> m (g x)

-- Traverse a partial structure
traversePartial :: Applicative m 
                => (forall a. Maybe a -> m (Maybe a)) 
                -> PartialPerson 
                -> m PartialPerson
traversePartial f person = Person
    <$> f (name person)
    <*> f (age person)
    <*> f (email person)
    <*> traverseAddress (address person)
  where
    traverseAddress addr = Address
        <$> f (street addr)
        <*> f (city addr)
        <*> f (zipCode addr)

-- Sequence effects
sequencePerson :: Applicative m => Person (Compose m f) -> m (Person f)
sequencePerson person = Person
    <$> getCompose (name person)
    <*> getCompose (age person)
    <*> getCompose (email person)
    <*> sequenceAddress (address person)
  where
    sequenceAddress addr = Address
        <$> getCompose (street addr)
        <*> getCompose (city addr)
        <*> getCompose (zipCode addr)
```

### Database Queries

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- Different representations for query building
data Field a
    = Literal a
    | Column String
    | Null
    deriving (Show)

type QueryPerson = Person Field

-- Build a query
queryExample :: QueryPerson
queryExample = Person
    { name = Column "name"
    , age = Literal 30
    , email = Column "email"
    , address = Address (Column "street") (Literal "Springfield") Null
    }

-- Generate SQL
generateSQL :: QueryPerson -> String
generateSQL person = 
    "SELECT * FROM persons WHERE " ++
    intercalate " AND " (catMaybes
        [ toCondition "name" (name person)
        , toCondition "age" (age person)
        , toCondition "email" (email person)
        ])
  where
    toCondition :: Show a => String -> Field a -> Maybe String
    toCondition col (Literal val) = Just $ col ++ " = " ++ show val
    toCondition col (Column c) = Just $ col ++ " = " ++ c
    toCondition _ Null = Nothing
```

### Diffing and Patching

```haskell
{-# LANGUAGE DataKinds #-}

data Diff a = Same | Changed a
    deriving (Show, Eq)

type PersonDiff = Person Diff

-- Compute diff between two persons
diffPerson :: CompletePerson -> CompletePerson -> PersonDiff
diffPerson old new = Person
    { name = if name old == name new then Same else Changed (name new)
    , age = if age old == age new then Same else Changed (age new)
    , email = if email old == email new then Same else Changed (email new)
    , address = diffAddress (address old) (address new)
    }
  where
    diffAddress oldAddr newAddr = Address
        { street = if street oldAddr == street newAddr then Same else Changed (street newAddr)
        , city = if city oldAddr == city newAddr then Same else Changed (city newAddr)
        , zipCode = if zipCode oldAddr == zipCode newAddr then Same else Changed (zipCode newAddr)
        }

-- Apply patch
applyPatch :: PersonDiff -> CompletePerson -> CompletePerson
applyPatch diff person = Person
    { name = case name diff of
        Same -> name person
        Changed n -> n
    , age = case age diff of
        Same -> age person
        Changed a -> a
    , email = case email diff of
        Same -> email person
        Changed e -> e
    , address = applyAddressPatch (address diff) (address person)
    }
  where
    applyAddressPatch diffAddr addr = Address
        { street = case street diffAddr of
            Same -> street addr
            Changed s -> s
        , city = case city diffAddr of
            Same -> city addr
            Changed c -> c
        , zipCode = case zipCode diffAddr of
            Same -> zipCode addr
            Changed z -> z
        }
```

### Serialization

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Aeson

-- Serializable version
data Serialized a = Serialized { unSerialized :: a }

type SerializedPerson = Person Serialized

-- Convert to JSON
toJSONPerson :: SerializedPerson -> Value
toJSONPerson person = object
    [ "name" .= unSerialized (name person)
    , "age" .= unSerialized (age person)
    , "email" .= unSerialized (email person)
    , "address" .= toJSONAddress (address person)
    ]
  where
    toJSONAddress addr = object
        [ "street" .= unSerialized (street addr)
        , "city" .= unSerialized (city addr)
        , "zipCode" .= unSerialized (zipCode addr)
        ]

-- Parse from JSON
fromJSONPerson :: Value -> Parser SerializedPerson
fromJSONPerson = withObject "Person" $ \o -> Person
    <$> (Serialized <$> o .: "name")
    <*> (Serialized <$> o .: "age")
    <*> (Serialized <$> o .: "email")
    <*> (o .: "address" >>= parseAddress)
  where
    parseAddress = withObject "Address" $ \o -> Address
        <$> (Serialized <$> o .: "street")
        <*> (Serialized <$> o .: "city")
        <*> (Serialized <$> o .: "zipCode")
```

## Advanced Patterns

### Barbies Library

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Barbies
import Data.Functor.Identity
import Data.Functor.Const

data Person f = Person
    { name :: f String
    , age :: f Int
    , email :: f String
    } deriving (Generic, FunctorB, TraversableB, ConstraintsB)

-- Automatically derive instances
instance ApplicativeB Person
instance DistributiveB Person

-- Use barbie functions
example :: Person Maybe -> Person (Const [String])
example = bmap toList
  where
    toList :: Maybe a -> Const [String] a
    toList Nothing = Const ["missing"]
    toList (Just _) = Const []
```

### Higher-Kinded Newtypes

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

newtype Validated a = Validated (Either [ValidationError] a)
    deriving (Functor, Applicative)

type ValidatedPerson = Person Validated

-- Validation logic
validateName :: String -> Validated String
validateName n
    | length n < 2 = Validated $ Left [TooShort "name"]
    | length n > 50 = Validated $ Left [TooLong "name"]
    | otherwise = Validated $ Right n

validateAge :: Int -> Validated Int
validateAge a
    | a < 0 = Validated $ Left [Negative "age"]
    | a > 150 = Validated $ Left [TooLarge "age"]
    | otherwise = Validated $ Right a

-- Validate entire person
validateCompletePerson :: CompletePerson -> Either [ValidationError] CompletePerson
validateCompletePerson person = 
    case validatedPerson of
        Person (Validated (Right n)) (Validated (Right a)) (Validated (Right e)) _ ->
            Right $ person { name = n, age = a, email = e }
        _ -> Left $ collectErrors validatedPerson
  where
    validatedPerson = Person
        (validateName $ name person)
        (validateAge $ age person)
        (validateEmail $ email person)
        (address person)  -- Simplified
```

## Use Cases

1. **Form Validation**: Partial data → validated data
2. **Configuration**: Partial config + defaults → complete config
3. **API Responses**: Handle optional fields uniformly
4. **Database**: Queries, updates, inserts with different field representations
5. **Testing**: Generate test data with different strategies
6. **Serialization**: Different representations for storage/transmission

## Related Patterns

- **[Type-Level Programming](type-level-programming)** - HKD uses type-level features
- **[GADTs](gadts)** - Can combine with HKD
- **[Lenses](lenses-prisms)** - HKD works well with lenses

## Advantages

1. **Reusability**: One definition, multiple interpretations
2. **Type Safety**: Compiler ensures correctness
3. **Generic Code**: Write once, apply everywhere
4. **Maintainability**: Changes propagate automatically
5. **Expressiveness**: Encode complex requirements in types

## Disadvantages

1. **Complexity**: Requires advanced type system features
2. **Learning Curve**: Unfamiliar pattern for many
3. **Boilerplate**: Can require significant setup
4. **Error Messages**: Type errors can be cryptic
5. **IDE Support**: May have limited tooling support

## Best Practices

1. **Use barbies library**: Reduces boilerplate significantly
2. **Derive Generic**: Enable generic programming
3. **Document interpretations**: Explain what each `f` means
4. **Provide type aliases**: Make common uses clear
5. **Start simple**: Add complexity as needed

## Libraries

- **barbies**: Core library for higher-kinded data
- **higgledy**: Generic HKD with less boilerplate
- **generic-lens**: Lens support for generic HKD
- **one-liner**: Generic programming utilities

## Further Reading

- [Barbies Library](https://hackage.haskell.org/package/barbies)
- [Higher-Kinded Data](https://reasonablypolymorphic.com/blog/higher-kinded-data/)
- [Higher-Kinded Data in Action](https://chrispenner.ca/posts/hkd-options)
- [Higgledy Documentation](https://hackage.haskell.org/package/higgledy)
