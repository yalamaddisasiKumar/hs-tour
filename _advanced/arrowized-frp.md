---
title: Arrowized FRP
---

## Overview

**Arrowized FRP** (Functional Reactive Programming) combines Arrows with FRP to create declarative, composable reactive systems. It represents time-varying values and event streams as arrows, enabling elegant modeling of interactive and reactive applications.

## Core Concepts

- **Signal Functions**: Arrows that transform signals over time
- **Events**: Discrete occurrences at specific times
- **Behaviors**: Continuously varying values
- **Composition**: Build complex behaviors from simple ones

## Type Signatures

```haskell
-- Signal function (simplified)
newtype SF a b = SF { runSF :: Signal a -> Signal b }

-- Time-varying signal
type Signal a = Time -> a

-- Event at discrete times
data Event a = NoEvent | Event a

-- Core combinators
integral :: VectorSpace a => SF a a
derivative :: VectorSpace a => SF a a
delay :: a -> SF a a
```

## Examples Using Yampa

### Basic Signal Functions

```haskell
{-# LANGUAGE Arrows #-}

import FRP.Yampa

-- Constant signal
constantSF :: SF () Double
constantSF = constant 5.0

-- Identity signal
identitySF :: SF Double Double
identitySF = arr id

-- Simple transformation
doubleSF :: SF Double Double
doubleSF = arr (*2)

-- Time-dependent signal
timeSF :: SF () Time
timeSF = time

-- Integral (accumulation over time)
integralSF :: SF Double Double
integralSF = integral
```

### Mouse Position Tracking

```haskell
{-# LANGUAGE Arrows #-}

import FRP.Yampa

data MouseInput = MouseInput
    { mousePos :: (Double, Double)
    , mouseButtons :: [Bool]
    }

data Output = Output
    { cursorPos :: (Double, Double)
    , highlightedArea :: Maybe Rect
    }

-- Track mouse position with smoothing
smoothMousePos :: SF MouseInput (Double, Double)
smoothMousePos = proc input -> do
    let (x, y) = mousePos input
    smoothX <- smooth 0.1 -< x
    smoothY <- smooth 0.1 -< y
    returnA -< (smoothX, smoothY)
  where
    smooth :: Double -> SF Double Double
    smooth factor = proc x -> do
        let target = x
        current <- integral -< (target - current) * factor
        returnA -< current
```

### Bounce Animation

```haskell
-- Bouncing ball physics
bouncingBall :: SF () Double
bouncingBall = proc () -> do
    -- Velocity with gravity
    velocity <- integral -< -9.8
    
    -- Position
    position <- integral -< velocity
    
    -- Detect collision and bounce
    rec
        let hitGround = position <= 0
        vel' <- if hitGround 
                then returnA -< -velocity * 0.8  -- Bounce with damping
                else returnA -< velocity
    
    returnA -< position

-- More complex bouncing with arrow notation
bounceSF :: Double -> SF () (Double, Double)
bounceSF initialHeight = proc () -> do
    rec
        -- Acceleration (gravity)
        let accel = if pos > 0 then -9.8 else 0
        
        -- Velocity integration
        vel <- (integral >>> arr (+ 0)) -< accel
        
        -- Position integration
        pos <- integral -< vel
        
        -- Bounce detection
        let bounce = pos <= 0 && vel < 0
        
        -- Velocity after bounce
        vel' <- if bounce
                then returnA -< -vel * 0.7  -- Damping
                else returnA -< vel
    
    returnA -< (pos, vel')
```

### Event Handling

```haskell
{-# LANGUAGE Arrows #-}

import FRP.Yampa

-- Button click events
data ButtonInput = ButtonInput
    { buttonPressed :: Bool
    , buttonPos :: (Int, Int)
    }

-- Detect button press edge
buttonPressEvent :: SF ButtonInput (Event ())
buttonPressEvent = proc input -> do
    pressed <- edge -< buttonPressed input
    returnA -< pressed

-- Count button clicks
clickCounter :: SF ButtonInput Int
clickCounter = proc input -> do
    clicks <- buttonPressEvent -< input
    count <- accumHold 0 -< (+1) <$ clicks
    returnA -< count

-- Debounce button (ignore rapid clicks)
debouncedButton :: Double -> SF ButtonInput (Event ())
debouncedButton delay = proc input -> do
    press <- buttonPressEvent -< input
    debounced <- debounce delay -< press
    returnA -< debounced
  where
    debounce :: Double -> SF (Event a) (Event a)
    debounce d = proc event -> do
        rec
            canFire <- hold True -< False <$ event
            let output = event `gate` canFire
            _ <- after d () -< output
        returnA -< output
```

### Game Character Control

```haskell
{-# LANGUAGE Arrows #-}

data Input = Input
    { keysDown :: [Key]
    , deltaTime :: Double
    }

data Character = Character
    { charPos :: (Double, Double)
    , charVel :: (Double, Double)
    , charFacing :: Direction
    } deriving (Show)

data Direction = North | South | East | West

-- Character controller
characterSF :: SF Input Character
characterSF = proc input -> do
    -- Determine desired velocity from input
    let desiredVel = inputToVelocity (keysDown input)
    
    -- Smooth velocity changes
    vel <- smoothVelocity -< desiredVel
    
    -- Integrate to get position
    pos <- integral -< vel
    
    -- Determine facing direction
    let facing = velocityToDirection vel
    
    returnA -< Character pos vel facing
  where
    inputToVelocity :: [Key] -> (Double, Double)
    inputToVelocity keys = 
        let vx = if KeyRight `elem` keys then 5.0 
                 else if KeyLeft `elem` keys then -5.0 
                 else 0.0
            vy = if KeyUp `elem` keys then 5.0 
                 else if KeyDown `elem` keys then -5.0 
                 else 0.0
        in (vx, vy)
    
    smoothVelocity :: SF (Double, Double) (Double, Double)
    smoothVelocity = proc (targetX, targetY) -> do
        vx <- smooth 0.2 -< targetX
        vy <- smooth 0.2 -< targetY
        returnA -< (vx, vy)
    
    smooth :: Double -> SF Double Double
    smooth factor = proc target -> do
        rec
            current <- iPre 0 -< next
            let next = current + (target - current) * factor
        returnA -< next
```

### Combinator-Based Animation

```haskell
-- Oscillation
oscillate :: Double -> SF () Double
oscillate freq = proc () -> do
    t <- time -< ()
    returnA -< sin (2 * pi * freq * t)

-- Fade in/out
fadeIn :: Double -> SF () Double
fadeIn duration = proc () -> do
    t <- time -< ()
    returnA -< min 1.0 (t / duration)

fadeOut :: Double -> SF () Double
fadeOut duration = proc () -> do
    alpha <- fadeIn duration -< ()
    returnA -< 1.0 - alpha

-- Sequence animations
sequenceSF :: SF () a -> SF () a -> Double -> SF () a
sequenceSF sf1 sf2 switchTime = switch sf1 $ \_ ->
    after switchTime () >>> sf2

-- Parallel composition
parallelSF :: SF () a -> SF () b -> SF () (a, b)
parallelSF sf1 sf2 = proc () -> do
    a <- sf1 -< ()
    b <- sf2 -< ()
    returnA -< (a, b)
```

### Collision Detection

```haskell
data GameObject = GameObject
    { objPos :: (Double, Double)
    , objSize :: (Double, Double)
    , objVel :: (Double, Double)
    }

-- Detect collisions between two objects
collisionSF :: SF (GameObject, GameObject) (Event ())
collisionSF = proc (obj1, obj2) -> do
    let colliding = checkCollision obj1 obj2
    collision <- edge -< colliding
    returnA -< collision
  where
    checkCollision :: GameObject -> GameObject -> Bool
    checkCollision a b =
        let (ax, ay) = objPos a
            (aw, ah) = objSize a
            (bx, by) = objPos b
            (bw, bh) = objSize b
        in ax < bx + bw && ax + aw > bx &&
           ay < by + bh && ay + ah > by

-- React to collision
withCollisionResponse :: SF GameObject GameObject
withCollisionResponse = proc obj -> do
    -- Check collision with boundaries
    let hitWall = objPos obj `hitsWall` worldBounds
    
    collision <- edge -< hitWall
    
    -- Reverse velocity on collision
    vel <- hold (objVel obj) -< negate (objVel obj) <$ collision
    
    returnA -< obj { objVel = vel }
```

### State Machines with Switch

```haskell
{-# LANGUAGE Arrows #-}

data PlayerState = Idle | Walking | Jumping | Falling

-- State machine using switch combinators
playerStateMachine :: SF Input Character
playerStateMachine = idle
  where
    idle :: SF Input Character
    idle = proc input -> do
        char <- stationaryCharacter -< input
        
        -- Transitions
        startWalk <- edge -< isWalkInput input
        startJump <- edge -< isJumpInput input
        
        -- Switch to new state on event
        switch (idle) $ \_ ->
            if startWalk /= NoEvent then walking
            else if startJump /= NoEvent then jumping
            else idle
    
    walking :: SF Input Character
    walking = proc input -> do
        char <- walkingCharacter -< input
        
        stopWalk <- edge -< not (isWalkInput input)
        startJump <- edge -< isJumpInput input
        
        switch walking $ \_ ->
            if stopWalk /= NoEvent then idle
            else if startJump /= NoEvent then jumping
            else walking
    
    jumping :: SF Input Character
    jumping = proc input -> do
        char <- jumpingCharacter -< input
        
        landed <- edge -< isOnGround char
        
        switch jumping $ \_ ->
            if landed /= NoEvent then idle else falling
    
    falling :: SF Input Character
    falling = proc input -> do
        char <- fallingCharacter -< input
        
        landed <- edge -< isOnGround char
        
        switch falling $ \_ ->
            if landed /= NoEvent then idle else falling
```

### Audio Synthesis

```haskell
-- Generate tones
tone :: Double -> SF () Double
tone freq = proc () -> do
    t <- time -< ()
    returnA -< sin (2 * pi * freq * t)

-- ADSR Envelope
envelope :: Double -> Double -> Double -> Double -> SF (Event ()) Double
envelope attack decay sustain release = proc trigger -> do
    -- Attack phase
    attackValue <- ramp attack -< trigger
    
    -- Decay phase
    decayValue <- ramp decay -< () <$ (attackValue >= 1.0)
    
    -- Sustain
    sustainValue <- hold sustain -< decayValue
    
    -- Release
    releaseEnd <- edge -< trigger == NoEvent
    releaseValue <- ramp release -< releaseEnd
    
    returnA -< min attackValue (max sustainValue releaseValue)
  where
    ramp :: Double -> SF (Event ()) Double
    ramp duration = undefined  -- Implementation
```

## Advanced Patterns

### Dynamic Collections

```haskell
-- Manage multiple dynamic objects
type ObjectID = Int

data GameWorld = GameWorld
    { objects :: Map ObjectID GameObject
    }

-- Parallel switching for multiple objects
multiObjectSF :: SF (Map ObjectID Input) (Map ObjectID GameObject)
multiObjectSF = dpSwitch route initial generate
  where
    route :: Map ObjectID Input -> SF (Map ObjectID Input) (Map ObjectID GameObject, Event (Change ObjectID))
    
    initial :: Map ObjectID (SF Input GameObject)
    
    generate :: Map ObjectID (SF Input GameObject) -> Event (Change ObjectID) -> SF (Map ObjectID Input) (Map ObjectID GameObject)
```

### Higher-Order Signal Functions

```haskell
-- Apply different SF based on mode
modeSwitcher :: SF (Mode, Input) Output
modeSwitcher = proc (mode, input) -> do
    rec
        output <- currentSF -< input
        let newSF = modeToSF mode
        currentSF <- iPre defaultSF -< newSF
    returnA -< output
```

## Related Patterns

- **[Arrow](../basic/arrow)** - Foundation of arrowized FRP
- **[Monad](../basic/monad)** - Alternative for sequencing
- **[Comonad](../basic/comonad)** - Related abstraction

## Advantages

1. **Composability**: Build complex behaviors from simple pieces
2. **Declarative**: Describe what, not how
3. **Time Handling**: Elegant treatment of time
4. **Modularity**: Easy to test and refactor
5. **Type Safety**: Catch errors at compile time

## Disadvantages

1. **Learning Curve**: Arrow notation is unfamiliar
2. **Debugging**: Harder to debug than imperative code
3. **Performance**: Can have overhead
4. **Space Leaks**: Need to be careful with accumulation

## Libraries

- **Yampa**: Most mature FRP library for Haskell
- **Netwire**: Alternative with different trade-offs
- **reactive-banana**: Event-based FRP (not arrow-based)
- **reflex**: FRP for web applications

## Best Practices

1. **Use arrow notation**: Makes code more readable
2. **Avoid space leaks**: Be careful with recursive definitions
3. **Compose liberally**: Small, focused signal functions
4. **Test components**: Test SFs in isolation
5. **Profile performance**: FRP can have unexpected costs

## Further Reading

- [The Yampa Arcade](http://www.cs.nott.ac.uk/~psxld/yampa-arcade.html)
- [Functional Reactive Programming, Continued](https://www.cs.yale.edu/homes/hudak/CS429F04/AFPLectureNotes.pdf)
- [Yampa Documentation](https://wiki.haskell.org/Yampa)
- [Arrow-Based FRP](http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf)
