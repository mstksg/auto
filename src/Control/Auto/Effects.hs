{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Control.Auto.Effects
-- Description : Accessing, executing, and manipulating underyling monadic
--               effects.
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- This module exports the preferred ways of interacting with the
-- underlying 'Monad' of the 'Auto' type, including accessing, executing,
-- and manipulating such effects.
--


module Control.Auto.Effects (
  -- * Running effects
  -- ** Continually
    arrM
  , effect
  , exec
  -- ** From inputs
  , effects
  -- ** On 'Blip's
  , arrMB
  , effectB
  , execB
  -- * One-time effects
  , cache
  , execOnce
  , cache_
  , execOnce_
  -- * "Sealing off" monadic 'Auto's
  , sealState
  , sealState_
  , sealReader
  , sealReader_
  ) where

import Control.Applicative
import Control.Auto.Blip
import Control.Auto.Core
import Control.Auto.Generate
import Control.Category
import Control.Monad
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State  (StateT, runStateT)
import Data.Serialize
import Prelude hiding             ((.), id)

-- | The very first output executes a monadic action and uses the result as
-- the output, ignoring all input.  From then on, it persistently outputs
-- that first result.
--
-- Like 'execOnce', except outputs the result of the action instead of
-- ignoring it.
--
-- Useful for loading resources in IO on the "first step", like
-- a word list:
--
-- @
-- dictionary :: Auto IO a [String]
-- dictionary = cache (lines <$> readFile "wordlist.txt")
-- @
--
cache :: (Serialize b, Monad m)
      => m b          -- ^ monadic action to execute and use the result of
      -> Auto m a b
cache m = snd <$> iteratorM (_cacheF m) (False, undefined)

-- | The non-resumable/non-serializable version of 'cache'.  Every time the
-- 'Auto' is deserialized/reloaded, it re-executes the action to retrieve
-- the result again.
--
-- Useful in cases where you want to "re-load" an expensive resource on
-- every startup, instead of saving it to in the save states.
--
-- @
-- dictionary :: Auto IO a [String]
-- dictionary = cache_ (lines <$> readFile "dictionary.txt")
-- @
cache_ :: Monad m
       => m b         -- ^ monadic action to execute and use the result of
       -> Auto m a b
cache_ m = snd <$> iteratorM_ (_cacheF m) (False, undefined)

_cacheF :: Monad m => m b -> (Bool, b) -> m (Bool, b)
_cacheF m (False, _) = liftM  (True,) m
_cacheF _ (True , x) = return (True, x)
{-# INLINE _cacheF #-}

-- | Always outputs '()', but when asked for the first output, executes the
-- given monadic action.
--
-- Pretty much like 'cache', but always outputs '()'.
--
execOnce :: Monad m
         => m b           -- ^ monadic action to execute; result discared
         -> Auto m a ()
execOnce m = mkStateM (\_ -> _execOnceF m) False

-- | The non-resumable/non-serializable version of 'execOnce'.  Every time
-- the 'Auto' is deserialized/reloaded, the action is re-executed again.
execOnce_ :: Monad m
          => m b          -- ^ monadic action to execute; result discared
          -> Auto m a ()
execOnce_ m = mkStateM_ (\_ -> _execOnceF m) False

_execOnceF :: Monad m => m a -> Bool -> m ((), Bool)
_execOnceF m = go
  where
    go False = liftM (const ((), True)) m
    go _     = return ((), True)

-- | To get every output, executes the monadic action and returns the
-- result as the output.  Always ignores input.
--
-- This is basically like an "effectful" 'pure':
--
-- @
-- 'pure'   :: b   -> 'Auto' m a b
-- 'effect' :: m b -> 'Auto' m a b
-- @
--
-- The output of 'pure' is always the same, and the output of 'effect' is
-- always the result of the same monadic action.  Both ignore their inputs.
--
-- Fun times when the underling 'Monad' is, for instance, 'Reader'.
--
-- >>> let a = effect ask    :: Auto (Reader b) a b
-- >>> let r = evalAuto a () :: Reader b b
-- >>> runReader r "hello"
-- "hello"
-- >>> runReader r 100
-- 100
--
effect :: m b           -- ^ monadic action to contually execute.
       -> Auto m a b
effect = mkConstM
{-# INLINE effect #-}

-- | Acts like 'id', in that the output stream is identical to the input
-- stream.  However, at each retrieval of output, executes the given
-- monadic action and ignores the result.
exec :: Monad m
     => m b           -- ^ monadic action to contually execute.
     -> Auto m a a
exec m = mkFuncM $ \x -> m >> return x
{-# INLINE exec #-}

-- | The input stream is a stream of monadic actions, and the output stream
-- is the result of their executions, through executing them.
effects :: Monad m => Auto m (m a) a
effects = arrM id

-- | Applies the given "monadic function" (function returning a monadic
-- action) to every incoming item; the result is the result of executing
-- the action returned.
--
-- Note that this essentially lifts a "Kleisli arrow"; it's like 'arr', but
-- for "monadic functions" instead of normal functions:
--
-- @
-- arr  :: (a -> b)   -> Auto m a b
-- arrM :: (a -> m b) -> Auto m a b
-- @
--
-- prop> arrM f . arrM g == arrM (f <=< g)
arrM :: (a -> m b)    -- ^ monadic function
     -> Auto m a b
arrM = mkFuncM
{-# INLINE arrM #-}

-- | Maps one blip stream to another; replaces every emitted value with the
-- result of the monadic function, executing it to get the result.
arrMB :: Monad m
      => (a -> m b)
      -> Auto m (Blip a) (Blip b)
arrMB = perBlip . arrM
{-# INLINE arrMB #-}

-- | Maps one blip stream to another; replaces every emitted value with the
-- result of a fixed monadic action, run every time an emitted value is
-- received.
effectB :: Monad m
        => m b
        -> Auto m (Blip a) (Blip b)
effectB = perBlip . effect
{-# INLINE effectB #-}

-- | Outputs the identical blip stream that is received; however, every
-- time it sees an emitted value, executes the given monadic action on the
-- side.
execB :: Monad m
      => m b
      -> Auto m (Blip a) (Blip a)
execB = perBlip . exec
{-# INLINE execB #-}

-- | Takes an 'Auto' that works with underlying global, mutable state, and
-- "seals off the state" from the outside world.
--
-- An 'Auto (StateT s m) a b' maps a stream of 'a' to a stream of 'b', but
-- does so in the context of requiring an initial 's' to start, and
-- outputting a modified 's'.
--
-- Consider this example 'State' 'Auto':
--
-- @
-- foo :: Auto (State s) Int Int
-- foo = proc x -> do
--     execB (modify (+1)) . emitOn odd  -< x
--     execB (modify (*2)) . emitOn even -< x
--     st   <- effect get -< ()
--     sumX <- sumFrom 0  -< x
--     id    -< sumX + st
-- @
--
-- On every output, the "global" state is incremented if the input is odd
-- and doubled if the input is even.  The stream @st@ is always the value
-- of the global state at that point.  @sumX@ is the cumulative sum of the
-- inputs.  The final result is the sum of the value of the global state
-- and the cumulative sum.
--
-- In writing like this, you lose some of the denotative properties because
-- you are working with a global state that updates at every output.  You
-- have some benefit of now being able to work with global state, if that's
-- what you wanted I guess.
--
-- To "run" it, you could use 'streamAuto' to get a @'State' Int Int@:
--
-- >>> let st = streamAuto foo [1..10] :: State Int Int
-- >>> runState st 5
-- ([  7, 15, 19, 36, 42, 75, 83,136,156,277], 222)
--
-- (The starting state is 5 and the ending state after all of that is 222)
--
-- However, writing your entire program with global state is a bad bad
-- idea!  So, how can you get the "benefits" of having small parts like
-- @foo@ be written using 'State', and being able to use it in a program
-- with no global state?
--
-- Using 'sealState'!
--
-- @
-- sealState       :: Auto (State s) a b -> s -> Auto' a b
-- sealState foo 5 :: Auto' Int Int
-- @
--
-- @
-- bar :: Auto' Int (Int, String)
-- bar = proc x -> do
--     food <- sealState foo 5 -< x
--     id -< (food, show x)
-- @
--
-- >>> streamAuto' bar [1..10]
-- [ (7, "1"), (15, "2"), (19, "3"), (36, "4"), (42, "5"), (75, "6") ...
--
-- We say that @'sealState' f s0@ takes an input stream, and the output
-- stream is the result of running the stream through @f@, first with an
-- initial state of @s0@, and afterwards with each next updated state.
--
sealState :: (Monad m, Serialize s)
          => Auto (StateT s m) a b
          -> s
          -> Auto m a b
sealState a s0 = mkAutoM (sealState <$> loadAuto a <*> get)
                         (saveAuto a *> put s0)
                         $ \x -> do
                             (Output y a', s1) <- runStateT (stepAuto a x) s0
                             return $ Output y (sealState a' s1)

-- | The non-resuming/non-serializing version of 'sealState'.
sealState_ :: Monad m
           => Auto (StateT s m) a b
           -> s
           -> Auto m a b
sealState_ a s0 = mkAutoM (sealState_ <$> loadAuto a <*> pure s0)
                          (saveAuto a)
                          $ \x -> do
                              (Output y a', s1) <- runStateT (stepAuto a x) s0
                              return $ Output y (sealState_ a' s1)

-- | Takes an 'Auto' that operates under the context of a read-only
-- environment, an environment value, and turns it into a normal 'Auto'
-- that always "sees" that value when it asks for one.
--
-- ghci> let a   = effect ask :: Auto (Reader b) a b
-- ghci> let rdr = streamAuto' a [1..5] :: Reader b [b]
-- ghci> runReader rdr "hey"
-- ["hey", "hey", "hey", "hey", "hey"]
--
-- Useful if you wanted to use it inside/composed with an 'Auto' that does
-- not have a global environment:
--
-- @
-- bar :: Auto' Int String
-- bar = proc x -> do
--     hey <- sealReader (effect ask) "hey" -< ()
--     id -< hey ++ show x
-- @
--
-- ghci> streamAuto' bar [1..5]
-- ["hey1", "hey2", "hey3", "hey4", "hey5"]
--
sealReader :: (Monad m, Serialize r)
           => Auto (ReaderT r m) a b
           -> r
           -> Auto m a b
sealReader a r = mkAutoM (sealReader <$> loadAuto a <*> get)
                         (saveAuto a *> put r)
                         $ \x -> do
                             Output y a' <- runReaderT (stepAuto a x) r
                             return $ Output y (sealReader a' r)

-- | The non-resuming/non-serializing version of 'sealReader'.
sealReader_ :: Monad m
            => Auto (ReaderT r m) a b
            -> r
            -> Auto m a b
sealReader_ a r = mkAutoM (sealReader_ <$> loadAuto a <*> pure r)
                          (saveAuto a)
                          $ \x -> do
                              Output y a' <- runReaderT (stepAuto a x) r
                              return $ Output y (sealReader_ a' r)
