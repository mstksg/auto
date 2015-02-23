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
import Control.Monad.Trans.State           (StateT, runStateT)
import Control.Monad.Trans.Reader           (ReaderT, runReaderT)
import Data.Serialize
import Prelude hiding                      ((.), id)

-- | "Executes" a monadic action once (and only once), and continues to
-- echo the result forevermore.
--
-- Like 'execOnce', exept outputs the result of the action.
--
-- Useful for loading resources in IO on the "first step", like
-- a dictionary:
--
-- @
-- dictionary :: Auto IO a [String]
-- dictionary = cache (lines <$> readFile "dictionary.txt")
-- @
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

-- | "Executes" the monadic action once, and outputs '()' forevermore.
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

-- | "Executes" the given monadic action at every tick/step of the 'Auto',
-- outputting the result.  Particularly useful with 'Reader' as the
-- underlying 'Monad'.
--
-- >>> let a = effect ask     :: Auto (Reader b) a b
-- >>> let r = evalAuto a ()  :: Reader b b
-- >>> runReader r "hello"
-- "hello"
-- >>> runReader r 123
-- 123
--
-- Basically like `pure`, but instead of `pure :: b -> Auto m a b`, you
-- have `effect :: m b -> Auto m a b`.
effect :: m b           -- ^ monadic action to contually execute.
       -> Auto m a b
effect = mkConstM
{-# INLINE effect #-}

-- | "Executes" the given monadic action at every tick/step of the 'Auto',
-- but ignores the result.  Basically acts like the 'id' 'Auto', with
-- attached effects.
exec :: Monad m
     => m b           -- ^ monadic action to contually execute.
     -> Auto m a a
exec m = mkFuncM $ \x -> m >> return x
{-# INLINE exec #-}

-- | Lifts a "monadic function" (Kleisli arrow) into an 'Auto'.  Like
-- 'arr', but with monadic functions.  Composition of such 'Auto's works
-- just like the composition of Kleisli arrows, with '(<=<)':
--
-- prop> arrM f . arrM g == arrM (f <=< g)
arrM :: (a -> m b)    -- ^ monadic function
     -> Auto m a b
arrM = mkFuncM
{-# INLINE arrM #-}

-- | Applies the "monadic function" to the contents of every incoming
-- 'Blip', and replaces the contents of the 'Blip' with the result of the
-- function.
arrMB :: Monad m
      => (a -> m b)
      -> Auto m (Blip a) (Blip b)
arrMB = perBlip . arrM
{-# INLINE arrMB #-}

-- | Executes a monadic action with every incoming 'Blip', and replaces the
-- contents of the 'Blip' with the result of the action.
effectB :: Monad m
        => m b
        -> Auto m (Blip a) (Blip b)
effectB = perBlip . effect
{-# INLINE effectB #-}

-- | Executes a monadic action with every incoming 'Blip', and passes back
-- that same 'Blip' unchanged.
execB :: Monad m
      => m b
      -> Auto m (Blip a) (Blip a)
execB = perBlip . exec
{-# INLINE execB #-}

sealState :: (Monad m, Serialize s)
          => Auto (StateT s m) a b
          -> s
          -> Auto m a b
sealState a s0 = mkAutoM (sealState <$> loadAuto a <*> get)
                         (saveAuto a *> put s0)
                         $ \x -> do
                             (Output y a', s1) <- runStateT (stepAuto a x) s0
                             return $ Output y (sealState a' s1)

sealState_ :: Monad m
           => Auto (StateT s m) a b
           -> s
           -> Auto m a b
sealState_ a s0 = mkAutoM (sealState_ <$> loadAuto a <*> pure s0)
                          (saveAuto a)
                          $ \x -> do
                              (Output y a', s1) <- runStateT (stepAuto a x) s0
                              return $ Output y (sealState_ a' s1)

sealReader :: (Monad m, Serialize r)
           => Auto (ReaderT r m) a b
           -> r
           -> Auto m a b
sealReader a r = mkAutoM (sealReader <$> loadAuto a <*> get)
                         (saveAuto a *> put r)
                         $ \x -> do
                             Output y a' <- runReaderT (stepAuto a x) r
                             return $ Output y (sealReader a' r)

sealReader_ :: Monad m
            => Auto (ReaderT r m) a b
            -> r
            -> Auto m a b
sealReader_ a r = mkAutoM (sealReader_ <$> loadAuto a <*> pure r)
                          (saveAuto a)
                          $ \x -> do
                              Output y a' <- runReaderT (stepAuto a x) r
                              return $ Output y (sealReader_ a' r)
