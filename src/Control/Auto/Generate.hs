{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Control.Auto.Generate
-- Description : 'Auto's that act as generators or "producers", ignoring input.
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- This module contains various 'Auto's that act as "producers", ignoring
-- their input and producing output through some source or a pure/monadic
-- function.
--
-- === Constant producers
--
-- Are you looking for "constant producers"?  'Auto's that constantly
-- output the same thing, or repeatedly execute the same monadic action and
-- return the result?  To keep things clean, they aren't re-exported here.
-- But you'll find the "constant 'Auto'" as pure from the 'Applicative'
-- interface:
--
-- @
-- 'pure' :: 'Monad' m => b -> 'Auto' m a b
-- @
--
-- And also the "repeatedly execute and return" 'Auto' as 'effect' from
-- "Control.Auto.Effects":
--
-- @
-- 'effect' :: 'Monad' m => m b -> 'Auto' m a b
-- @
--

module Control.Auto.Generate (
  -- * From lists
    fromList
  , fromLongList
  , fromList_
  -- * From functions
  -- ** Iterating
  , iterator
  , iteratorM
  , iterator_
  , iteratorM_
  -- ** Unfolding
  -- | "Iterating with state".
  , unfold
  , unfoldM
  , unfold_
  , unfoldM_
  ) where

import Control.Applicative
import Control.Auto.Core
import Control.Category
import Data.Serialize
import Prelude hiding                 ((.), id)

-- | Construct an 'Auto' that ignores its input and just continually emits
-- elements from the given list.  Ouputs 'Nothing' forever after reaching
-- the end of the list.
--
-- Serializes itself by storing the entire rest of the list in binary, so
-- if your list is long, it might take up a lot of space upon
-- storage.  If your list is infinite, it makes an infinite binary, so be
-- careful!
--
--   * Storing: O(n) time and space on length of remaining list
--   * Loading: O(1) time in the number of times the 'Auto' has been
--   stepped + O(n) time in the length of the remaining list.
--
fromList :: (Serialize b, Monad m)
         => [b]                 -- ^ list to output element-by-element
         -> Auto m a (Maybe b)
fromList = mkState (const _uncons)

-- | A version of 'fromList' that is safe for long or infinite lists, or
-- lists with unserializable elements.
--
-- There is a small cost in the time of loading/resuming, which is @O(n)@
-- on the number of times the Auto had been stepped at the time of
-- saving.  This is because it has to drop the @n@ first elements in the
-- list, to "resume" to the proper position.
--
--   * Storing: O(1) time and space on the length of the remaining list
--   * Loading: O(n) time on the number of times the 'Auto' has been
--   stepped, maxing out at O(n) on the length of the entire input list.
fromLongList :: Monad m
             => [b]                 -- ^ list to output element-by-element
             -> Auto m a (Maybe b)
fromLongList xs = go 0 xs
  where
    loader = do
      stopped <- get
      if stopped
        then return finished
        else do
          i <- get
          return (go i (drop i xs))
    finished = mkAuto loader
                      (put True)
                      $ \_ -> Output Nothing finished
    go i ys  = mkAuto loader
                      (put (False, i))
                      $ \_ -> case ys of
                                (y':ys') -> Output (Just y') (go (i + 1) ys')
                                []       -> Output Nothing finished

-- | The non-resuming/non-serializing version of 'fromList'.
fromList_ :: Monad m
          => [b]                -- ^ list to output element-by-element
          -> Auto m a (Maybe b)
fromList_ = mkState_ (const _uncons)

_uncons :: [a] -> (Maybe a, [a])
_uncons []     = (Nothing, [])
_uncons (x:xs) = (Just x , xs)

-- | Analogous to 'unfoldr' from "Prelude".  "unfold" out the outputs of an
-- 'Auto'; maintains an accumulator of type @c@, and at every step, applies
-- the unfolding function to the accumulator.  If the result is 'Nothing',
-- then the rest of the Auto will be 'Nothing' forever.  If the result is
-- @'Just' (y, acc)@, outputs @y@ and stores @acc@ as the new accumulator.
--
-- Given an initial accumulator.
unfold :: forall m a b c. (Serialize c, Monad m)
       => (c -> Maybe (b, c))     -- ^ unfolding function
       -> c                       -- ^ initial accumulator
       -> Auto m a (Maybe b)
unfold f = mkState (_unfoldF f) . Just

-- | Like 'unfold', but the unfolding function is monadic.
unfoldM :: forall m a b c. (Serialize c, Monad m)
        => (c -> m (Maybe (b, c)))     -- ^ unfolding function
        -> c                           -- ^ initial accumulator
        -> Auto m a (Maybe b)
unfoldM f = mkStateM (_unfoldMF f) . Just

-- | The non-resuming & non-serializing version of 'unfold'.
unfold_ :: forall m a b c. Monad m
        => (c -> Maybe (b, c))     -- ^ unfolding function
        -> c                       -- ^ initial accumulator
        -> Auto m a (Maybe b)
unfold_ f = mkState_ (_unfoldF f) . Just

-- | The non-resuming & non-serializing version of 'unfoldM'.
unfoldM_ :: forall m a b c. Monad m
         => (c -> m (Maybe (b, c)))     -- ^ unfolding function
         -> c                           -- ^ initial accumulator
         -> Auto m a (Maybe b)
unfoldM_ f = mkStateM_ (_unfoldMF f) . Just

_unfoldF :: (c -> Maybe (b, c))
         -> a
         -> Maybe c
         -> (Maybe b, Maybe c)
_unfoldF _ _ Nothing  = (Nothing, Nothing)
_unfoldF f _ (Just x) = case f x of
                          Just (y, x') -> (Just y, Just x')
                          Nothing      -> (Nothing, Nothing)

_unfoldMF :: Monad m
          => (c -> m (Maybe (b, c)))
          -> a
          -> Maybe c
          -> m (Maybe b, Maybe c)
_unfoldMF _ _ Nothing  = return (Nothing, Nothing)
_unfoldMF f _ (Just x) = do
    res <- f x
    return $ case res of
               Just (y, x') -> (Just y, Just x')
               Nothing      -> (Nothing, Nothing)





-- | Analogous to 'iterate' from "Prelude".  Keeps accumulator value and
-- continually applies the function to the value at every step, outputting
-- the result.
--
-- The first result is the initial accumulator value.
--
-- >>> let (y, _) = stepAutoN' 10 (iterator (*2) 1) ()
-- >>> y
-- [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
iterator :: (Serialize b, Monad m)
         => (b -> b)        -- ^ iterating function
         -> b               -- ^ starting value and initial output
         -> Auto m a b
iterator f = a_
  where
    a_ y0 = mkAuto (a_ <$> get)
                   (put y0)
                   $ \_ -> Output y0 (a_ (f y0))

-- | Like 'iterator', but with a monadic function.
iteratorM :: (Serialize b, Monad m)
          => (b -> m b)     -- ^ (monadic) iterating function
          -> b              -- ^ starting value and initial output
          -> Auto m a b
iteratorM f = a_
  where
    a_ y0 = mkAutoM (a_ <$> get)
                    (put y0)
                    $ \_ -> do
                        y1 <- f y0
                        return (Output y0 (a_ y1))

-- | The non-resuming/non-serializing version of 'iterator'.
iterator_ :: Monad m
          => (b -> b)        -- ^ iterating function
          -> b               -- ^ starting value and initial output
          -> Auto m a b
iterator_ f = a_
  where
    a_ y0 = mkAuto_ $ \_ -> Output y0 (a_ (f y0))

-- | The non-resuming/non-serializing version of 'iteratorM'.
iteratorM_ :: Monad m
           => (b -> m b)     -- ^ (monadic) iterating function
           -> b              -- ^ starting value and initial output
           -> Auto m a b
iteratorM_ f = a_
  where
    a_ y0 = mkAutoM_ $ \_ -> do
                         y1 <- f y0
                         return (Output y0 (a_ y1))

