{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Control.Auto.Run
-- Description : Various utilities for running and unrolling 'Auto's, both
--               interactively and non-interactively.
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- This module provides utilities for "running" and "unrolling" 'Auto's.
-- You'll find "enhanced" versions of 'stepAuto', mechanisms for running
-- 'Auto's "interactively" inside 'IO', monadic and non-monadic
-- "self-runners" (provide the handlers, and the 'Auto' just recursively
-- runs intself), and finally, ways of "unrolling" the underlying 'Monad'
-- of 'Auto's into more manageable and composable and easy to work with
-- forms.
--

module Control.Auto.Run (
  -- * Special 'stepAuto' versions.
  -- ** Streaming over lists
    streamAuto
  , streamAuto'
  , overList
  , overList'
  -- ** Running over one item repetitively
  , stepAutoN
  , stepAutoN'
  , evalAutoN
  , evalAutoN'
  -- * Running "interactively"
  , interactAuto
  , interactRS
  , interactM
  -- ** Helpers
  , duringRead
  , bindRead
  -- * Generalized "self-runners"
  , run
  , runM
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Core
import Control.Auto.Interval
import Control.Monad hiding       (mapM, mapM_)
import Data.Functor.Identity
import Data.Maybe
import Prelude hiding             (interact, mapM, mapM_)
import Text.Read

-- | Steps the 'Auto' through every element of the given list as input.
--
-- >>> let a          = accum (+) 0
-- >>> let (ys, a')   = runIdentity (overList a [4,8,-3,5])
-- >>> ys
-- [4, 12, 9, 14]
-- >>> let Output y _ = runIdentity (stepAuto a 7)
-- >>> y
-- 21
overList :: Monad m
         => Auto m a b            -- ^ the 'Auto' to run
         -> [a]                   -- ^ list of inputs to step the 'Auto' with
         -> m ([b], Auto m a b)   -- ^ list of outputs and the updated 'Auto'
overList a []     = return ([], a)
overList a (x:xs) = do
    Output y a' <- stepAuto a  x
    (ys, a'')   <- overList a' xs
    return (y:ys, a'')

-- | Like 'overList', but with an 'Auto'' (the underlying 'Monad' is
-- 'Identity')
--
-- >>> let a          = accum (+) 0
-- >>> let (ys, a')   = overList' a [4,8,-3,5]
-- >>> ys
-- [4, 12, 9, 14]
-- >>> let Output y _ = stepAuto' a 7
-- >>> y
-- 21
overList' :: Auto' a b          -- ^ the 'Auto'' to run
          -> [a]                -- ^ list of inputs to step the 'Auto'' with
          -> ([b], Auto' a b)   -- ^ list of outputs and the updated 'Auto''
overList' a []     = ([], a)
overList' a (x:xs) = let Output y a' = stepAuto' a x
                         (ys, a'')   = overList' a' xs
                     in  (y:ys, a'')

-- | Stream an 'Auto' over a list, returning the list of results.  Does
-- this "lazily" (over the Monad), so with most Monads, this should work
-- fine with infinite lists.
--
-- Note that, conceptually, this turns an @'Auto' m a b@ into an @[a] ->
-- m [b]@.
streamAuto :: Monad m
           => Auto m a b        -- ^ 'Auto' to stream
           -> [a]               -- ^ input stream
           -> m [b]             -- ^ output stream
streamAuto _ []     = return []
streamAuto a (x:xs) = do
    Output y a' <- stepAuto a x
    ys <- streamAuto a' xs
    return (y:ys)

-- | Stream an 'Auto'' over a list, returning the list of results.  Does
-- this lazily, so this should work fine with (and is actually somewhat
-- designed for) infinite lists.
--
-- Note that conceptually this turns an @'Auto'' a b@ into an @[a] -> [b]@
streamAuto' :: Auto' a b        -- ^ 'Auto'' to stream
            -> [a]              -- ^ input stream
            -> [b]              -- ^ output stream
streamAuto' _ []     = []
streamAuto' a (x:xs) = let Output y a' = stepAuto' a x
                           ys          = streamAuto' a' xs
                       in  y:ys

-- | Repeatedly steps an 'Auto' with the same input a given number of
-- times.
--
-- prop> stepAutoN n a0 x = overList a0 (replicate n x)
--
-- >>> let a          = iterator (*2) 1
-- >>> let (ys, a')   = runIdentity (stepAutoN 8 a ())
-- >>> ys
-- [1, 2, 4, 8, 16, 32, 64, 128]
-- >>> let Output y _ = runIdentity (stepAuto a ())
-- >>> y
-- 256
stepAutoN :: Monad m
          => Int                  -- ^ number of times to step the 'Auto'
          -> Auto m a b           -- ^ the 'Auto' to run
          -> a                    -- ^ the repeated input
          -> m ([b], Auto m a b)  -- ^ list of outputs and the updated 'Auto'
stepAutoN n a0 x = go (max n 0) a0
  where
    go 0 a = return ([], a)
    go i a = do
      Output y a' <- stepAuto a x
      (ys, a'')   <- go (i - 1)  a'
      return (y:ys, a'')

-- | Like 'stepAutoN', but with an 'Auto'' (the underlying 'Monad' is
-- 'Identity')
--
-- >>> let a          = iterator (*2) 1
-- >>> let (ys, a')   = stepAutoN 8 a ()
-- >>> ys
-- [1, 2, 4, 8, 16, 32, 64, 128]
-- >>> let Output y _ = stepAuto a ()
-- >>> y
-- 256
stepAutoN' :: Int                 -- ^ number of times to step the 'Auto''
           -> Auto' a b           -- ^ the 'Auto'' to run
           -> a                   -- ^ the repeated input
           -> ([b], Auto' a b)    -- ^ list of outputs and the updated 'Auto''
stepAutoN' n a0 x = runIdentity (stepAutoN n a0 x)

-- | Like 'stepAutoN', but drops the "next 'Auto'".  Only returns the list
-- of results.
evalAutoN :: Monad m
          => Int                  -- ^ number of times to step the 'Auto'
          -> Auto m a b           -- ^ the 'Auto' to run
          -> a                    -- ^ the repeated input
          -> m [b]                -- ^ list of outputs
evalAutoN n a0 = liftM fst . stepAutoN n a0

-- | Like 'stepAutoN'', but drops the "next 'Auto''".  Only returns the
-- list of results.
--
-- 'evalAutoN' for 'Auto''.
evalAutoN' :: Int                 -- ^ number of times to step the 'Auto''
           -> Auto' a b           -- ^ the 'Auto'' to run
           -> a                   -- ^ the repeated input
           -> [b]                 -- ^ list of outputs and the updated 'Auto''
evalAutoN' n a0 = fst . stepAutoN' n a0

-- execAutoN :: Monad m
--           => Int
--           -> Auto m a b
--           -> a
--           -> m (Auto m a b)
-- execAutoN n a0 = liftM snd . stepAutoN n a0

-- execAutoN' :: Int
--            -> Auto' a b
--            -> a
--            -> Auto' a b
-- execAutoN' n a0 = snd . stepAutoN' n a0

-- | Heavy duty abstraction for "self running" an 'Auto'.  Give a starting
-- input and a function from an output to the next input to feed in, and
-- the 'Auto', and you get a feedback loop that constantly feeds back in
-- the result of the function applied to the previous output.  "Stops" when
-- said function returns 'Nothing'.
--
-- Note that the none of the results are returned from the loop.  Instead,
-- if you want to process the results, they must be utilized in the
-- "side-effects' of the "next input" function.  (ie, a write to a file, or
-- an accumulation to a state).
--
run :: Monad m
    => a
    -> (b -> m (Maybe a))
    -> Interval m a b
    -> m (Interval m a b)
run x0 f = runM x0 f id

-- | A generalized version of 'run' where the 'Monad' you are "running" the
-- 'Auto' in is different than the 'Monad' underneath the 'Auto'.  You just
-- need to provide the natural transformation.
runM :: (Monad m, Monad m')
     => a                         -- ^ Starting input
     -> (b -> m (Maybe a))        -- ^ Handling output and next input in @m@
     -> (forall c. m' c -> m c)   -- ^ Natural transformation from @m'@ (the Auto monad) to @m@ (the running monad)
     -> Interval m' a b           -- ^ Auto in monad @m'@
     -> m (Interval m' a b)       -- ^ Return the resulting/run Auto in @m@
runM x0 f nt a = do
    Output my a' <- nt $ stepAuto a x0
    case my of
      Just y  -> do
        x1 <- f y
        case x1 of
          Just x  -> runM x f nt a'
          Nothing -> return a'
      Nothing ->
        return a'

-- | Run an 'Auto'' "interactively".  Every step grab a string from stdin,
-- and feed it to the 'Auto'.  If the 'Auto' pops out 'Nothing', end the
-- session; if it outputs a 'Just', print it out to stdout and repeat all
-- over again.
--
-- If your 'Auto' outputs something other than a 'String', you can use
-- 'fmap' to transform the output into a 'String' en-route (like @'fmap'
-- 'show'@).
--
-- If your 'Auto' takes in something other than a 'String', you can 'lmap'
-- a function to convert the input 'String' to whatever intput your 'Auto'
-- expects.
--
-- You can use 'duringRead' or 'bindRead' if you have an 'Auto'' or
-- 'Interval'' that takes something 'read'able, to chug along until you
-- find something non-readable.
--
-- Outputs the final 'Auto'' when the interaction terminates.
interactAuto :: Interval' String String         -- ^ 'Interval'' to run interactively
             -> IO (Interval' String String)    -- ^ final 'Interval'' after it all
interactAuto = interactM putStrLn (return . runIdentity)

-- | Like 'interact', but instead of taking @'Interval'' 'String'
-- 'String'@, takes any @'Interval'' a b@ as long as @a@ is 'Read' and @b@
-- is 'Show'.
--
-- Will "stop" if either (1) the input is not 'read'-able or (2) the
-- 'Interval'' turns off.
--
-- Outputs the final 'Auto'' when the interaction terminates.
interactRS :: (Read a, Show b)
           => Interval' a b                 -- ^ 'Interval'' to run interactively
           -> IO (Interval' String String)  -- ^ final 'Interval'' after it all
interactRS = interactAuto . bindRead . fmap (fmap show)


-- | Like 'interact', but much more general.  You can run it with an 'Auto'
-- of any underlying 'Monad', as long as you provide the natural
-- transformation from that 'Monad' to 'IO'.
--
-- The 'Auto' can also output any @'Maybe' b@; you have to provide
-- a function to "handle" it yourself; a @b -> 'IO' '()'@ (if you don't
-- want to print it and you wanted to, say, write it to a file.)
interactM :: Monad m
          => (b -> IO ())             -- ^ function to "handle" each succesful 'Auto' output
          -> (forall c. m c -> IO c)  -- ^ natural transformation from the underlying 'Monad' of the 'Auto' to 'IO'
          -> Interval m String b      -- ^ 'Auto' to run "interactively"
          -> IO (Interval m String b) -- ^ final 'Auto' after it all
interactM f nt a = do
    x <- getLine
    runM x f' nt a
  where
    f' y = do
      f y
      Just <$> getLine

-- | Turn an 'Auto' that takes a "readable" @a@ and outputs a @b@ into an
-- 'Auto' that takes a 'String' and outputs a @'Maybe' b@.  When the
-- 'String' is successfuly readable as the @a@, it steps the 'Auto' and
-- outputs a succesful 'Just' result; when it isn't, it outputs a 'Nothing'
-- on that step.
--
-- >>> let a0 = duringRead (accum (+) (0 :: Int))
-- >>> let Output y1 a1 = stepAuto' a0 "12"
-- >>> y1
-- Just 12
-- >>> let Output y2 a2 = stepAuto' a1 "orange"
-- >>> y2
-- Nothing
-- >>> let Output y3 _  = stepAuto' a2 "4"
-- >>> y3
-- Just 16
--
-- See 'interact' for neat use cases.
duringRead :: (Monad m, Read a)
           => Auto m a b                -- ^ 'Auto' taking in a readable @a@, outputting @b@
           -> Interval m String b       -- ^ 'Auto' taking in 'String', outputting @'Maybe' b@
duringRead a = during a <<^ readMaybe

-- | Like 'duringRead', but the original 'Auto' would output a @'Maybe' b@
-- instead of a @b@.  Returns 'Nothing' if either the 'String' fails to
-- parse or if the original 'Auto' returned 'Nothing'; returns 'Just' if
-- the 'String' parses and the original 'Auto' returned 'Just'.
--
-- See 'interact' for neat use cases.
bindRead :: (Monad m, Read a)
         => Interval m a b        -- ^ 'Auto' taking in a readable @a@, outputting @'Maybe' b@
         -> Interval m String b   -- ^ 'Auto' taking in 'String', outputting @'Maybe' b@
bindRead a = bindI a <<^ readMaybe
