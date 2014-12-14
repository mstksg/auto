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
    overList
  , overList'
  , stepAutoN
  , stepAutoN'
  -- * Running "interactively"
  , interact
  , interactM
  -- ** Helpers
  , duringRead
  , bindRead
  -- * Generalized "self-runners"
  , run
  , runM
  -- * Unrolling monadic 'Auto's
  , runStateA
  , runReaderA
  , runTraversableA
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Core
import Control.Auto.Interval
import Control.Monad hiding       (mapM, mapM_)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable hiding (concatMap)
import Data.Functor.Identity
import Data.Maybe
import Data.Traversable
import Prelude hiding             (interact, mapM, mapM_)
import Text.Read

-- | Steps the 'Auto' through every element of the given list as input.
--
-- >>> let a          = mkAccum (+) 0
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
-- >>> let a          = mkAccum (+) 0
-- >>> let (ys, a')   = overList' a [4,8,-3,5]
-- >>> ys
-- [4, 12, 9, 14]
-- >>> let Output y _ = stepAuto' a 7
-- >>> y
-- 21
overList' :: Auto' a b          -- ^ the 'Auto'' to run
          -> [a]                -- ^ list of inputs to step the 'Auto'' with
          -> ([b], Auto' a b)   -- ^ list of outputs and the updated 'Auto''
overList' a xs = runIdentity (overList a xs)

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
          -> m ([b], Auto m a b)  -- ^ list of outputs and the updated 'Auto''
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
stepAutoN' :: Int -> Auto' a b -> a -> ([b], Auto' a b)
stepAutoN' n a0 x = runIdentity (stepAutoN n a0 x)

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
-- Useful helper functions 'duringRead' and 'bindRead' are provided for
-- this case.   'duringRead' turns an @'Auto' m a b@ (with @'Read' a@) into
-- an @'Auto' m 'String' ('Maybe' b)@, where it "reads" the 'String' into
-- the 'a'...but if the 'read' fails, the whole 'Auto' returns 'Nothing'.
-- When used with 'interact', that means that a failed 'read' terminates
-- the loop.
interact :: Interval' String String         -- ^ 'Auto' to run interactively
         -> IO (Interval' String String)
interact = interactM putStrLn (return . runIdentity)

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
          -> IO (Interval m String b)
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
-- >>> let a0 = duringRead (mkAccum (+) (0 :: Int))
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

-- | "Unrolls" the underlying 'StateT' of an 'Auto' into an 'Auto' that
-- takes in an input state every turn (in addition to the normal input) and
-- outputs, along with the original result, the modified state.
--
-- So now you can use any @'StateT' s m@ as if it were an @m@.  Useful if
-- you want to compose and create some isolated 'Auto's with access to an
-- underlying state, but not your entire program.
--
-- Also just simply useful as a convenient way to use an 'Auto' over
-- 'State' with 'stepAuto' and friends.
--
-- When used with @'State' s@, it turns an @'Auto' ('State' s) a b@ into an
-- @'Auto'' (a, s) (b, s)@.
runStateA :: Monad m
          => Auto (StateT s m) a b
          -> Auto m (a, s) (b, s)
runStateA a = mkAutoM (runStateA <$> loadAuto a)
                      (saveAuto a)
                      $ \(x, s) -> do
                          (Output y a', s') <- runStateT (stepAuto a x) s
                          return (Output (y, s') (runStateA a'))

-- | "Unrolls" the underlying 'ReaderT' of an 'Auto' into an 'Auto' that
-- takes in the input "environment" every turn in addition to the normal
-- input.
--
-- So you can use any @'ReaderT' r m@ as if it were an @m@.  Useful if you
-- want to compose and create some isolated 'Auto's with access to an
-- underlying environment, but not your entire program.
--
-- Also just simply useful as a convenient way to use an 'Auto' over
-- 'Reader' with 'stepAuto' and friends.
--
-- When used with @'Reader' r@, it turns an @'Auto' ('Reader' r) a b@ into
-- an @'Auto'' (a, r) b@.
runReaderA :: Monad m
           => Auto (ReaderT r m) a b
           -> Auto m (a, r) b
runReaderA a = mkAutoM (runReaderA <$> loadAuto a)
                       (saveAuto a)
                       $ \(x, r) -> do
                           Output y a' <- runReaderT (stepAuto a x) r
                           return (Output y (runReaderA a'))

-- | "Unrolls" the underlying 'Monad' of an 'Auto' if it happens to be
-- 'Traversable' ('[]', 'Maybe', etc.).
--
-- It can turn, for example, an @'Auto' [] a b@ into an @'Auto'' a [b]@; it
-- collects all of the results together.  Or an @'Auto' 'Maybe' a b@ into
-- an @'Auto'' a ('Maybe' b)@.
--
-- If you find a good use for this, let me know :)
runTraversableA :: (Monad f, Traversable f)
                => Auto f a b
                -> Auto m a (f b)
runTraversableA = go . return
  where
    go a = mkAuto (go <$> mapM loadAuto a)
                  (mapM_ saveAuto a)
                  $ \x -> let o  = a >>= (`stepAuto` x)
                              y  = liftM outRes o
                              a' = liftM outAuto o
                          in  Output y (go a')
