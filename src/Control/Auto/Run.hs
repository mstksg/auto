{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Control.Auto.Run
-- Description : Various utilities for running and unrolling 'Auto's, both
--               interactively and non-interactively.
-- Copyright   : (c) Justin Le 2015
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
  -- * Running on concurrent channels
  , runOnChan
  , runOnChanM
  ) where

import Control.Applicative
import Control.Auto.Core
import Control.Auto.Interval
import Control.Concurrent
import Control.Monad hiding  (mapM, mapM_)
import Data.Functor.Identity
import Data.Maybe
import Data.Profunctor
import Prelude hiding        (interact, mapM, mapM_)
import Text.Read

-- | Streams the 'Auto' over a list of inputs; that is, "unwraps" the @[a]
-- -> m [b]@ inside.  Streaming is done in the context of the underlying
-- monad; when done consuming the list, the result is the list of outputs
-- updated/next 'Auto' in the context of the underlying monad.
--
-- Basically just steps the 'Auto' by feeding in every item in the list and
-- pops out the list of results and the updated/next 'Auto', monadically
-- chaining the steppings.
--
-- See 'overList'' for a simpler example; the following example uses
-- effects from 'IO' to demonstrate the monadic features of 'overList'.
--
-- >>> let a = arrM print *> sumFrom 0 :: Auto IO Int Int
-- >>> (ys, a') <- overList a [1..5]
-- 1    -- IO effects
-- 2
-- 3
-- 4
-- 5
-- >>> ys
-- [1,3,6,10,15]
-- >>> (ys', _) <- overList a' [11..15]
-- 11   -- IO effects
-- 12
-- 13
-- 14
-- 15
-- >>> ys'
-- [26,38,51,65,80]
--
-- @a@ is like @'sumFrom' 0@, except at every step, prints the input item
-- to stdout as a side-effect.  Note that in executing we get the updated
-- @a'@, which ends up with an accumulator of 15.  Now, when we stream
-- @a'@, we pick up were we left off (from 15) on the results.
--
overList :: Monad m
         => Auto m a b            -- ^ the 'Auto' to run
         -> [a]                   -- ^ list of inputs to step the 'Auto' with
         -> m ([b], Auto m a b)   -- ^ list of outputs and the updated 'Auto'
overList a []     = return ([], a)
overList a (x:xs) = do
    (y, a')   <- stepAuto a  x
    (ys, a'') <- overList a' xs
    return (y:ys, a'')

-- | Streams an 'Auto'' over a list of inputs; that is, "unwraps" the @[a]
-- -> [b]@ inside.  When done comsuming the list, returns the outputs and
-- the updated/next 'Auto''.
--
-- >>> let (ys, updatedSummer) = overList' (sumFrom 0) [1..5]
-- >>> ys
-- [1, 3, 6, 10, 15]
-- >>> let (ys', _) = streamAuto' updatedSummer [1..5]
-- >>> ys'
-- [16, 18, 21, 25, 30]
--
-- If you wanted to stream over an infinite list then you don't care about
-- the 'Auto'' at the end, and probably want 'streamAuto''.
--
overList' :: Auto' a b          -- ^ the 'Auto'' to run
          -> [a]                -- ^ list of inputs to step the 'Auto'' with
          -> ([b], Auto' a b)   -- ^ list of outputs and the updated 'Auto''
overList' a []     = ([], a)
overList' a (x:xs) = let (y, a')   = stepAuto' a x
                         (ys, a'') = overList' a' xs
                     in  (y:ys, a'')

-- | Stream an 'Auto' over a list, returning the list of results.  Does
-- this "lazily" (over the Monad), so with most Monads, this should work
-- fine with infinite lists.  (That is, @'streamAuto' ('arrM' f)@ behaves
-- exactly like @'mapM' f@, and you can reason with 'Auto's as if you'd
-- reason with @mapM@ on an infinite list)
--
-- Note that, conceptually, this turns an @'Auto' m a b@ into an @[a] ->
-- m [b]@.
--
-- See 'streamAuto'' for a simpler example; here is one taking advantage of
-- monadic effects:
--
-- >>> let a = arrM print *> sumFrom 0 :: Auto IO Int Int
-- >>> ys <- streamAuto a [1..5]
-- 1                -- IO effects
-- 2
-- 3
-- 4
-- 5
-- >>> ys
-- [1,3,6,10,15]    -- the result
--
-- @a@ here is like @'sumFrom' 0@, except at every step, prints the input
-- item to stdout as a side-effect.
--
-- Note that we use "stream" here slightly differently than in libraries
-- like /pipes/ or /conduit/.  We don't stream over the @m@ Monad (like
-- @IO@)...we stream over the __input elements__.  Using 'streamAuto' on an
-- infinite list allows you to "stop", for example, to find the
-- result...but it will still sequence all the *effects*.
--
-- For example:
--
-- >>> take 10 <$> streamAuto (arrM print *> id) [1..]
--
-- Will execute 'print' on every element before "returning" with [1..10].
--
-- >>> flip runState 0 $ take 10 <$> streamAuto (arrM (modify . (+)) *> id) [1..]
-- ([1,2,3,4,5,6,7,8,9,10], .... (never terminates)
--
-- This will immediately return the "result", and you can bind to the
-- result with `(>>=)`, but it'll never return a "final state", because the
-- final state involves executing all of the 'modify's.
--
-- In short, treat this like you'd treat 'mapM'...but it's not a "stream"
-- in the sense of IO streaming libraries; it streams over the /input
-- values/, not the /effects/.
--
streamAuto :: Monad m
           => Auto m a b        -- ^ 'Auto' to stream
           -> [a]               -- ^ input stream
           -> m [b]             -- ^ output stream
streamAuto _ []     = return []
streamAuto a (x:xs) = do
    (y, a') <- stepAuto a x
    ys      <- streamAuto a' xs
    return (y:ys)

-- | Stream an 'Auto'' over a list, returning the list of results.  Does
-- this lazily, so this should work fine with (and is actually somewhat
-- designed for) infinite lists.
--
-- Note that conceptually this turns an @'Auto'' a b@ into an @[a] -> [b]@
--
-- >>> streamAuto' (arr (+3)) [1..10]
-- [4,5,6,7,8,9,10,11,12,13]
-- >>> streamAuto' (sumFrom 0) [1..5]
-- [1,3,6,10,15]
-- >>> streamAuto' (productFrom 1) . streamAuto' (sumFrom 0) $ [1..5]
-- [1,3,18,180,2700]
-- >>> streamAuto' (productFrom 1 . sumFrom 0) $ [1..5]
-- [1,3,18,180,2700]
-- >>> streamAuto' id [1..5]
-- [1,2,3,4,5]
--
streamAuto' :: Auto' a b        -- ^ 'Auto'' to stream
            -> [a]              -- ^ input stream
            -> [b]              -- ^ output stream
streamAuto' _ []     = []
streamAuto' a (x:xs) = let (y, a') = stepAuto' a x
                           ys      = streamAuto' a' xs
                       in  y:ys

-- | Streams (in the context of the underlying monad) the given 'Auto' with
-- a stream of constant values as input, a given number of times.  After
-- the given number of inputs, returns the list of results and the
-- next/updated 'Auto', in the context of the underlying monad.
--
-- prop> stepAutoN n a0 x = overList a0 (replicate n x)
--
-- See 'stepAutoN'' for a simpler example; here is one taking advantage of
-- monadic effects:
--
-- >>> let a = arrM print *> sumFrom 0 :: Auto IO Int Int
-- >>> (ys, a') <- stepAutoN 5 a 3
-- 3                -- IO effects
-- 3
-- 3
-- 3
-- 3
-- >>> ys
-- [3,6,9,12,15]    -- the result
-- >>> (ys'', _) <- stepAutoN 5 a' 5
-- 5                -- IO effects
-- 5
-- 5
-- 5
-- 5
-- >>> ys''
-- [20,25,30,35,50] -- the result
--
-- @a@ here is like @'sumFrom' 0@, except at every step, prints the input
-- item to stdout as a side-effect.
--
stepAutoN :: Monad m
          => Int                  -- ^ number of times to step the 'Auto'
          -> Auto m a b           -- ^ the 'Auto' to run
          -> a                    -- ^ the repeated input
          -> m ([b], Auto m a b)  -- ^ list of outputs and the updated 'Auto'
stepAutoN n a0 x = go (max n 0) a0
  where
    go 0 a = return ([], a)
    go i a = do
      (y , a')  <- stepAuto a x
      (ys, a'') <- go (i - 1)  a'
      return (y:ys, a'')

-- | Streams the given 'Auto'' with a stream of constant values as input,
-- a given number of times.  After the given number of inputs, returns the
-- list of results and the next/updated 'Auto'.
--
-- prop> stepAutoN' n a0 x = overList' a0 (replicate n x)
--
-- >>> let (ys, a') = stepAutoN' 5 (sumFrom 0) 3
-- >>> ys
-- [3,6,9,12,15]
-- >>> let (ys', _) = stepAutoN' 5 a' 5
-- >>> ys'
-- [20,25,30,35,40]
--
stepAutoN' :: Int                 -- ^ number of times to step the 'Auto''
           -> Auto' a b           -- ^ the 'Auto'' to run
           -> a                   -- ^ the repeated input
           -> ([b], Auto' a b)    -- ^ list of outputs and the updated 'Auto''
stepAutoN' n a0 x = runIdentity (stepAutoN n a0 x)

-- | Streams (in the context of the underlying monad) the given 'Auto' with
-- a stream of constant values as input, a given number of times.  After
-- the given number of inputs, returns the list of results in the context
-- of the underlying monad.
--
-- Like 'stepAutoN', but drops the "next 'Auto'".  Only returns the list
-- of results.
--
-- >>> let a = arrM print *> sumFrom 0 :: Auto IO Int Int
-- >>> ys <- evalAutoN 5 a 3
-- 3                -- IO effects
-- 3
-- 3
-- 3
-- 3
-- >>> ys
-- [3,6,9,12,15]    -- the result
--
-- @a@ here is like @'sumFrom' 0@, except at every step, prints the input
-- item to stdout as a side-effect.
evalAutoN :: Monad m
          => Int                  -- ^ number of times to step the 'Auto'
          -> Auto m a b           -- ^ the 'Auto' to run
          -> a                    -- ^ the repeated input
          -> m [b]                -- ^ list of outputs
evalAutoN n a0 = liftM fst . stepAutoN n a0

-- | Streams the given 'Auto'' with a stream of constant values as input,
-- a given number of times.  After the given number of inputs, returns the
-- list of results and the next/updated 'Auto'.
--
-- Like 'stepAutoN'', but drops the "next 'Auto''".  Only returns the list
-- of results.
--
-- >>> evalAutoN' 5 (sumFrom 0) 3
-- [3,6,9,12,15]
--
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
-- input action, a (possibly side-effecting) function from an output to
-- the next input to feed in, and the 'Auto', and you get a feedback
-- loop that constantly feeds back in the result of the function applied to
-- the previous output. "Stops" when the "next input" function returns
-- 'Nothing'.
--
-- Note that the none of the results are actually returned from the loop.
-- Instead, if you want to process the results, they must be utilized in
-- the "side-effects' of the "next input" function.  (ie, a write to
-- a file, or an accumulation to a state).
--
run :: Monad m
    => m a                -- ^ action to retrieve starting input
    -> (b -> m (Maybe a)) -- ^ handling output and next input in @m@
    -> Auto m a b         -- ^ 'Auto'
    -> m (Auto m a b)     -- ^ return the ran/updated 'Auto' in @m@
run = runM id

-- | A generalized version of 'run' where the 'Monad' you are "running" the
-- 'Auto' in is different than the 'Monad' underneath the 'Auto'.  You just
-- need to provide the natural transformation.
runM :: (Monad m, Monad m')
     => (forall c. m' c -> m c)   -- ^ natural transformation from @m'@ (the Auto monad) to @m@ (the running monad)
     -> m a                       -- ^ action to retrieve starting input
     -> (b -> m (Maybe a))        -- ^ handling output and next input in @m@
     -> Auto m' a b               -- ^ 'Auto' in monad @m'@
     -> m (Auto m' a b)           -- ^ return the resulting/run Auto in @m@
runM nt x0 f a = do
    (y, a') <- nt . stepAuto a =<< x0
    x1 <- f y
    case x1 of
      -- TODO: optimize for no return x
      Just x  -> runM nt (return x) f a'
      Nothing -> return a'

-- | Run an 'Auto'' "interactively".  Every step grab a string from stdin,
-- and feed it to the 'Interval''.  If the 'Interval'' is "off", ends the
-- session; if it is "on", then prints the output value to stdout and
-- repeat all over again.
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
-- find something non-readable; there's also 'interactRS' which handles
-- most of that for you.
--
-- Outputs the final 'Interval'' when the interaction terminates.
interactAuto :: Interval' String String         -- ^ 'Interval'' to run interactively
             -> IO (Interval' String String)    -- ^ final 'Interval'' after it all
interactAuto = interactM (return . runIdentity) f
  where
    f (Just str) = True <$ putStrLn str
    f Nothing    = return False

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
-- The 'Auto' can any @'Maybe' b@; you have to provide
-- a function to "handle" it yourself; a @b -> 'IO' 'Bool'@.  You can print
-- the result, or write the result to a file, etc.; the 'Bool' parameter
-- determines whether or not to "continue running", or to stop and return
-- the final updated 'Auto'.
interactM :: Monad m
          => (forall c. m c -> IO c) -- ^ natural transformation from the underlying 'Monad' of the 'Auto' to 'IO'
          -> (b -> IO Bool)          -- ^ function to "handle" each succesful 'Auto' output
          -> Auto m String b         -- ^ 'Auto' to run "interactively"
          -> IO (Auto m String b)    -- ^ final 'Auto' after it all
interactM nt f = runM nt getLine f'
  where
    f' y = do
      cont <- f y
      if cont
        then Just <$> getLine
        else return Nothing


-- | Turn an 'Auto' that takes a "readable" @a@ and outputs a @b@ into an
-- 'Auto' that takes a 'String' and outputs a @'Maybe' b@.  When the
-- 'String' is successfuly readable as the @a@, it steps the 'Auto' and
-- outputs a succesful 'Just' result; when it isn't, it outputs a 'Nothing'
-- on that step.
--
-- >>> let a0 = duringRead (accum (+) (0 :: Int))
-- >>> let (y1, a1) = stepAuto' a0 "12"
-- >>> y1
-- Just 12
-- >>> let (y2, a2) = stepAuto' a1 "orange"
-- >>> y2
-- Nothing
-- >>> let (y3, _ ) = stepAuto' a2 "4"
-- >>> y3
-- Just 16
--
-- See 'interact' for neat use cases.
duringRead :: (Monad m, Read a)
           => Auto m a b                -- ^ 'Auto' taking in a readable @a@, outputting @b@
           -> Interval m String b       -- ^ 'Auto' taking in 'String', outputting @'Maybe' b@
duringRead = lmap readMaybe . during

-- | Like 'duringRead', but the original 'Auto' would output a @'Maybe' b@
-- instead of a @b@.  Returns 'Nothing' if either the 'String' fails to
-- parse or if the original 'Auto' returned 'Nothing'; returns 'Just' if
-- the 'String' parses and the original 'Auto' returned 'Just'.
--
-- See 'interact' for neat use cases.
bindRead :: (Monad m, Read a)
         => Interval m a b        -- ^ 'Auto' taking in a readable @a@, outputting @'Maybe' b@
         -> Interval m String b   -- ^ 'Auto' taking in 'String', outputting @'Maybe' b@
bindRead = lmap readMaybe . bindI

-- | A generalized version of 'runOnChan' that can run on any @'Auto' m@;
-- all that is required is a natural transformation from the underyling
-- 'Monad' @m@ to 'IO'.
runOnChanM :: Monad m
           => (forall c. m c -> IO c) -- ^ natural transformation from the
                                      --     underling 'Monad' of the
                                      --     'Auto' to 'IO'
           -> (b -> IO Bool)          -- ^ function to "handle" each
                                      --     succesful 'Auto' output;
                                      --     result is whether or not to
                                      --     continue.
           -> Chan a                  -- ^ 'Chan' queue to pull input from.
           -> Auto m a b              -- ^ 'Auto' to run
           -> IO (Auto m a b)         -- ^ final 'Auto' after it all, when
                                      --     the handle resturns 'False'
runOnChanM nt f chan = go
  where
    go a0 = do
      x       <- readChan chan
      (y, a1) <- nt $ stepAuto a0 x
      cont <- f y
      if cont
        then go a1
        else return a1

-- | Runs the 'Auto'' in IO with inputs read from a 'Chan' queue, from
-- "Control.Concurrency.Chan".  It'll block until the 'Chan' has a new
-- input, run the 'Auto' with the received input, process the output with
-- the given handling function, and start over if the handling function
-- returns 'True'.
runOnChan :: (b -> IO Bool)           -- ^ function to "handle" each
                                      --     succesful 'Auto' output;
                                      --     result is whether or not to
                                      --     continue.
           -> Chan a                  -- ^ 'Chan' queue to pull input from.
           -> Auto' a b               -- ^ 'Auto'' to run
           -> IO (Auto' a b)          -- ^ final 'Auto' after it all, when
                                      --     the handle resturns 'False'
runOnChan = runOnChanM (return . runIdentity)
