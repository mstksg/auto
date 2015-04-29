{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Control.Auto.Effects
-- Description : Accessing, executing, and manipulating underyling monadic
--               effects.
-- Copyright   : (c) Justin Le 2015
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
  -- ** From inputs
  , effects
  -- ** On 'Blip's
  , arrMB
  , effectB
  , execB
  -- ** One-time effects
  , cache
  , execOnce
  , cache_
  , execOnce_
  -- * Hoists
  , hoistA
  , generalizeA

  -- * Specific underlying monads
  -- $monads

  -- ** 'ReaderT'
  -- $reader
  , runReaderA
  , sealReader
  , sealReader_
  , readerA
  -- *** Sealing from sources
  , sealReaderMVar
  , sealReaderM

  -- ** 'WriterT'
  -- $writer
  , writerA
  , runWriterA

  -- ** 'StateT'
  -- $state
  , sealState
  , sealState_
  , runStateA
  , stateA
  , accumA

  -- , runMaybeA
  -- , maybeA

  -- ** 'Traversable'
  , runTraversableA

  -- ** 'IO'
  , catchA
  ) where

import Control.Applicative
import Control.Auto.Blip
import Control.Auto.Core
import Control.Auto.Generate
import Control.Category
import Control.Concurrent
import Control.Exception
import Control.Monad hiding       (mapM, mapM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans.State  (StateT(StateT), runStateT)
import Control.Monad.Trans.Writer (WriterT(WriterT), runWriterT)
import Data.Foldable
import Data.Monoid
import Data.Serialize
import Data.Traversable
import Prelude hiding             ((.), id, mapM, mapM_)

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
--
-- One neat trick you can do is that you can "tag on effects" to a normal
-- 'Auto' by using '*>' from "Control.Applicative".  For example:
--
-- >>> let a = arrM print *> sumFrom 0
-- >>> ys <- streamAuto a [1..5]
-- 1                -- IO output
-- 2
-- 3
-- 4
-- 5
-- >>> ys
-- [1,3,6,10,15]    -- the result
--
-- Here, @a@ behaves "just like" @'sumFrom' 0@...except, when you step it,
-- it prints out to stdout as a side-effect.  We just gave automatic
-- stdout logging behavior!
--
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
execB mx = perBlip (arrM $ \x -> mx >> return x)
{-# INLINE execB #-}

-- $monads
--
-- 'Auto's can be run in the context of an underlying monad; this means
-- that, instead of just being a straight-up @[a] -> [b]@, pairing up each
-- @a@ with a @b@, you can actually attach a "context" to the @b@-making
-- process, in order to enrich your streaming logic with things like
-- a global read-only environment, a global sink, or global mutable state.
-- The main benefit is that these things all /compose/ like any other
-- 'Auto'...they compose with '.', you can use 'Applicative', 'Arrow',
-- etc., and they'll combine properly.
--
-- For the most part, a good general philosophy is to only have a "small
-- part" of your program over a monad.  You might have a small region of
-- your program that would benefit from having a global environment, a
-- small region of your program that would benefit from having a sink, or
-- a small program that would benefit from global mutable state.  Exercise
-- good style and write maintainable code by limiting the effectful parts
-- to the bare minimum essential, then using 'runReaderA', 'sealReader',
-- 'runWriterA', 'sealStateA', etc. to "close off" or seal the effects, and
-- use the 'Auto' like a normal one without effects.
--
-- In this section are combinators for working with specific underlying
-- monads...and a little description on how each might be useful.  Remember
-- to use them wisely!  Adding any underlying monad causes the complexity
-- of reasoning with your code to go up (depending on which monad), so make
-- sure that you get a real gain before using these!

-- $reader
-- 'Reader', or 'ReaderT' is probably one of the most useful underlying
-- monads to work with.  Basically, instead of @[a] -> [b]@, you have @[a]
-- -> r -> [b]@.  Generate @b@'s, but with an @r@ parameter you can always
-- access.  In practice, you can use 'Reader' to hide a lot of boilerplate
-- threading, add an extra "side input" channel, or compose 'Auto's with
-- a /static guarantee/ that all 'Auto's composed will use the /same/ @r@
-- environment.
--
-- Using 'effect', you have access to the environment:
--
-- @
-- 'effect' 'ask' :: 'MonadReader' r m => 'Auto' m a r
-- @
--
-- Which is an 'Auto' where the only thing it does is continually output
-- the environment @r@.  You can throw this into any /proc/ block over
-- 'Reader', and you have a way to bring your environment "into scope":
--
-- @
-- env <- 'effect' 'ask' -< ()
-- @
--
--
-- For a use case example, you might have:
--
-- @
-- foo :: Auto m (Int, Database) Bool
-- bar :: Auto m (Bool, Database) Int
-- baz :: Auto m (Bool, Database) String
-- @
--
-- Where every 'Auto' use a @Database@ parameter to do their job...and it
-- only makes sense when all of them are composed under the same
-- @Database@.  You can use normal proc notation:
--
-- @
-- full :: Auto m (Int, Database) String
-- full = proc (inp, db) -> do
--     fo <- foo -< (inp, db)
--     br <- bar -< (fo, db)
--     bz <- baz -< (fo, db)
--     id -< replicate br bz
-- @
--
-- Or, you can put them all under 'Reader' and have the parameters pass
-- implicitly:
--
-- @
-- fullR :: Auto (ReaderT Database m) Int String
-- fullR = proc inp -> do
--     fo <- readerA foo -< inp
--     br <- readerA bar -< fo
--     bz <- readerA baz -< fo
--     id -< replicate br bz
-- @
--
-- You can recover the original behavior of @full@ by using 'runReaderA' to
-- "unroll" the implicit argument:
--
-- @
-- full' :: Auto m (Int, Database) String
-- full' = runReaderA fullR
-- @
--
-- You can also "seal" @fullR@ so that it always runs with the same
-- @Database@ at every step using 'sealReader':
--
-- @
-- fullSealed :: Database -> Auto m Int String
-- fullSealed = sealReader fullR
-- @
--
-- @fullSealed db@ will now assume that @foo@, @bar@, and @baz@ all get the
-- same environment forever when they are stepped/streamed.

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
           => Auto (ReaderT r m) a b    -- ^ 'Auto' run over global environment
           -> Auto m (a, r) b           -- ^ 'Auto' receiving environments
runReaderA a = mkAutoM (runReaderA <$> resumeAuto a)
                       (saveAuto a)
                     $ \(x, r) -> do
                         (y, a') <- runReaderT (stepAuto a x) r
                         return (y, runReaderA a')

-- | Takes an 'Auto' that operates under the context of a read-only
-- environment, an environment value, and turns it into a normal 'Auto'
-- that always "sees" that value when it asks for one.
--
-- >>> let a   = effect ask :: Auto (Reader b) a b
-- >>> let rdr = streamAuto' a [1..5] :: Reader b [b]
-- >>> runReader rdr "hey"
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
-- >>> streamAuto' bar [1..5]
-- ["hey1", "hey2", "hey3", "hey4", "hey5"]
--
-- Note that this version serializes the given @r@ environment, so that
-- every time the 'Auto' is reloaded/resumed, it resumes with the
-- originally given @r@ environment, ignoring whatever @r@ is given to it
-- when trying to resume it.  If this is not the behavior you want, use
-- 'sealReader_'.
--
-- 'Reader' is convenient because it allows you to "chain" and "compose"
-- 'Auto's with a common environment, instead of explicitly passing in
-- values every time.  For a convenient way of generating 'Auto's under
-- 'ReaderT', and also for some motivating examples, see 'readerA' and
-- 'runReaderA'.
--
sealReader :: (Monad m, Serialize r)
           => Auto (ReaderT r m) a b    -- ^ 'Auto' run over 'Reader'
           -> r                         -- ^ the perpetual environment
           -> Auto m a b
sealReader a0 r = go a0
  where
    go a = mkAutoM (sealReader <$> resumeAuto a <*> get)
                   (saveAuto a *> put r)
                 $ \x -> do
                     (y, a') <- runReaderT (stepAuto a x) r
                     return (y, go a')

-- | The non-resuming/non-serializing version of 'sealReader'.  Does not
-- serialize/reload the @r@ environment, so that whenever you "resume" the
-- 'Auto', it uses the new @r@ given when you are trying to resume, instead
-- of loading the originally given one.
--
-- /DOES/ serialize the actual 'Auto'!
sealReader_ :: Monad m
            => Auto (ReaderT r m) a b   -- ^ 'Auto' run over 'Reader'
            -> r                        -- ^ the perpetual environment
            -> Auto m a b
sealReader_ a0 r = go a0
  where
    go a = mkAutoM (go <$> resumeAuto a)
                   (saveAuto a)
                 $ \x -> do
                     (y, a') <- runReaderT (stepAuto a x) r
                     return (y, go a')

-- | Takes an 'Auto' that operates under the context of a read-only
-- environment, an environment value, and turns it into a normal 'Auto'
-- that always gets its environment value by executing an action every step
-- in the underlying monad.
--
-- This can be abused to write unreadable code really fast if you don't use
-- it in a disciplined way.   One possible usage is to query a database in
-- 'IO' (or 'MonadIO') for a value at every step.  If you're using
-- underlying global state, you can use it to query that too, with 'get' or
-- 'gets'.  You could even use 'getLine', maybe, to get the result from
-- standard input at every step.
--
-- One disciplined wrapper around this is 'sealReaderMVar', where the
-- environment at every step comes from reading an 'MVar'.  This can be
-- used to "hot swap" configuration files.
--
sealReaderM :: Monad m
            => Auto (ReaderT r m) a b   -- ^ 'Auto' run over 'Reader'
            -> m r                      -- ^ action to draw new @r@ at every step
            -> Auto m a b
sealReaderM a0 r' = go a0
  where
    go a = mkAutoM (go <$> resumeAuto a)
                   (saveAuto a)
                 $ \x -> do
                     r <- r'
                     (y, a') <- runReaderT (stepAuto a x) r
                     return (y, go a')


-- | Takes an 'Auto' that operates under the context of a read-only
-- environment, an environment value, and turns it into a normal 'Auto'
-- that always gets its environment value from an 'MVar'.
--
-- This allows for "hot swapping" configurations.  If your whole program
-- runs under a configuration data structure as the environment, you can
-- load the configuration data to the 'MVar' and then "hot swap" it out by
-- just changing the value in the 'MVar' from a different thread.
--
-- Note that this will block on every "step" until the 'MVar' is
-- readable/full/has a value, if it does not.
--
-- Basically a disciplined wrapper/usage over 'sealReaderM'.
sealReaderMVar :: MonadIO m
               => Auto (ReaderT r m) a b    -- ^ 'Auto' run over 'Reader'
               -> MVar r                    -- ^ 'MVar' containing an @r@ for every step
               -> Auto m a b
sealReaderMVar a0 mv = sealReaderM a0 $ liftIO (readMVar mv)

-- | Transforms an 'Auto' on two input streams ( a "normal input" stream
-- @a@ and an "environment input stream" @r@) into an 'Auto' on one input
-- stream @a@ with an underlying environment @r@ through a 'Reader' monad.
--
-- Why is this useful?  Well, if you have several 'Auto's that all take in
-- a side @r@ stream, and you want to convey that every single one should
-- get the /same/ @r@ at every step, you can instead have all of them pull
-- from a common underlying global environment.
--
-- Note: Function is the inverse of 'runReaderA':
--
-- @
-- 'readerA' . 'runReaderA' == 'id'
-- 'runReaderA' . 'readerA' == 'id'
-- @
readerA :: Monad m
        => Auto m (a, r) b           -- ^ 'Auto' receiving an environment.
        -> Auto (ReaderT r m) a b    -- ^ 'Auto' run over an environment.
readerA a = mkAutoM (readerA <$> resumeAuto a)
                    (saveAuto a)
                  $ \x -> ReaderT $ \r -> do
                      (y, a') <- stepAuto a (x, r)
                      return (y, readerA a')

-- $writer
-- 'WriterT' gives you a shared "sink" to dump data into.  You can dump in
-- data by using
--
-- @
-- 'arrM' 'tell' :: 'MonadWriter' w m => 'Auto' m w ()
-- 'effect' . 'tell' :: 'MonadWriter' w m => w -> 'Auto' m a ()
-- @
--
-- @
-- foo :: Auto (Writer (Sum Int)) Int Int
-- foo = effect (tell 1) *> effect (tell 1) *> sumFrom 0
-- @
--
-- >>> let fooWriter = streamAuto foo
-- >>> runWriter $ fooWriter [1..10]
-- ([1,3,6,10,15,21,28,36,45,55], Sum 20)
--
-- @foo@ increments an underlying counter twice every time it is stepped;
-- its "result" is just the cumulative sum of the inputs.
--
-- If you have several 'Auto's that all output some "side-channel" value
-- that is just all accumulated at the end, and you want to implicitly
-- accumulate it all, you can just have them all dump into an underlying
-- 'Writer' sink instead of aggregating them explicitly.
--
-- For example:
--
-- @
-- foo :: Auto m Int (Bool, [String])
-- bar :: Auto m Bool (Int, [String])
-- baz :: Auto m Bool (String, [String])
-- @
--
-- Each of these has a "logging output" that should be aggregated all at
-- the end.
--
-- One way you can do this is by using an explicit proc block:
--
-- @
-- full :: Auto m Int (String, [String])
-- full = proc inp -> do
--     x <- sumFrom 0 -< inp
--     (fo, foW) <- foo -< inp + x
--     (br, brW) <- bar -< fo
--     (bz, bzW) <- baz -< fo
--     id -< (replicate br bz, foW <> brW <> bzW)
-- @
--
-- Or, you can handle the extra output implicitly using 'writerA':
--
-- @
-- fullW :: Auto (WriterT [String] m) Int String
-- fullW = proc inp -> do
--     x  <- sumFrom 0   -< inp
--     fo <- writerA foo -< inp + x
--     br <- writerA bar -< fo
--     bz <- writerA baz -< fo
--     id -< replicate br bz
-- @
--
-- Note that @'sumFrom' 0@ still works the same and doesn't interfere,
-- logging nothing.
--
-- You can recover the original @full@ with 'runWriterA', which
-- "unwraps" the underlying 'Writer':
--
-- @
-- full' :: Auto m Int (String, [String])
-- full' = runWriterA fullW
-- @
--

-- | "Unrolls" the underlying @'WriterT' w m@ 'Monad', so that an 'Auto'
-- that takes in a stream of @a@ and outputs a stream of @b@ will now
-- output a stream @(b, w)@, where @w@ is the "new log" of the underlying
-- 'Writer' at every step.
--
-- Examples:
--
-- @
-- foo :: Auto (Writer (Sum Int)) Int Int
-- foo = effect (tell 1) *> effect (tell 1) *> sumFrom 0
-- @
--
-- >>> let fooWriter = streamAuto foo
-- >>> runWriter $ fooWriter [1..10]
-- ([1,3,6,10,15,21,28,36,45,55], Sum 20)
--
-- @foo@ increments an underlying counter twice every time it is stepped;
-- its "result" is just the cumulative sum of the inputs.
--
-- When we "stream" it, we get a @[Int] -> 'Writer' (Sum Int)
-- [Int]@...which we can give an input list and 'runWriter' it, getting
-- a list of outputs and a "final accumulator state" of 10, for stepping it
-- ten times.
--
-- However, if we use 'runWriterA' before streaming it, we get:
--
-- >>> let fooW = runWriterA foo
-- >>> streamAuto' fooW [1..10]
-- [ (1 , Sum 2), (3 , Sum 2), (6 , Sum 2)
-- , (10, Sum 2), (15, Sum 2), (21, Sum 2), -- ...
--
-- Instead of accumulating it between steps, we get to "catch" the 'Writer'
-- output at every individual step.
--
-- We can write and compose our own 'Auto's under 'Writer', using the
-- convenience of a shared accumulator, and then "use them" with other
-- 'Auto's:
--
-- @
-- bar :: Auto' Int Int
-- bar = proc x -> do
--   (y, w) <- runWriterA foo -< x
--   blah <- blah -< w
-- @
--
-- And now you have access to the underlying accumulator of @foo@ to
-- access.  There, @w@ represents the continually updating accumulator
-- under @foo@, and will be different/growing at every "step".
--
-- For a convenient way to /create/ an 'Auto' under 'WriterT', see
-- 'writerA'.
runWriterA :: (Monad m, Monoid w)
           => Auto (WriterT w m) a b
           -> Auto m a (b, w)
runWriterA a = mkAutoM (runWriterA <$> resumeAuto a)
                       (saveAuto a)
                     $ \x -> do
                         ((y, a'), w) <- runWriterT (stepAuto a x)
                         return ((y, w), runWriterA a')


-- | Transforms an 'Auto' on with two output streams (a "normal output
-- stream" @b@, and a "logging output stream" @w@) into an 'Auto' with just
-- one output stream @a@, funneling the logging stream @w@ into an
-- underlying 'WriterT' monad.
--
-- Note: Function is the inverse of 'runWriterA':
--
-- @
-- 'writerA' . 'runWriterA' == 'id'
-- 'runWriterA' . 'writerA' == 'id'
-- @
writerA :: (Monad m, Monoid w)
        => Auto m a (b, w)          -- ^ 'Auto' with a "normal" output
                                    --     stream @b@s and a "logging"
                                    --     stream @w@s
        -> Auto (WriterT w m) a b   -- ^ 'Auto' under an underlying
                                    --     'WriterT', logging @w@s
writerA a = mkAutoM (writerA <$> resumeAuto a)
                    (saveAuto a)
                  $ \x -> WriterT $ do
                      ((y, w), a') <- stepAuto a x
                      return ((y, writerA a'), w)

-- $state
-- An underlying 'StateT' gives you access to a global, mutable state.
--
-- At first this might be seem a little silly.  We went through all this
-- trouble to avoid the headache of global mutable state, and now we add ti
-- back in?
--
-- One nice usage is an underlying entropy generator (you can deal with
-- this more explicitly with 'sealRandom' in
-- "Control.Auto.Process.Random"), or maybe some underlying pool that every
-- 'Auto' shares that would be a big headache to thread manually.
--
-- The main benefit here is that, using tools like 'sealState' and
-- 'runStateA', we can /isolate/ the portion of our program that takes
-- advantage of shared mutable state, and /seal off/ or only give that part
-- access to the state... and nobody else.
--
-- Anyways, it should go without saying that you should think really long
-- and really hard before adding in global state to your program.  It is
-- almost always better to use principles of local statefulness and
-- denotative composition to achieve what you want.  Relying on this
-- construct might lead to very unmaintainable code, and definitely code
-- that is much more difficult to reason with.  I suggest trying to find
-- another solution first in all cases!

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
-- Using 'sealState'!  Write the part of your program that would like
-- shared global state with 'State'...and compose it with the rest as if it
-- doesn't, locking it away!
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
-- For a convenient way of "creating" an 'Auto' under 'StateT' in the first
-- place, see 'stateA'.
--
sealState :: (Monad m, Serialize s)
          => Auto (StateT s m) a b    -- ^ 'Auto' run over 'State'
          -> s                        -- ^ initial state
          -> Auto m a b
sealState a s0 = mkAutoM (sealState <$> resumeAuto a <*> get)
                         (saveAuto a *> put s0)
                       $ \x -> do
                           ((y, a'), s1) <- runStateT (stepAuto a x) s0
                           return (y, sealState a' s1)

-- | The non-resuming/non-serializing version of 'sealState'.
sealState_ :: Monad m
           => Auto (StateT s m) a b   -- ^ 'Auto' run over 'State'
           -> s                       -- ^ initial state
           -> Auto m a b
sealState_ a s0 = mkAutoM (sealState_ <$> resumeAuto a <*> pure s0)
                          (saveAuto a)
                          $ \x -> do
                              ((y, a'), s1) <- runStateT (stepAuto a x) s0
                              return (y, sealState_ a' s1)


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
--
-- For a convenient way to "generate" an 'Auto' 'StateT', see 'stateA'
--
runStateA :: Monad m
          => Auto (StateT s m) a b      -- ^ 'Auto' run over a state transformer
          -> Auto m (a, s) (b, s)       -- ^ 'Auto' whose inputs and outputs are a state transformer
runStateA a = mkAutoM (runStateA <$> resumeAuto a)
                      (saveAuto a)
                    $ \(x, s) -> do
                        ((y, a'), s') <- runStateT (stepAuto a x) s
                        return ((y, s'), runStateA a')


-- | Transforms an 'Auto' with two input streams and two output streams (a
-- "normal" input @a@ output @b@ stream, and a "state transforming"
-- side-stream taking in @s@ and outputting @s@), abstracts away the @s@
-- stream as a modifcation to an underyling 'StateT' monad.  That is, your
-- normal inputs and outputs are now your /only/ inputs and outputs, and
-- your input @s@ comes from the underlying global mutable state, and the
-- output @s@ goes to update the underlying global mutable state.
--
-- For example, you might have a bunch of 'Auto's that interact with
-- a global mutable state:
--
-- @
-- foo :: Auto (StateT Double m) Int Bool
-- bar :: Auto (StateT Double m) Bool Int
-- baz :: Auto (StateT Double m) Bool String
-- @
--
-- Where @foo@, @bar@, and @baz@ all interact with global mutable state.
-- You'd use them like this:
--
-- @
-- full :: Auto (StateT Double m) Int String
-- full = proc inp -> do
--     fo <- foo -< inp
--     br <- bar -< fo
--     bz <- baz -< fo
--     id -< replicae br bz
-- @
--
-- 'stateA' allows you generate a new @Auto@ under 'StateT':
--
-- @
-- thing :: Auto m (Int, Double) (Bool, Double)
-- stateA thing :: Auto (StateT Double m) Int Bool
-- @
--
-- So now the two side-channels are interpreted as working with the global
-- state:
--
-- @
-- full :: Auto (StateT Double m) Int String
-- full = proc inp -> do
--     fo <- foo          -< inp
--     tg <- stateA thing -< inp
--     br <- bar          -< fo || tg
--     bz <- baz          -< fo && tg
--     id -< replicae br bz
-- @
--
-- You can then "seal it all up" in the end with an initial state, that
-- keeps on re-running itself with the resulting state every time:
--
-- @
-- full' :: Double -> Auto m Int String
-- full' = sealState full
-- @
--
-- Admittedly, this is a bit more esoteric and dangerous (programming with
-- global state? what?) than its components 'readerA' and 'writerA';
-- I don't actually recommend you programming with global state unless it
-- really is the best solution to your problem...it tends to encourage
-- imperative code/loops, and "unreasonable" and manageable code.  See
-- documentation for 'sealStateA' for best practices.  Basically every bad
-- thing that comes with global mutable state.  But, this is provided here
-- for sake of completeness with 'readerA' and 'writerA'.
--
-- Note: function is the inverse of 'runstateA'.
--
-- @
-- 'stateA' . 'runStateA' == 'id'
-- 'runStateA' . 'stateA' == 'id'
-- @
stateA :: Monad m
       => Auto m (a, s) (b, s)   -- ^ 'Auto' whose inputs and outputs are a
                                 --     state transformer
       -> Auto (StateT s m) a b  -- ^ 'Auto' run over a state transformer
stateA a = mkAutoM (stateA <$> resumeAuto a)
                   (saveAuto a)
                 $ \x -> StateT $ \s -> do
                     ((y, s'), a') <- stepAuto a (x, s)
                     return ((y, stateA a'), s')

-- | Like 'stateA', but assumes that the output is the modified state.
accumA :: Monad m
       => Auto m (a, s) s   -- ^ 'Auto' taking inputs and states and
                            --     returning updated states
       -> Auto (StateT s m) a s     -- ^ 'Auto' over a state transformer
accumA a = mkAutoM (accumA <$> resumeAuto a)
                   (saveAuto a)
                 $ \x -> StateT $ \s -> do
                     (s', a') <- stepAuto a (x, s)
                     return ((s', accumA a'), s')

-- | "Unrolls" the underlying 'Monad' of an 'Auto' if it happens to be
-- 'Traversable' ('[]', 'Maybe', etc.).
--
-- It can turn, for example, an @'Auto' [] a b@ into an @'Auto'' a [b]@; it
-- collects all of the results together.  Or an @'Auto' 'Maybe' a b@ into
-- an @'Auto'' a ('Maybe' b)@.
--
-- This might be useful if you want to make some sort of "underlying
-- inhibiting" 'Auto' where the entire computation might just end up being
-- 'Nothing' in the end.  With this, you can turn that
-- possibly-catastrophically-failing 'Auto' (with an underlying 'Monad' of
-- 'Maybe') into a normal 'Auto', and use it as a normal 'Auto' in
-- composition with other 'Auto's...returning 'Just' if your computation
-- succeeded.
--
-- @
-- 'runTraversableA' :: 'Auto' 'Maybe' a b -> 'Interval'' a b
-- @
--
-- @
-- foo :: Auto Maybe Int Int
-- foo = arrM $ \x -> if even x then Just (x `div` 2) else Nothing
--
-- bar :: Auto Maybe Int Int
-- bar = arrM Just
-- @
--
-- >>> streamAuto foo [2,4,6,7]
-- Nothing
-- >>> streamAuto' (runTraversableA foo) [2,4,6,7]
-- [Just 1, Just 2, Just 3, Nothing]
-- >>> streamAuto (foo &&& bar) [2,4,6]
-- Just [(1, 2),(2, 4),(3, 6)]
-- >>> streamAuto (foo &&& bar) [2,4,6,7]
-- Nothing
-- >>> streamAuto' (runTraversableA foo <|?> runTraversableA bar) [2,4,6,7]
-- [Just 1, Just 2, Just 3, Just 7]
runTraversableA :: (Monad f, Traversable f)
                => Auto f a b           -- ^ 'Auto' run over traversable structure
                -> Auto m a (f b)       -- ^ 'Auto' returning traversable structure
runTraversableA = go . return
  where
    go a = mkAuto (go <$> mapM resumeAuto a)
                  (mapM_ saveAuto a)
                  $ \x -> let o  = a >>= (`stepAuto` x)
                              y  = liftM fst o
                              a' = liftM snd o
                          in  (y, go a')


-- | Wraps a "try" over an underlying 'IO' monad; if the Auto encounters a
-- runtime exception while trying to "step" itself, it'll output a 'Left'
-- with the 'Exception'.  Otherwise, will output 'left'.
--
-- Note that you have to explicitly specify the type of the exceptions you
-- are catching; see "Control.Exception" documentation for more details.
catchA :: Exception e
       => Auto IO a b               -- ^ Auto over IO, expecting an
                                    --     exception of a secific type.
       -> Auto IO a (Either e b)
catchA a = a_
  where
    a_ = mkAutoM (catchA <$> resumeAuto a)
                 (saveAuto a)
               $ \x -> do
                   eya' <- try $ stepAuto a x
                   case eya' of
                     Right (y, a') -> return (Right y, catchA a')
                     Left e        -> return (Left e , a_)
-- TODO: Possibly look into bringing in some more robust tools from
-- monad-control and other industry established error handling routes?
-- Also, can we modify an underlying monad with implicit catching behavior?
