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
  -- * One-time effects
  , cache
  , execOnce
  , cache_
  , execOnce_
  -- * Manipulating underlying monads
  -- ** "Sealing off" monadic 'Auto's
  , sealState
  , sealState_
  , sealReader
  , sealReader_
  -- ** "Unrolling"/"reifying" monadic 'Auto's
  , runStateA
  , runReaderA
  , runWriterA
  , runTraversableA
  -- ** Hoists
  , hoistA
  , generalizeA
  -- ** Working with IO
  , catchA
  -- ** Constructing monadic 'Auto's from other monads
  , fromState
  , fromState_
  ) where

import Control.Applicative
import Control.Auto.Blip
import Control.Exception
import Control.Auto.Core
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Control.Auto.Generate
import Control.Category
import Control.Monad hiding       (mapM, mapM_)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Monoid
import Control.Monad.Trans.State  (StateT, runStateT)
import Data.Foldable
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
-- This can be extended to sealing 'RandT' from the /MonadRandom/ package
-- as well, as long as you 'hoistA' first with @'StateT' . 'runRandT'@.
--
--
sealState :: (Monad m, Serialize s)
          => Auto (StateT s m) a b
          -> s
          -> Auto m a b
sealState a s0 = mkAutoM (sealState <$> resumeAuto a <*> get)
                         (saveAuto a *> put s0)
                       $ \x -> do
                           ((y, a'), s1) <- runStateT (stepAuto a x) s0
                           return (y, sealState a' s1)

-- | The non-resuming/non-serializing version of 'sealState'.
sealState_ :: Monad m
           => Auto (StateT s m) a b
           -> s
           -> Auto m a b
sealState_ a s0 = mkAutoM (sealState_ <$> resumeAuto a <*> pure s0)
                          (saveAuto a)
                          $ \x -> do
                              ((y, a'), s1) <- runStateT (stepAuto a x) s0
                              return (y, sealState_ a' s1)

-- | Turns an @a -> 'StateT' s m b@ arrow into an @'Auto' m a b@, when
-- given an initial state.  Will continually "run the function", using the
-- state returned from the last run.
fromState :: (Serialize s, Monad m)
          => (a -> StateT s m b)
          -> s
          -> Auto m a b
fromState st = mkStateM (runStateT . st)

-- | Non-seralizing/non-resuming version of 'fromState'.  The state isn't
-- serialized/resumed, so every time the 'Auto' is resumed, it starts over
-- with the given initial state.
fromState_ :: Monad m
           => (a -> StateT s m b)
           -> s
           -> Auto m a b
fromState_ st = mkStateM_ (runStateT . st)

-- | "Unrolls" the underlying @'WriterT' w m@ 'Monad', so that an 'Auto'
-- that takes in a stream of @a@ and outputs a stream of @b@ will now
-- output a stream @(b, w)@, where @w@ is the "new log" of the underlying
-- 'Writer' at every step.
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
runWriterA :: (Monad m, Monoid w)
           => Auto (WriterT w m) a b
           -> Auto m a (b, w)
runWriterA a = mkAutoM (runWriterA <$> resumeAuto a)
                       (saveAuto a)
                     $ \x -> do
                         ((y, a'), w) <- runWriterT (stepAuto a x)
                         return ((y, w), runWriterA a')

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
sealReader :: (Monad m, Serialize r)
           => Auto (ReaderT r m) a b    -- ^ 'Auto' run over 'Reader'
           -> r                         -- ^ the perpetual environment
           -> Auto m a b
sealReader a r = mkAutoM (sealReader <$> resumeAuto a <*> get)
                         (saveAuto a *> put r)
                       $ \x -> do
                           (y, a') <- runReaderT (stepAuto a x) r
                           return (y, sealReader a' r)

-- | The non-resuming/non-serializing version of 'sealReader'.  Does not
-- serialize/reload the @r@ environment, so that whenever you "resume" the
-- 'Auto', it uses the new @r@ given when you are trying to resume, instead
-- of loading the originally given one.
sealReader_ :: Monad m
            => Auto (ReaderT r m) a b   -- ^ 'Auto' run over 'Reader'
            -> r                        -- ^ the perpetual environment
            -> Auto m a b
sealReader_ a r = mkAutoM (sealReader_ <$> resumeAuto a <*> pure r)
                          (saveAuto a)
                        $ \x -> do
                            (y, a') <- runReaderT (stepAuto a x) r
                            return (y, sealReader_ a' r)

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
          => Auto (StateT s m) a b      -- ^ 'Auto' run over a state transformer
          -> Auto m (a, s) (b, s)       -- ^ 'Auto' whose inputs and outputs are a start transformer
runStateA a = mkAutoM (runStateA <$> resumeAuto a)
                      (saveAuto a)
                    $ \(x, s) -> do
                        ((y, a'), s') <- runStateT (stepAuto a x) s
                        return ((y, s'), runStateA a')

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

-- | "Unrolls" the underlying 'Monad' of an 'Auto' if it happens to be
-- 'Traversable' ('[]', 'Maybe', etc.).
--
-- It can turn, for example, an @'Auto' [] a b@ into an @'Auto'' a [b]@; it
-- collects all of the results together.  Or an @'Auto' 'Maybe' a b@ into
-- an @'Auto'' a ('Maybe' b)@.
--
-- This might be useful if you want to make some sort of "underyling
-- inhibiting" 'Auto' where the entire computation might just end up being
-- 'Nothing' in the end.  With this, you can turn that
-- possibly-catastrophically-failing 'Auto' (with an underlying 'Monad' of
-- 'Maybe') into a normal 'Auto', and use it as a normal 'Auto' in
-- composition with other 'Auto's...returning 'Just' if your computation
-- succeeded.
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
--
-- TODO: Possibly look into bringing in some more robust tools from
-- monad-control and other industry established error handling routes?
-- Also, can we modify an underlying monad with implicit cacting behavior?
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
