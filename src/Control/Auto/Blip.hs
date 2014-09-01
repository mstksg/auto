{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Control.Auto.Blip
-- Description : Tools for generating and manipulating 'Blip' streams.
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
--
-- This module provides tools for generating and manipulating 'Blip'
-- streams.  The 'Blip' abstraction is not fundamental to 'Auto', but
-- rather is a very useful tool for the denotation of many programs, games,
-- simulations, and computations in general that you are likely to write
-- with this library.
--

module Control.Auto.Blip (
  -- $blip
  -- * The Blip type
    Blip
  , blip
  , merge
  , mergeL
  , mergeR
  , emitOn
  , emitJusts
  , onJusts
  , fromBlips
  -- * Composition with Blip streams
  , perBlip
  , bindB
  -- * Step/"time" based Blip streams and generators
  , never
  , immediately
  , inB
  , every
  , eachAt
  , eachAt_
  -- * Modifying Blip streams
  , tagBlips
  , modifyBlips
  , (<&)
  , (&>)
  , once
  , notYet
  , filterB
  , takeB
  , takeWhileB
  , dropB
  , dropWhileB
  -- * Scanning & Accumulating Blip streams
  , accumB
  , accumB_
  , scanB
  , scanB_
  , mscanB
  , mscanB_
  , countB
  -- * Blips on edges
  , onChange
  , onChange_
  , became
  , became_
  , became'
  , noLonger
  , noLonger_
  , noLonger'
  , onFlip
  , onFlip_
  , onFlip'
  ) where

import Control.Applicative
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Category
import Data.Monoid
import Data.Serialize
import Prelude hiding                     ((.), id, sequence)

infixl 5 <&
infixl 5 &>

-- $blip
--

-- | Merges two 'Blip' streams together into one, which emits
-- /either/ of the original 'Blip' streams emit.  If both emit at the same
-- time, the left (first) one is favored.
mergeL :: Blip a -> Blip a -> Blip a
mergeL = merge const

-- | Merges two 'Blip' streams together into one, which emits
-- /either/ of the original 'Blip' streams emit.  If both emit at the same
-- time, the right (second) one is favored.
mergeR :: Blip a -> Blip a -> Blip a
mergeR = merge (flip const)

-- | Takes two 'Auto's generating 'Blip' streams and returns a "merged"
-- 'Auto' that emits when either of the original 'Auto's emit.  When both
-- emit at the same time, the left (first) one is favored.
--
-- prop> a1 <& a2 == mergeL <$> a1 <*> a2
(<&) :: Monad m
     => Auto m a (Blip b)
     -> Auto m a (Blip b)
     -> Auto m a (Blip b)
(<&) = liftA2 (merge const)

-- | Takes two 'Auto's generating 'Blip' streams and returns a "merged"
-- 'Auto' that emits when either of the original 'Auto's emit.  When both
-- emit at the same time, the right (second) one is favored.
--
-- prop> a1 &> a2 == mergeR <$> a1 <*> a2
(&>) :: Monad m
     => Auto m a (Blip b)
     -> Auto m a (Blip b)
     -> Auto m a (Blip b)
(&>) = liftA2 (merge (flip const))


-- | An 'Auto' that ignores its input and produces a 'Blip' stream that
-- never emits.
never :: Auto m a (Blip b)
never = mkConst NoBlip

-- | An 'Auto' that produces a 'Blip' stream that emits on the first ever
-- "tick", with the input at that step.  It never emits again after that.
--
-- Often used with 'pure':
--
-- >>> immediately . pure "Emit me!"
--
-- Or, in proc notation:
--
-- >>> blp <- immediately -< "Emit me!"
--
-- To get a 'Blip' stream that emits a given value (eg., "Emit me!") once
-- and stops emitting ever again.
immediately :: Auto m a (Blip a)
immediately = mkState f False
  where
    f _ True  = (NoBlip, True)
    f x False = (Blip x, True)

-- | An 'Auto' that produces a 'Blip' stream that only emits once, after
-- waiting the given number of ticks.  It emits the input at that step.
--
-- prop> immediately == inB 0
inB :: Int                -- ^ number of steps before value is emitted.
    -> Auto m a (Blip a)
inB n = mkState f (n, False)
  where
    f _ (_, True )             = (NoBlip, (0  , True ))
    f x (i, False) | i <= 0    = (Blip x, (0  , True ))
                   | otherwise = (NoBlip, (i-1, False))

-- | An 'Auto' producing a 'Blip' stream that emits the input whenever the
-- input satisfies a given predicate.
--
-- Warning!  This 'Auto' has the capability of "breaking" 'Blip' semantics.
-- Be sure you know what you are doing when using this.  'Blip' streams are
-- semantically supposed to only emit at discrete, separate occurrences.
-- Do not use this for interval-like (on and off for chunks at a time)
-- things; each input should be dealt with as a separate thing.
--
-- For interval semantics, we have "Control.Auto.Interval".
--
-- Good example:
--
-- > -- is only emitting at discrete blips
-- > emitOn even . iterator (+ 1) 0
--
-- Bad examples:
--
-- > -- is emitting for "durations" or "intervals" of time.
-- > emitOn (< 10) . iterator (+ 1) 0
-- >
-- > emitOn (\_ -> True) . foo
--
emitOn :: (a -> Bool)   -- ^ predicate to emit on
       -> Auto m a (Blip a)
emitOn p = mkFunc $ \x -> if p x then Blip x else NoBlip

-- | An 'Auto' that runs every input through a @a -> 'Maybe' b@ test and
-- produces a blip stream that emits the value inside every 'Just' result.
--
-- A less "boolean-blind" version of 'emitOn'.
--
-- Warning!  Carries all of the same dangers of 'emitOn'.  You can easily
-- break 'Blip' semantics with this if you aren't sure what you are doing.
-- Remember to only emit at discrete, separate occurences, and not for
-- interval-like (on and off for chunks at a time) things.  For interval
-- semantics, we have "Control.Auto.Interval".
--
-- See the examples of 'emitOn' for more concrete good/bad use cases.
emitJusts :: (a -> Maybe b)     -- ^ "predicate" to emit on.
          -> Auto m a (Blip b)
emitJusts p = mkFunc (maybe NoBlip Blip . p)


-- | @'every' n@ is an 'Auto' that takes in a stream of @a@s and produces
-- a blip stream that emits the input @a@ every @n@ steps.  First emission
-- is on the @n@th step.
every :: Int    -- ^ emit every @n@ steps.
      -> Auto m a (Blip a)
every (max 0 -> n) = mkState f n
  where
    f x i | i <= 1    = (Blip x, n    )
          | otherwise = (NoBlip, n - 1)

-- | @'eachAt' n xs@ is an 'Auto' that ignores its input and emits each
-- element of @xs@ one at a time, every @n@ steps.
--
-- Once the list is exhausted, never emits again.
--
-- The process of serializing and resuming this 'Auto' is O(n) space and
-- time with the length of @xs@.  So don't serialize this if you plan on
-- passing an infinite list :)
eachAt :: Serialize b
       => Int   -- ^ emit every @n@ steps
       -> [b]   -- ^ list to emit values from
       -> Auto m a (Blip b)
eachAt (max 1 -> n) xs = mkState (\_ -> _eachAtF n) (n, xs)

-- | The non-serializing/non-resumable version of 'eachAt'.
eachAt_ :: Int    -- ^ emit every @n@ steps
        -> [b]    -- ^ list to emit values from
        -> Auto m a (Blip b)
eachAt_ (max 1 -> n) xs = mkState_ (\_ -> _eachAtF n) (n, xs)

_eachAtF :: Int -> (Int, [b]) -> (Blip b, (Int, [b]))
_eachAtF n (i, xs) = case xs of
                       []               -> (NoBlip, (0    , xs))
                       y:ys | i <= 1    -> (Blip y, (n    , ys))
                            | otherwise -> (NoBlip, (n - 1, xs))

-- | Suppress all emissions when the predicate is false.
filterB :: (a -> Bool)      -- ^ filtering predicate
        -> Auto m (Blip a) (Blip a)
filterB p = mkFunc $ \x -> case x of
                             Blip x' | p x' -> x
                             _              -> NoBlip

-- | Supress all emissions except for the very first.
once :: Auto m (Blip a) (Blip a)
once = mkState f False
  where
    f _          True  = (NoBlip, True )
    f e@(Blip _) False = (e,       True )
    f _          False = (NoBlip, False)

-- | Suppress only the first emission, and ignore the rest.
notYet :: Auto m (Blip a) (Blip a)
notYet = mkState f False
  where
    f e        True  = (e      , True )
    f (Blip _) False = (NoBlip, True )
    f _        False = (NoBlip, False)

-- | @'takeB' n@ allows only the first @n@ emissions to pass.
takeB :: Int    -- ^ number of emissions to allow to pass
      -> Auto m (Blip a) (Blip a)
takeB = mkState f . max 0
  where
    f _ 0          = (NoBlip, 0  )
    f e@(Blip _) i = (e      , i-1)
    f _          i = (NoBlip, i  )

-- | Allow emissions to pass until the first that fails the predicate.
takeWhileB :: (a -> Bool)       -- ^ filtering predicate
           -> Auto m (Blip a) (Blip a)
takeWhileB p = mkState f False
  where
    f _          True        = (NoBlip, True )
    f e@(Blip x) False | p x = (e      , False)
    f _          False       = (NoBlip, True )

-- | @'dropB' n@ suppresses the first @n@ emissions and passes through the
-- rest.
dropB :: Int      -- ^ number of emissions to suppress initially
      -> Auto m (Blip a) (Blip a)
dropB = mkState f . max 0
  where
    f x        0 = (x      , 0  )
    f (Blip _) i = (NoBlip, i-1)
    f _        i = (NoBlip, i  )

-- | Suppress all emissions until the first one satisfying the predicate,
-- then allow the rest through.
dropWhileB :: (a -> Bool)     -- ^ filtering predicate
           -> Auto m (Blip a) (Blip a)
dropWhileB p = mkState f False
  where
    f e          True              = (e      , True )
    f e@(Blip x) False | p x       = (NoBlip, False)
                       | otherwise = (e      , True )
    f _          False             = (NoBlip, False)

-- | Accumulates all emissions in the incoming 'Blip' stream with
-- a "folding function", with a given starting value.  @b -> a -> b@, with
-- a starting @b@, gives @'Auto' m ('Blip' a) ('Blip' b)@.
--
-- The resulting 'Blip' stream will emit every time the input stream emits,
-- but with the "accumulated value".
--
-- Basically 'mkAccum', but on blip stream emissions.
--
-- prop> accumB f x0 == perBlip (mkAccum f x0)
accumB :: Serialize b
       => (b -> a -> b)     -- ^ folding function
       -> b                 -- ^ initial value
       -> Auto m (Blip a) (Blip b)
accumB f = mkState (_accumBF f)

-- | The non-serializing/non-resuming version of 'accumB'.
accumB_ :: (b -> a -> b)    -- ^ folding function
        -> b                -- ^ initial value
        -> Auto m (Blip a) (Blip b)
accumB_ f = mkState_ (_accumBF f)

_accumBF :: (b -> a -> b) -> Blip a -> b -> (Blip b, b)
_accumBF f e y0 = case e of
                    Blip x -> let y1 = f y0 x
                              in  (Blip y1, y1)
                    NoBlip ->     (NoBlip , y0)

-- | At every step, outputs the result of applying the "folding function"
-- with against the contents of every emitted value so far.
--
-- prop> scanB f x0 == (hold . accumB f x0) <|!> pure x0
--
-- >>> let a = scanB (+) 0 . eachAt 2 [1,2,3]
-- >>> let Output res _ = stepAutoN' 8 ()
-- >>> res
-- [0, 1, 1, 3, 3, 6, 6, 6, 6]
scanB :: Serialize b
      => (b -> a -> b)      -- ^ folding function
      -> b                  -- ^ initial value
      -> Auto m (Blip a) b
scanB f = mkAccum (_scanBF f)

-- | The non-serializing/non-resuming version of 'scanB'.
scanB_ :: (b -> a -> b)
       -> b                   -- ^ folding function
       -> Auto m (Blip a) b   -- ^ initial value
scanB_ f = mkAccum_ (_scanBF f)

_scanBF :: (b -> a -> b) -> b -> Blip a -> b
_scanBF f y0 = blip y0 (f y0)

-- | At every step, outputs the monoid sum of all emitted values received
-- so far.
mscanB :: (Monoid a, Serialize a)
       => Auto m (Blip a) a
mscanB = scanB (<>) mempty

-- | The non-serializing/non-resuming version of 'mscanB'.
mscanB_ :: Monoid a
        => Auto m (Blip a) a
mscanB_ = scanB_ (<>) mempty

-- | Outputs the number of emitted values of the input 'Blip' stream so
-- far.
countB :: Auto m (Blip a) Int
countB = mkAccum (\i -> (i +) . blip 0 (const 1)) 0

-- | Produces a blip stream that emits whenever the predicate applied to
-- the input switches from false to true.  Emits with the triggering input
-- value.
became :: Serialize a
       => (a -> Bool)       -- ^ change condition
       -> Auto m a (Blip a)
became p = mkAccum (_becameF p) NoBlip

-- | Produces a blip stream that emits whenever the predicate applied to
-- the input switches from true to false.  Emits with the triggering input
-- value.
noLonger :: Serialize a
         => (a -> Bool)     -- ^ change condition
         -> Auto m a (Blip a)
noLonger p = became (not . p)

-- | Produces a blip stream that emits whenever the predicate applied to
-- the input switches from true to false or false to true.  Emits with the
-- triggering input value.
onFlip :: (Serialize a, Monad m)
       => (a -> Bool)       -- ^ change condition
       -> Auto m a (Blip a)
onFlip p = became p &> noLonger p

-- | The non-serializing/non-resumable version of 'became'.
became_ :: Monad m
        => (a -> Bool)      -- ^ change condition
        -> Auto m a (Blip a)
became_ p = mkAccum_ (_becameF p) NoBlip

-- | The non-serializing/non-resumable version of 'noLonger'.
noLonger_ :: Monad m
          => (a -> Bool)    -- ^ change condition
          -> Auto m a (Blip a)
noLonger_ p = became_ (not . p)

-- | The non-serializing/non-resumable version of 'onFlip'.
onFlip_ :: Monad m
        => (a -> Bool)      -- ^ change condition
        -> Auto m a (Blip a)
onFlip_ p = became_ p &> noLonger_ p

_becameF :: (a -> Bool) -> Blip a -> a -> Blip a
_becameF p e x | p x       = blip (Blip x) (const NoBlip) e
               | otherwise = NoBlip

-- | Like 'became', but emits a '()' instead of the triggering input value.
became' :: Monad m
        => (a -> Bool)        -- ^ change condition
        -> Auto m a (Blip ())
became' p = mkAccum f NoBlip
  where
    f e x | p x       = blip (Blip ()) (const NoBlip) e
          | otherwise = NoBlip

-- | Like 'noLonger', but emits a '()' instead of the triggering input
-- value.
noLonger' :: Monad m
          => (a -> Bool)        -- ^ change condition
          -> Auto m a (Blip ())
noLonger' p = became' (not . p)

-- | Like 'onFlip', but emits a '()' instead of the triggering input value.
onFlip' :: Monad m
        => (a -> Bool)            -- ^ change condition
        -> Auto m a (Blip Bool)
onFlip' p = fmap (True <$) (became' p) &> fmap (False <$) (noLonger' p)

-- | Produces a blip stream that emits whenever the input value changes.
-- Emits with the new value.
onChange :: (Serialize a, Eq a) => Auto m a (Blip a)
onChange = mkState _onChangeF Nothing

-- | The non-serializing/non-resumable version of 'onChange'.
onChange_ :: Eq a => Auto m a (Blip a)
onChange_ = mkState_ _onChangeF Nothing

_onChangeF :: Eq a => a -> Maybe a -> (Blip a, Maybe a)
_onChangeF x Nothing               = (NoBlip , Just x)
_onChangeF x (Just x') | x == x'   = (NoBlip , Just x')
                       | otherwise = (Blip x', Just x')

-- | An 'Auto' that emits whenever it receives a 'Just' input, with the
-- value inside the 'Just'.
--
-- Warning!  Carries all of the same dangers of 'emitOn'.  You can easily
-- break 'Blip' semantics with this if you aren't sure what you are doing.
-- Remember to only emit at discrete, separate occurences, and not for
-- interval-like (on and off for chunks at a time) things.  For interval
-- semantics, we have "Control.Auto.Interval".
--
-- See the examples of 'emitOn' for more concrete good/bad use cases.
onJusts :: Auto m (Maybe a) (Blip a)
onJusts = mkFunc (maybe NoBlip Blip)

-- | @'fromBlips' d@ is an 'Auto' that decomposes the incoming 'Blip'
-- stream by constantly outputting @d@ except when the stream emits, and
-- outputs the emitted value when it does.
fromBlips :: a  -- ^ the "default value" to output when the input is not
                --   emitting.
          -> Auto m (Blip a) a
fromBlips d = mkFunc (blip d id)

-- | Re-emits every emission from the input 'Blip' stream, but replaces its
-- value with the given value.
--
-- prop> tagBlips x == modifyBlips (const x)
tagBlips :: b             -- ^ value to replace every emitted value with
         -> Auto m (Blip a) (Blip b)
tagBlips y = mkFunc (y <$)

-- | Re-emits every emission from the input 'Blip' stream, but applies the
-- given function to the emitted value.
modifyBlips :: (a -> b)     -- ^ function to modify emitted values with
            -> Auto m (Blip a) (Blip b)
modifyBlips f = mkFunc (fmap f)

-- | Takes an @'Auto' m a b@ (an 'Auto' that turns incoming @a@s into
-- outputting @b@s) into an @'Auto' m ('Blip' a) ('Blip' b)@; the original
-- 'Auto' is lifted to only be applied to emitted contents of a 'Blip'
-- stream.
--
-- When the stream emits, the original 'Auto' is "stepped" with the emitted
-- value; when it does not, it is paused and frozen until the next
-- emission.
--
-- >>> let a    = perBlip (sumFrom 0)
-- >>> let blps = eachAt 2 [1,5,2]
-- >>> let Output bres _ = stepAutoN 8 blps ()
-- >>> bres
-- [NoBlip, Blip 1, NoBlip, Blip 5, NoBlip, Blip 2, NoBlip, NoBlip]
-- >>> let Output ares _ = stepAutoN 8 a ()
-- >>> ares
-- [NoBlip, Blip 1, NoBlip, Blip 6, NoBlip, Blip 8, NoBlip, NoBlip]
perBlip :: Monad m => Auto m a b -> Auto m (Blip a) (Blip b)
perBlip a = a_
  where
    a_ = mkAutoM (perBlip <$> loadAuto a)
                 (saveAuto a)
                 $ \x -> case x of
                           Blip x' -> do
                             Output y a' <- stepAuto a x'
                             return (Output (Blip y) (perBlip a'))
                           NoBlip  ->
                             return (Output NoBlip   a_           )

-- -- | Why is this even here
-- perBlipI :: Monad m
--           => Auto m (Maybe a) (Maybe b)
--           -> Auto m (Blip a) (Blip b)
-- perBlipI = dimap (blip Nothing Just) (maybe NoBlip Blip)

-- | Takes an @'Auto' m a ('Blip' b)@ (an 'Auto' that turns incoming @a@s
-- into a 'Blip' stream of @b@s) into an @'Auto' m ('Blip' a) ('Blip' b)@.
-- The original 'Auto' is applied only to emitted contents of a 'Blip'
-- stream.
--
-- It is like 'perBlip', but the resulting @'Blip' ('Blip' b)@ is "joined"
-- back into a @'Blip' b@.
--
-- prop> bindB a == fmap (blip NoBlip id) (perBlip a)
bindB :: Monad m => Auto m a (Blip b) -> Auto m (Blip a) (Blip b)
bindB  = fmap (blip NoBlip id) . perBlip
