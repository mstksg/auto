{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Control.Auto.Interval
-- Description : Tools for working with "interval" semantics: "On or off"
--               'Auto's.
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
--
-- This module provides combinators and utilities for working with the
-- semantic concept of "intervals": 'Auto's producing values that can
-- either be "on" or "off"...typically for contiguous chunks at a time.
--

module Control.Auto.Interval (
  -- * Intervals
  -- $intervals
    Interval
  , Interval'
  -- * Static intervals
  , off
  , toOn
  , fromInterval
  , fromIntervalWith
  , onFor
  , offFor
  -- , window
  -- * Filter intervals
  , when
  , unless
  -- * Choice
  , (<|!>)
  , (<|?>)
  , chooseInterval
  , choose
  -- * Blip-based intervals
  , after
  , before
  , between
  , hold
  , hold_
  , holdFor
  , holdFor_
  -- * Composition with intervals
  , during
  , compI
  , bindI
  ) where

import Control.Applicative
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Category
import Control.Monad (join)
import Data.Foldable (asum, foldr)
import Data.Traversable (sequenceA)
import Data.Maybe
import Data.Serialize
import Prelude hiding             ((.), id, mapM, foldr)

-- $intervals
--
-- In practice, and for convenience, this "on or offness" is represented by
-- the very handy common type, 'Maybe'.  An "interval-producing" 'Auto'
-- looks like this:
--
-- @
--     'Auto' m a ('Maybe' b)
-- @
--
-- (In contrast to the "normal", "always-on" 'Auto', @'Auto' m a b@)
--
-- We say that the above 'Auto' takes in a stream of @a@s and produces
-- an "interval stream" of @b@s.  When it outputs a 'Nothing', the stream
-- is interpreted as "off".  When it outputs a 'Just', the stream is
-- interpreted as "on", with the value inside the 'Just'.
--
-- For example, we can look at @'onFor' :: 'Int' -> 'Auto' m a ('Maybe' a)@
-- in action. @'onFor' n@ is "on", and lets all values through, for @n@
-- steps; then it turns "off" forever.
--
-- >>> let a = onFor 2 . count
-- >>> let Output res _ = stepAutoN' 5 a ()
-- >>> res
-- [Just 1, Just 2, Nothing, Nothing, Nothing]
--
-- (Recall that 'count', from "Control.Auto.Time", outputs the current step
-- count)
--
-- @'onFor' 2@ is "on" for two steps, and lets the output of 'count'
-- through; then it is "off", and "blocks" all of the output of 'count'.
--
-- == Motivation
--
-- Intervals happen to particularly useful when used with the various
-- /switching/ combinators from "Control.Auto.Switch".
--
-- You might find it useful to "sequence" 'Auto's such that they "switch"
-- from one to the other, dynamically.  For example, an 'Auto' that acts
-- like @'pure' 0@ for three steps, and then like 'count' for the rest:
--
-- >>> let a1 = (onFor 3 . pure 0) --> count
-- >>> let Output res1 _ = stepAutoN' 8 a1 ()
-- >>> res1
-- [0, 0, 0, 1, 2, 3, 4, 5]
--
-- (Recall that @'pure' x@ is the 'Auto' that ignores its input and always
-- outputs @x@)
--
-- Or in reverse, an 'Auto' that behaves like 'count' until the count is
-- above 3, then switches to @'pure' 0@
--
-- >>> let a2 = (when (<= 3) . count) --> pure 0
-- >>> let Output res2 _ = stepAutoN' 8 a2 ()
-- >>> res2
-- [1, 2, 3, 0, 0, 0, 0, 0]
--
-- That's just a small example using one switching combinator, '-->'.  But
-- hopefully it demonstrates that one powerful motivation behind
-- "intervals" being a "thing" is because of how it works with switches.
--
-- Another neat motivation is that intervals work pretty well with the
-- 'Blip' semantic tool, as well.
--
-- The following 'Auto' will be "off" and suppress all of its input (from
-- 'count') /until/ the 'Blip' stream produced by @'inB' 3@ emits
-- something, then it'll allow 'count' to pass.
--
-- >>> let a3 = after . (count &&& inB 3)
-- >>> let Output res3 _ = stepAutoN' 5 a3 ()
-- >>> res3
-- [Nothing, Nothing, Just 3, Just 4, Just 4]
--
-- == Combinators
--
-- === Converting back into normal streams
--
-- You can convert interval streams back into normal streams by using
-- 'fromInterval' and 'fromIntervalWith', analogous to 'fromMaybe' and
-- 'maybe' from "Data.Maybe", respectively:
--
-- >>> let a = fromIntervalWith "off" show . onFor 2 . count
-- >>> let Output res _ = stepAutoN' 5 a ()
-- >>> res
-- ["1", "2", "off", "off", "off"]
--
-- === Choice
--
-- You can also "choose" between interval streams, with choice combinators
-- like '<|?>' and '<|!>'.
--
-- >>> let a = onFor 2 . pure "hello"
--        <|!> onFor 4 . pure "world"
--        <|!> pure "goodbye!"
-- >>> let Output res _ = stepAutoN' 6 a ()
-- >>> res
-- ["hello", "hello", "world", "world", "goodbye!", "goodbye!"]
--
-- The above could also be written with 'choose':
--
-- >>> let a = choose (pure "goodbye!")
--                    [ onFor 2 . pure "hello"
--                    , onFor 4 . pure "world"
--                    ]
--
-- === Composition
--
-- You can also "compose" together 'Auto's involving intervals.
--
-- If you have an @'Auto' m a b@ and an @'Auto' m b c@, then you can
-- compose them with '.'.  But if you can't compose the second if the first
-- is @'Auto' m a ('Maybe' b)@.
--
-- You can use 'during' to turn an @'Auto' m b c@ into an @'Auto' m ('Maybe' b) ('Maybe' c)@,
-- you can compose it fine!
--
-- If you had an @'Auto' m b ('Maybe' c)@, like 'onFor' and literally
-- almost every 'Auto' provided in this module, you can use 'bindI' to turn
-- it into an @'Auto' m ('Maybe' b) ('Maybe' c)@.
--
-- >>> let a1 = bindI (onFor 4) . offFor 1 . count
-- >>> let Output res1 _ = stepAutoN' 6 a1 ()
-- >>> res1
-- [Nothing, Just 2, Just 3, Just 4, Nothing, Nothing]
--
-- >>> let a2 = bindI (when even) . bindI (onFor 4) . offFor 1 . count
-- >>> let Output res2 _ = stepAutoN' 6 a2 ()
-- >>> res2
-- [Nothing, Just 2, Nothing, Just 4, Nothing, Nothing]
--
-- The implementation works so that any "on"/'Just' inputs will step the
-- lifted 'Auto' like normal, with the contents of the 'Just', and any
-- "off"/'Nothing' inputs cause the lifted 'Auto' to be skipped and frozen.
--
-- == Warning: Switching
--
-- Note that when any of these combinators "block" (or "inhibit" or
-- "suppress", whatever you call it) their input as a part of a composition
-- pipeline (as in for 'off', 'onFor', 'offFor', etc.), the /input/ 'Auto's
-- are /still stepped/ and "run".  If the inputs had any monad effects,
-- they would too be exected at every step.  In order to "freeze" and not
-- run or step an 'Auto' at all, you have to use switches.
--
-- ('during' and 'bindI' are not included in this bunch.)
--
--

infixr 3 <|?>
infixr 3 <|!>
infixr 1 `compI`

type Interval m a b = Auto m a (Maybe b)
type Interval'  a b = Auto'  a (Maybe b)

-- | An 'Auto' that produces an interval that always "off" ('Nothing'),
-- never letting anything pass.
--
-- Note that any monadic effects of the input 'Auto' when composed with
-- 'off' are still executed, even though their result value is suppressed.
--
-- prop> off == arr (const Nothing)
off :: Interval m a b
off = mkConst Nothing

-- | An 'Auto' that takes a value stream and turns it into an "always-on"
-- interval, with that value.  Lets every value pass through.
--
-- prop> toOn == arr Just
toOn :: Interval m a a
toOn = mkFunc Just

-- | An 'Auto' taking in an interval stream and transforming it into
-- a normal value stream, using the given default value whenever the
-- interval is off/blocking.
--
-- prop> fromInterval d = arr (fromMaybe d)
fromInterval :: a       -- ^ value to output for "off" periods
             -> Auto m (Maybe a) a
fromInterval d = mkFunc (fromMaybe d)

-- | An 'Auto' taking in an interval stream and transforming it into
-- a normal value stream, using the given default value whenever the
-- interval is off/blocking, and applying the given function to the input
-- when the interval is on/passing.  Analogous to 'maybe' from "Prelude"
-- and "Data.Maybe".
--
-- prop> fromIntervalWith d f = arr (maybe d f)
fromIntervalWith :: b
                 -> (a -> b)
                 -> Auto m (Maybe a) b
fromIntervalWith d f = mkFunc (maybe d f)

-- | An 'Auto' that behaves like 'toOn' (letting values pass, "on")
-- for the given number of steps, then otherwise is off (suppressing all
-- input values from passing) forevermore.
--
onFor :: Int      -- ^ amount of steps to stay "on" for
      -> Interval m a a
onFor = mkState f . max 0
  where
    f _ 0 = (Nothing, 0    )
    f x i = (Just x , i - 1)

-- | An 'Auto' that is off for the given number of steps, suppressing all
-- input values, then behaves like 'toOn' forevermore, passing through
-- values as "on" values.
offFor :: Int     -- ^ amount of steps to be "off" for.
       -> Interval m a a
offFor = mkState f . max 0
  where
    f x 0 = (Just x , 0    )
    f _ i = (Nothing, i - 1)

-- window :: Int -> Int -> Auto m a (Maybe a)
-- window b e = mkState f (Just 1)
--   where
--     f _ Nothing              = (Nothing, Nothing)
--     f x (Just i) | i > e     = (Nothing, Nothing)
--                  | i < b     = (Nothing, Just (i + 1))
--                  | otherwise = (Just x , Just (i + 1))

-- | An 'Auto' that allows values to pass whenever the input satisfies the
-- predicate...and is off otherwise.
--
-- >>> let a = when (\x -> x >= 2 && x <= 4) . count
-- >>> let Output res _ = stepAutoN' 6 a ()
-- >>> res
-- [Nothing, Just 2, Just 3, Just 4, Nothing, Nothing]
--
-- ('count' is the 'Auto' that ignores its input and outputs the current
-- step count at every step)
--
when :: (a -> Bool)   -- ^ interval predicate
     -> Interval m a a
when p = mkFunc f
  where
    f x | p x       = Just x
        | otherwise = Nothing

-- | Like 'when', but only allows values to pass whenever the input does
-- not satisfy the predicate.  Blocks whenever the predicate is true.
--
-- >>> let a = unless (\x -> x < 2 &&& x > 4) . count
-- >>> let Output res _ = stepAutoN' 6 a ()
-- >>> res
-- [Nothing, Just 2, Just 3, Just 4, Nothing, Nothing]
--
-- ('count' is the 'Auto' that ignores its input and outputs the current
-- step count at every step)
--
unless :: (a -> Bool)   -- ^ interval predicate
       -> Interval m a a
unless p = mkFunc f
  where
    f x | p x       = Nothing
        | otherwise = Just x

-- | Takes in a value stream and a 'Blip' stream.  Doesn't allow any values
-- in at first, until the 'Blip' stream emits.  Then, allows all values
-- through as "on" forevermore.
--
-- >>> let a = after . (count &&& inB 3)
-- >>> let Output res _ = stepAutoN' 5 a ()
-- >>> res
-- [Nothing, Nothing, Just 3, Just 4, Just 4]
--
-- ('count' is the 'Auto' that ignores its input and outputs the current
-- step count at every step, and @'inB' 3@ is the 'Auto' generating
-- a 'Blip' stream that emits at the third step.)
--
-- Be careful to remember that 'after' does not actually "switch" anything.
-- In the above example, 'count' is still "run" at every step, and is
-- progressed (and if it were an 'Auto' with monadic effects, they would
-- still be executed).  It just isn't allowed to pass through 'after' until
-- the 'Blip' stream emits.
--
after :: Interval m (a, Blip b) a
after = mkState f False
  where
    f (x, _     ) True  = (Just x , True )
    f (x, Blip _) False = (Just x , True )
    f _           False = (Nothing, False)

-- | Takes in a value stream and a 'Blip' stream.  Allows all values
-- through, as "on", until the 'Blip' stream emits...then doesn't let
-- anything pass after that.
--
-- >>> let a = before . (count &&& inB 3)
-- >>> let Output res _ = stepAutoN' 5 a ()
-- >>> res
-- [Just 1, Just 2, Nothing, Nothing, Nothing]
--
-- ('count' is the 'Auto' that ignores its input and outputs the current
-- step count at every step, and @'inB' 3@ is the 'Auto' generating
-- a 'Blip' stream that emits at the third step.)
--
-- Be careful to remember that 'before' doesn't actually "switch" anything.
-- In the above example, 'count' is /still/ "run" at every step (and if it
-- were an 'Auto' with monad effects, they would still be executed).  It's
-- just that the values are suppressed.
--
before :: Interval m (a, Blip b) a
before = mkState f False
  where
    f _           True  = (Nothing, True )
    f (_, Blip _) False = (Nothing, True )
    f (x, _     ) False = (Just x , False)

-- | Takes in a value stream and two 'Blip' streams.  Starts off as "off",
-- not letting anything pass.  When the first 'Blip' stream emits, it
-- toggles onto the "on" state and lets everything pass; when the second
-- 'Blip' stream emits, it toggles back onto the "off" state.
--
-- >>> let a = before . (count &&& (inB 3 &&& inB 5))
-- >>> let Output res _ = stepAutoN' 7 a ()
-- >>> res
-- [Nothing, Nothing, Just 3, Just 4, Nothing, Nothing, Nothing]
between :: Interval m (a, (Blip b, Blip c)) a
between = mkState f False
  where
    f (_, (_, Blip _)) _     = (Nothing, False)
    f (x, (Blip _, _)) _     = (Just x , True )
    f (x, _          ) True  = (Just x , True )
    f _                False = (Nothing, False)

-- | Takes in a 'Blip' stream and constantly outputs the last emitted
-- value.  Starts off as 'Nothing'.
--
-- >>> let a1 = hold . inB 3 . count
-- >>> let Output res1 _ = stepAutoN' 5 a1 ()
-- >>> res1
-- [Nothing, Nothing, Just 3, Just 3, Just 3]
--
-- You can make this behave as an @'Auto' m ('Blip' a) b@ (no possible
-- 'Nothing's) by providing a "default" value, to be used when the input
-- stream has not yet emitted, in one of two ways:
--
-- The first, using '<|!>':
--
-- >>> let a2 = (hold . inB 3 . count) <|!> pure 100
-- >>> let Output res2 _ = stepAutoN' 5 a2 ()
-- >>> res2
-- [100, 100, 3, 3, 3]
--
-- The second, using 'fromInterval':
--
-- >>> let a3 = fromInterval 100 . hold . inB 3 . count
-- >>> let Output res3 _ = stepAutoN' 5 a3 ()
-- >>> res3
-- [100, 100, 3, 3, 3]
--
hold :: Serialize a => Interval m (Blip a) a
hold = mkAccum f Nothing
  where
    f x = blip x Just

-- | The non-serializing/non-resuming version of 'hold'.
hold_ :: Interval m (Blip a) a
hold_ = mkAccum_ f Nothing
  where
    f x = blip x Just

-- | Like 'hold', but it only "holds" the last emitted value for the given
-- number of steps.
--
-- >>> let a = holdFor 2 . inB 3 . count
-- >>> let Output res _ = stepAutoN' 7 a ()
-- >>> res
-- [Nothing, Nothing, Just 3, Just 4, Nothing, Nothing, Nothing]
--
holdFor :: Serialize a
        => Int      -- ^ number of steps to hold the last emitted value for
        -> Interval m (Blip a) a
holdFor n = mkState (_holdForF n) (Nothing, max 0 n)

-- | The non-serializing/non-resuming version of 'holdFor'.
holdFor_ :: Int   -- ^ number of steps to hold the last emitted value for
         -> Interval m (Blip a) a
holdFor_ n = mkState_ (_holdForF n) (Nothing, max 0 n)

_holdForF :: Int -> Blip a -> (Maybe a, Int) -> (Maybe a, (Maybe a, Int))
_holdForF n = f   -- n should be >= 0
  where
    f x s = (y, (y, i))
      where
        (y, i) = case (x, s) of
                   (Blip b,  _    ) -> (Just b , n    )
                   (_     , (_, 0)) -> (Nothing, 0    )
                   (_     , (z, j)) -> (z      , j - 1)

-- | This "chooses" between two interval-producing 'Auto's; behaves like
-- the first 'Auto' if it is "on"; otherwise, behaves like the second.
--
-- >>> let a = (onFor 2 . pure "hello") <|?> (onFor 4 . pure "world")
-- >>> let Output res _ = stepAutoN' 5 a ()
-- >>> res
-- [Just "hello", Just "hello", Just "world", Just "world", Nothing]
--
-- You can drop the parentheses, because of precedence; the above could
-- have been written as:
--
-- >>> let a' = onFor 2 . pure "hello" <|?> onFor 4 . pure "world"
--
-- Warning: If your underlying monad produces effects, remember that /both/
-- 'Auto's are run at every step, along with any monadic effects,
-- regardless of whether they are "on" or "off".
--
-- Note that more often than not, '<|!>' is probably more useful.  This
-- is useful only in the case that you really, really want an interval at
-- the end of it all.
--
(<|?>) :: Monad m
       => Interval m a b    -- ^ choice 1
       -> Interval m a b    -- ^ choice 2
       -> Interval m a b
(<|?>) = liftA2 (<|>)

-- | "Chooses" between an interval-producing 'Auto' and an "normal" value,
-- "always on" 'Auto'.  Behaves like the "on" value of the first 'Auto' if
-- it is on; otherwise, behaves like the second.
--
-- >>> let a1 = (onFor 2 . pure "hello") <|!> pure "world"
-- >>> let Output res1 _ = stepAutoN' 5 a1 ()
-- >>> res1
-- ["hello", "hello", "world", "world", "world"]
--
-- This one is neat because it associates from the right, so it can be
-- "chained":
--
-- >>> let a2 = onFor 2 . pure "hello"
--         <|!> onFor 4 . pure "world"
--         <|!> pure "goodbye!"
-- >>> let Output res2 _ = stepAutoN' 6 a2 ()
-- >>> res2
-- ["hello", "hello", "world", "world", "goodbye!", "goodbye!"]
--
-- >  a <|!> b <!|> c
--
-- associates as
--
-- >  a <|!> (b <|!> c)
--
-- So using this, you can "chain" a bunch of choices between intervals, and
-- then at the right-most, "final" one, provide the default behavior.
--
-- Warning: If your underlying monad produces effects, remember that /both/
-- 'Auto's are run at every step, along with any monadic effects,
-- regardless of whether they are "on" or "off".
(<|!>) :: Monad m
       => Interval m a b        -- ^ interval 'Auto'
       -> Auto m a b            -- ^ "normal" 'Auto'
       -> Auto m a b
(<|!>) = liftA2 (flip fromMaybe)

-- | Run all 'Auto's from the same input, and return the behavior of the
-- first one that is not 'Nothing'.  If all are 'Nothing', output
-- 'Nothing'.
--
-- prop> chooseInterval == foldr (<|?>) off
chooseInterval :: Monad m
               => [Interval m a b]    -- ^ the 'Auto's to run and
                                      --   choose from
               -> Interval m a b
chooseInterval = fmap asum . sequenceA

-- | Run all 'Auto's from the same input, and return the behavior of the
-- first one that is not 'Nothing'; if all are 'Nothing', return the
-- behavior of the "default case".
--
-- prop> choose == foldr (<|!>)
choose :: Monad m
       => Auto m a b          -- ^ the 'Auto' to behave like if all
                              --   others are 'Nothing'
       -> [Interval m a b]    -- ^ 'Auto's to run and choose from
       -> Auto m a b
choose = foldr (<|!>)


-- | "Lifts" an @'Auto' m a b@ (transforming @a@s into @b@s) into an
-- @'Auto' m ('Maybe' a) ('Maybe' b)@ (or, @'Interval' m ('Maybe' a) b@,
-- transforming /intervals/ of @a@s into /intervals/ of @b@.
--
-- It does this by "running" the given 'Auto' whenever it receives a 'Just'
-- value, and skipping/pausing it whenever it receives a 'Nothing' value.
--
-- >>> let a1 = during (sumFrom 0) . onFor 2 . pure 1
-- >>> let Output res1 _ = stepAutoN' 5 a1 ()
-- >>> res1
-- [Just 1, Just 2, Nothing, Nothing, Nothing]
-- >>> let a2 = during (sumFrom 0) . offFor 2 . pure 1
-- >>> let Output res2 _ = stepAutoN' 5 a2 ()
-- >>> res2
-- [Nothing, Nothing, Just 1, Just 2, Just 3]
--
-- (Remember that @'pure' x@ is the 'Auto' that ignores its input and
-- constantly just pumps out @x@ at every step)
--
-- Note the difference between putting the 'sumFrom' "after" the
-- 'offFor' in the chain with 'during' (like the previous example)
-- and putting the 'sumFrom' "before":
--
-- >>> let a3 = offFor 2 . sumFrom 0 . pure 1
-- >>> let Output res3 _ = stepAutoN' 5 a3 ()
-- >>> res3
-- [Nothing, Nothing, Just 3, Just 4, Just 5]
--
-- In the first case (with @a2@), the output of @'pure' 1@ was suppressed
-- by 'offFor', and @'during' ('sumFrom' 0)@ was only summing on the times
-- that the 1's were "allowed through"...so it only "starts counting" on
-- the third step.
--
-- In the second case (with @a3@), the output of the @'pure' 1@ is never
-- suppressed, and went straight into the @'sumFrom' 0@.  'sumFrom' is
-- always summing, the entire time.  The final output of that @'sumFrom' 0@
-- is suppressed at the end with @'offFor' 2@.
--
during :: Monad m => Auto m a b -> Interval m (Maybe a) b
during a = a_
  where
    a_ = mkAutoM (during <$> loadAuto a)
                 (saveAuto a)
                 $ \x -> case x of
                           Just x' -> do
                             Output y a' <- stepAuto a x'
                             return (Output (Just y) (during a'))
                           Nothing ->
                             return (Output Nothing  a_         )

-- | "Lifts" (more technically, "binds") an @'Interval' m a b@ into
-- an @'Interval' m ('Maybe' a) b@
--
-- This very important combinator allows you to properly "chain" ("bind")
-- together series of inhibiting 'Auto's.  If you have an
-- @'Interval' m a b@ and an @'Interval' m b c@, you can chain them
-- into an @'Interval' m a c@.
--
-- @
--     f             :: 'Interval' m a b
--     g             :: 'Interval' m b c
--     'bindI' g . f :: 'Interval' m a c
-- @
--
-- (Users of libraries with built-in inhibition semantics like Yampa and
-- netwire might recognize this as the "default" composition in those other
-- libraries)
--
-- The given 'Auto' is "run" only on the 'Just' inputs, and paused on
-- 'Nothing' inputs.
--
-- It's kind of like 'during', but the resulting @'Maybe' ('Maybe' b))@ is
-- "joined" back into a @'Maybe' b@.
--
-- prop> bindI a == fmap join (during a)
--
-- As a contrived example, how about an 'Auto' that only allows values
-- through during a window...between, say, the second and fourth steps:
--
-- >>> let window start finish = bindI (onFor finish) . offFor start
-- >>> let a = window 1 4 . count
-- >>> let Output res _ = stepAutoN' 5 a ()
-- >>> res
-- [Nothing, Just 2, Just 3, Just 4, Nothing, Nothing]
--
-- (Remember that 'count' is the 'Auto' that ignores its input and displays
-- the current step count, starting with 1)
--
bindI :: Monad m => Interval m a b -> Interval m (Maybe a) b
bindI = fmap join . during

-- | Composes two 'Interval's, the same way that '.' composes two 'Auto's:
--
-- @
--     (.)   :: Auto     m b c -> Auto     m a b -> Auto     m a c
--     compI :: Interval m b c -> Interval m a b -> Interval m a c
-- @
--
-- Basically, if any 'Interval' in the chain is "off", then the entire rest
-- of the chain is "skipped", short-circuiting a la 'Maybe'.
--
-- (Users of libraries with built-in inhibition semantics like Yampa and
-- netwire might recognize this as the "default" composition in those other
-- libraries)
--
-- As a contrived example, how about an 'Auto' that only allows values
-- through during a window...between, say, the second and fourth steps:
--
-- >>> let window start finish = onFor finish `compI` offFor start
-- >>> let a = window 1 4 . count
-- >>> let Output res _ = stepAutoN' 5 a ()
-- >>> res
-- [Nothing, Just 2, Just 3, Just 4, Nothing, Nothing]
--
-- (Remember that 'count' is the 'Auto' that ignores its input and displays
-- the current step count, starting with 1)
--
compI :: Monad m => Interval m b c -> Interval m a b -> Interval m a c
compI f g = fmap join (during f) . g

-- _during' :: forall m a b t. (Monad m, Traversable t) => Auto m a b -> Auto m (t a) (t b)
-- _during' a = a_
--   where
--     a_ = mkAutoM (_during' <$> loadAuto a)
--                  (saveAuto a)
--                  f
--     f :: t a -> m (Output m (t a) (t b))
--     f  = liftM m2o . mapM (stepAuto a)
--     m2o :: t (Output m a b) -> Output m (t a) (t b)
--     m2o m  = Output (fmap outRes m) $ if null (toList m)
--                                         then a_
--                                         else (_ . fmap (_during' . outAuto)) m

    -- ff :: Auto m (t a) (t b)
    --    -> Auto m (t a) (t b)
    --    -> Auto m (t a) (t b)
    -- ff a' acc = liftA2 (<|>) a' acc
    -- ff o _ = _during' (outAuto o)
    -- f  = maybe fn fj
    -- fj = liftM (onOutput Just _during') . stepAuto a
    -- fn = return (Output Nothing  a_         )
