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
-- semantic concept of "intervals": an 'Auto' whose output stream is "on"
-- or "off" for (conceputally) contiguous chunks of time.
--

module Control.Auto.Interval (
  -- * Intervals
  -- $intervals
    Interval
  , Interval'
  -- * Static 'Interval's
  , off
  , toOn
  , fromInterval
  , fromIntervalWith
  , onFor
  , offFor
  , window
  -- * Filter 'Interval's
  , whenI
  , unlessI
  -- * Choice
  , (<|!>)
  , (<|?>)
  , chooseInterval
  , choose
  -- * Blip-based 'Interval's
  , after
  , before
  , between
  , hold
  , hold_
  , holdFor
  , holdFor_
  -- * Composition with 'Interval'
  , during
  , compI
  , bindI
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Category
import Control.Monad              (join)
import Data.Maybe
import Data.Profunctor
import Data.Serialize
import Prelude hiding             ((.), id, mapM)

-- $intervals
--
-- An auto that exhibits this "interval" behavior is represented with the
-- 'Interval' type synonym:
--
-- @
-- type 'Interval' m a b = 'Auto' m a ('Maybe' b)
-- type 'Interval'' a b  = 'Auto'' a ('Maybe' b)
-- @
--
-- So, the compiler sees an @'Interval' m a b@ as if it were an @'Auto'
-- m a ('Maybe' b)@.  If it helps you reason about type signatures and type
-- inference, you can make the substitution in your head too!
--
-- An @'Interval' m a b@ takes an input stream of @a@s and output stream of
-- @b@s that are "on" and "off" for chunks at a time; 'Nothing' is
-- interpreted as "off", and @'Just' x@ is interpreted as "on" with a value
-- of @x@.
--
-- A classic example is @'onFor' :: 'Int' -> 'Interval' m a a@.  With
-- @'onFor' n@, the output stream behaves exactly like the input stream for
-- the first @n@ steps, then is "off" forever after:
--
-- >>> streamAuto' (onFor 3) [1..7]
-- [Just 1, Just 2, Just 3, Nothing, Nothing, Nothing, Nothing]
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
-- >>> take 8 . streamAuto' a1 $ repeat ()
-- [0, 0, 0, 1, 2, 3, 4, 5]
--
-- (Recall that @'pure' x@ is the 'Auto' that ignores the input stream and
-- gives an output stream of constant @x@s)
--
-- Or in reverse, an 'Auto' that behaves like 'count' until the count is
-- above 3, then switches to @'pure' 0@
--
-- >>> let a2 = (whenI (<= 3) . count) --> pure 0
-- >>> take 8 . streamAuto' a2 $ repeat ()
-- [1, 2, 3, 0, 0, 0, 0, 0]
--
-- That's just a small example using one switching combinator, '-->'.  But
-- hopefully it demonstrates that one powerful motivation behind
-- "intervals" being a "thing" is because of how it works with switches.
--
-- Another neat motivation is that intervals work pretty well with the
-- 'Blip' semantic tool, as well.
--
-- The following 'Interval' will be "off" and suppress all of its input
-- (from 'count') /until/ the 'Blip' stream produced by @'inB' 3@ emits
-- something, then it'll allow 'count' to pass.
--
-- >>> let a3 = after . (count &&& inB 3)
-- >>> let a3 = proc () -> do
--             c   <- count -< ()
--             blp <- inB 3 -< ()
--             after -< (c, blp)
-- >>> take 5 . streamAuto' a3 $ repeat ()
-- [Nothing, Nothing, Just 3, Just 4, Just 4]
--
-- Intervals are also used for things that want their 'Auto's to "signal"
-- when they are "off".  'Interval' is the universal language for, "you can
-- be done with me", when it is needed.  For example, the 'interactAuto'
-- loop takes an 'Interval String String', and "turns off" on the first
-- 'Nothing' or "off" value.  'gather' keeps a collection of 'Interval's,
-- and removes them whenever they output a 'Nothing'/turn "off".
--
-- == The Contract
--
-- So, why have an 'Interval' type, and not always just use 'Auto'?
--
-- You can say that, if you are given an 'Interval', then it comes with
-- a "contract" (by documentation) that the 'Auto' will obey /interval
-- semantics/.
--
-- @'Auto' m a ('Maybe' b)@ can mean a lot of things and represent a lot of
-- things.
--
-- However, if you offer something of an 'Interval' type, or if you find
-- something of an 'Interval' type, it comes with some sort of assurance
-- that that 'Auto' will /behave/ like an interval: on and off for
-- contiguous periods of time.
--
-- In addition, this allows us to further clarify /what our functions
-- expect/.  By saying that a function expects an 'Interval':
--
-- @
--     chooseInterval :: [Interval m a b]
--                    -> Interval m a b
-- @
--
-- 'chooseInterval' has the ability to "state" that it /expects/ things
-- that follow interval semantics in order to "function" properly and in
-- order to properly "return" an 'Interval'.
--
-- Of course, this is not enforced by the compiler.  However, it's useful
-- to create a way to clearly state that what you are offering or what you
-- are expecting does indeed follow this useful pattern.
--
-- == Combinators
--
-- === Converting back into normal streams
--
-- You can take an incoming interval stream and output a "normal"
-- "always-on" stream by using the 'fromInterval' and 'fromIntervalWith'
-- 'Auto's, analogous to 'fromMaybe' and 'maybe' from "Data.Maybe",
-- respectively:
--
-- >>> let a = fromIntervalWith "off" show . onFor 2
-- >>> streamAuto' a [1..5]
-- ["1", "2", "off", "off", "off"]
--
-- You can also use '<|!>', coming up next....
--
-- === Choice
--
-- You can "choose" between interval streams, with choice combinators like
-- '<|?>' and '<|!>'.
--
-- >>> let a = onFor 2 . pure "hello"
--        <|!> onFor 4 . pure "world"
--        <|!> pure "goodbye!"
-- >>> take 6 . streamAuto' a $ repeat ()
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
-- Another tool that makes 'Interval's powerful is the ability to compose
-- them.
--
-- If you have an @'Auto' m a b@ and an @'Auto' m b c@, then you can
-- compose them with '.'.
--
-- If you have an @'Auto' m a b@ and an @'Interval' m b c@, then you can
-- compose them by throwing in a 'toOn' in the chain, or @'fmap' 'Just'@:
--
-- @
-- a               :: 'Auto' m a b
-- i               :: 'Interval' m b c
-- i . 'toOn' . a    :: 'Interval' m a c
-- 'fmap' 'Just' a :: 'Interval' m a b
-- i . 'fmap' 'Just' a :: 'Interval' m a c
-- @
--
-- If you have an @'Interval' m a b@ and an @'Auto' m b c@, you can "lift"
-- the second 'Auto' to be an 'Auto' that only "acts" on "on"/'Just'
-- outputs of the 'Interval':
--
-- @
--     i            :: 'Interval' m a b
--     a            :: 'Auto' m b c
--     'during' a     :: 'Auto' m ('Maybe' a) ('Maybe' b)
--     'during' a . i :: 'Interval' m a c
-- @
--
-- Finally, the kleisli composition: if you have an @'Interval' m a b@ and
-- an @'Interval' m b c@, you can use 'compI': (or also 'bindI')
--
-- @
--     i1            :: 'Interval' m a b
--     i2            :: 'Interval' m b c
--     i2 `'compI'` i1 :: 'Interval' m a b c
--     'bindI' i2 . i1 :: 'Interval' m a b c
-- @
--
-- >>> let a1        = when (< 5) `compI` offFor 2
-- >>> streamAuto' a1 [1..6]
-- [Nothing, Nothing, Just 3, Just 4, Nothing, Nothing]
--
-- The implementation works so that any "on"/'Just' inputs will step the
-- lifted 'Auto' like normal, with the contents of the 'Just', and any
-- "off"/'Nothing' inputs cause the lifted 'Auto' to be skipped.
--
-- 'compI' adds a lot of power to 'Interval' because now you can always
-- work "with 'Interval's", bind them just like normal 'Auto's, and then
-- finally "exit" them after composing and combining many.
--
-- == Warning: Switching
--
-- Note that when any of these combinators "block" (or "inhibit" or
-- "suppress", whatever you call it) their input as a part of a composition
-- pipeline (as in for 'off', 'onFor', 'offFor', etc.), the /input/ 'Auto's
-- are /still stepped/ and "run".  If the inputs had any monad effects,
-- they would too be executed at every step.  In order to "freeze" and not
-- run or step an 'Auto' at all, you have to use switches.
--

infixr 3 <|?>
infixr 3 <|!>
infixr 1 `compI`

-- | Represents a relationship between an input and an output, where the
-- output can be "on" or "off" (using 'Just' and 'Nothing') for contiguous
-- chunks of time.
--
-- "Just" a type alias for @'Auto' m a ('Maybe' b)@.  If you ended up here
-- with a link...no worries!  If you see @'Interval' m a b@, just think
-- @'Auto' m a ('Maybe' b)@ for type inference/type checking purposes.
--
-- If you see something of type 'Interval', you can rest assured that it
-- has "interval semantics" --- it is on and off for meaningfully
-- contiguous chunks of time, instead of just on and off willy nilly.  If
-- you have a function that expects an 'Interval', then the function
-- expects its argument to behave in this way.
--
type Interval m a b = Auto m a (Maybe b)

-- | 'Interval', specialized with 'Identity' as its underlying 'Monad'.
-- Analogous to 'Auto'' for 'Auto'.
type Interval'  a b = Auto'  a (Maybe b)

-- | The output stream is alwayas off, regardless of the input.
--
-- Note that any monadic effects of the input 'Auto' when composed with
-- 'off' are still executed, even though their result value is suppressed.
--
-- prop> off == pure Nothing
off :: Interval m a b
off = mkConst Nothing

-- | The output stream is always on, with exactly the value of the
-- corresponding input.
--
-- prop> toOn == arr Just
toOn :: Interval m a a
toOn = mkFunc Just

-- | An "interval collapsing" 'Auto'.  A stream of on/off values comes in;
-- the output is the value of the input when the input is on, and the
-- "default value" when the input is off.
--
-- Much like 'fromMaybe' from "Data.Maybe".
--
-- prop> fromInterval d = arr (fromMaybe d)
fromInterval :: a       -- ^ value to output for "off" periods
             -> Auto m (Maybe a) a
fromInterval d = mkFunc (fromMaybe d)

-- | An "interval collapsing" 'Auto'.  A stream of on/off values comes in;
-- when the input is off, the output is the "default value".  When the
-- input is off, the output is the given function applied to the "on"
-- value.
--
-- Much like 'maybe' from "Data.Maybe".
--
-- prop> fromIntervalWith d f = arr (maybe d f)
fromIntervalWith :: b             -- ^ default value, when input is off
                 -> (a -> b)      -- ^ function to apply when input is on
                 -> Auto m (Maybe a) b
fromIntervalWith d f = mkFunc (maybe d f)

-- | For @'onFor' n@, the first @n@ items in the output stream are always
-- "on" (passing through with exactly the value of the corresponding
-- input); for the rest, the output stream is always "off", suppressing all
-- input values forevermore.
--
-- If a number less than 0 is passed, 0 is used.
--
onFor :: Int      -- ^ amount of steps to stay "on" for
      -> Interval m a a
onFor = mkState f . Just . max 0
  where
    f x (Just i) | i > 0 = (Just x , Just (i - 1))
    f _ _        = (Nothing, Nothing)

-- | For @'offFor' n@, the first @n@ items in the output stream are always
-- "off", suppressing all input; for the rest, the output stream is always
-- "on", outputting exactly the value of the corresponding input.
offFor :: Int     -- ^ amount of steps to be "off" for.
       -> Interval m a a
offFor = mkState f . Just . max 0
  where
    f _ (Just i) | i > 0 = (Nothing, Just (i - 1))
    f x _                = (Just x , Nothing     )

-- | A combination of 'onFor' and 'offFor'; for @'window' b e@, the output
-- stream will be "on" from item @b@ to item @e@ inclusive with the value
-- of the corresponding input; for all other times, the output stream is
-- always off, suppressing any input.
window :: Int     -- ^ start of window
       -> Int     -- ^ end of window (inclusive)
       -> Interval m a a
window b e = mkState f (Just 1)
  where
    f _ Nothing              = (Nothing, Nothing)
    f x (Just i) | i > e     = (Nothing, Nothing)
                 | i < b     = (Nothing, Just (i + 1))
                 | otherwise = (Just x , Just (i + 1))

-- | The output is "on" with exactly the value of he corresponding input
-- when the input passes the predicate, and is "off" otherwise.
--
-- >>> let a = whenI (\x -> x >= 2 && x <= 4)
-- >>> streamAuto' a [1..6]
-- [Nothing, Just 2, Just 3, Just 4, Nothing, Nothing]
--
-- Careful when using this; you could exactly create an 'Interval' that
-- "breaks" "interval semantics"; for example, 'whenI even', when you know
-- your input stream does not consist of chunks of even numbers and odd
-- numbers at a time.
--
whenI :: (a -> Bool)   -- ^ interval predicate
     -> Interval m a a
whenI p = mkFunc f
  where
    f x | p x       = Just x
        | otherwise = Nothing

-- | Like 'whenI', but only allows values to pass whenever the input does
-- not satisfy the predicate.  Blocks whenever the predicate is true.
--
-- >>> let a = unlessI (\x -> x < 2 &&& x > 4)
-- >>> steamAuto' a [1..6]
-- >>> res
-- [Nothing, Just 2, Just 3, Just 4, Nothing, Nothing]
--
unlessI :: (a -> Bool)   -- ^ interval predicate
       -> Interval m a a
unlessI p = mkFunc f
  where
    f x | p x       = Nothing
        | otherwise = Just x

-- | Takes two input streams --- a stream of normal values, and a blip
-- stream.  Before the first emitted value of the input blip stream, the
-- output is always "off", suppressing all inputs.  /After/ the first
-- emitted value of the input blip stream, the output is always "on" with
-- the corresponding value of the first input stream.
--
-- >>> let a = after . (count &&& inB 3)
-- >>> take 6 . streamAuto' a $ repeat ()
-- >>> res
-- [Nothing, Nothing, Just 3, Just 4, Just 5, Just 6]
--
-- ('count' is the 'Auto' that ignores its input and outputs the current
-- step count at every step, and @'inB' 3@ is the 'Auto' generating
-- a 'Blip' stream that emits at the third step.)
--
-- Be careful to remember that in the above example, 'count' is still "run"
-- at every step, and is progressed (and if it were an 'Auto' with monadic
-- effects, they would still be executed).  It just isn't allowed to pass
-- its output values through 'after' until the 'Blip' stream emits.
--
after :: Interval m (a, Blip b) a
after = mkState f False
  where
    f (x, _     ) True  = (Just x , True )
    f (x, Blip _) False = (Just x , True )
    f _           False = (Nothing, False)

-- | Takes two input streams --- a stream of normal values, and a blip
-- stream.  Before the first emitted value of the input blip stream, the
-- output is always "on" with the corresponding value of the first input
-- stream.  /After/ the first emitted value of the input blip stream, the
-- output will be "off" forever, suppressing all input.
--
-- >>> let a = before . (count &&& inB 3)
-- >>> take 5 . streamAuto' a $ repeat ()
-- >>> res
-- [Just 1, Just 2, Nothing, Nothing, Nothing]
--
-- ('count' is the 'Auto' that ignores its input and outputs the current
-- step count at every step, and @'inB' 3@ is the 'Auto' generating
-- a 'Blip' stream that emits at the third step.)
--
-- Be careful to remember that in the above example, 'count' is still "run"
-- at every step, and is progressed (and if it were an 'Auto' with monadic
-- effects, they would still be executed).  It just isn't allowed to pass
-- its output values through 'before' after the 'Blip' stream emits.
--
before :: Interval m (a, Blip b) a
before = mkState f False
  where
    f _           True  = (Nothing, True )
    f (_, Blip _) False = (Nothing, True )
    f (x, _     ) False = (Just x , False)

-- | Takes three input streams: a stream of normal values, a blip stream of
-- "turning-on" blips, and a blip stream of "turning-off" blips.  After the
-- first blip stream emits, the output will switch to "on" with the value
-- of the first input stream.  After the second blip stream emits, the
-- output will switch to "off", supressing all inputs.  An emission from
-- the first stream toggles this "on"; an emission from the second stream
-- toggles this "off".
--
-- >>> let a        = between . (count &&& (inB 3 &&& inB 5))
-- >>> take 7 . streamAuto' a $ repeat ()
-- [Nothing, Nothing, Just 3, Just 4, Nothing, Nothing, Nothing]
between :: Interval m (a, (Blip b, Blip c)) a
between = mkState f False
  where
    f (_, (_, Blip _)) _     = (Nothing, False)
    f (x, (Blip _, _)) _     = (Just x , True )
    f (x, _          ) True  = (Just x , True )
    f _                False = (Nothing, False)

-- | The output is constantly "on" with the last emitted value of the input
-- blip stream.  However, before the first emitted value, it is "off".
-- value of the input blip stream.  From then on, the output is always the
-- last emitted value
--
-- >>> let a = hold . inB 3
-- >>> streamAuto' a [1..5]
-- [Nothing, Nothing, Just 3, Just 3, Just 3]
--
-- If you want an @'Auto' m ('Blip' a) a@ (no 'Nothing'...just a "default
-- value" before everything else), then you can use 'holdWith' from
-- "Control.Auto.Blip"...or also just 'hold' with '<|!>' or 'fromInterval'.
hold :: Serialize a
     => Interval m (Blip a) a
hold = accum f Nothing
  where
    f x = blip x Just

-- | The non-serializing/non-resuming version of 'hold'.
hold_ :: Interval m (Blip a) a
hold_ = accum_ f Nothing
  where
    f x = blip x Just

-- | For @'holdFor' n@, The output is only "on" if there was an emitted
-- value from the input blip stream in the last @n@ steps.  Otherwise, is
-- off.
--
-- Like 'hold', but it only "holds" the last emitted value for the given
-- number of steps.
--
-- >>> let a = holdFor 2 . inB 3
-- >>> streamAuto' 7 a [1..7]
-- >>> res
-- [Nothing, Nothing, Just 3, Just 3, Nothing, Nothing, Nothing]
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

-- | Forks a common input stream between the two 'Interval's and returns,
-- itself, an 'Interval'.  If the output of the first one is "on", the
-- whole thing is on with that output. Otherwise, the output is exactly
-- that of the second one.
--
-- >>> let a = (onFor 2 . pure "hello") <|?> (onFor 4 . pure "world")
-- >>> take 5 . streamAuto' a $ repeat ()
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

-- | Forks a common input stream between an 'Interval' and an 'Auto', and
-- returns, itself, a normal non-interval 'Auto'..  If the
-- output of the first one is "on", the output of the whole thing is that
-- "on" value.  Otherwise, the output is exactly that of the second one.
--
-- >>> let a1 = (onFor 2 . pure "hello") <|!> pure "world"
-- >>> take 5 . streamAuto' a1 $ repeat ()
-- ["hello", "hello", "world", "world", "world"]
--
-- This one is neat because it associates from the right, so it can be
-- "chained":
--
-- >>> let a2 = onFor 2 . pure "hello"
--         <|!> onFor 4 . pure "world"
--         <|!> pure "goodbye!"
-- >>> take 6 . streamAuto' a2 $ repeat ()
-- ["hello", "hello", "world", "world", "goodbye!", "goodbye!"]
--
-- >  a <|!> b <|!> c
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

-- | Forks an input stream between all 'Interval's in the list.  The result
-- is an 'Interval' whose output is "on" when any of the original
-- 'Interval's is on, with the value of the /first/ "on" one.
--
-- prop> chooseInterval == foldr (<|?>) off
chooseInterval :: Monad m
               => [Interval m a b]    -- ^ the 'Auto's to run and
                                      --   choose from
               -> Interval m a b
chooseInterval = foldr (<|?>) (pure Nothing)

-- | Forks an input stream between all 'Interval's in the list, plus
-- a "default 'Auto'.  The output is the value of the first "on"
-- 'Interval'; if there isn't any, the output from the "default 'Auto'" is
-- used.
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
-- It does this by running the 'Auuto' as normal when the input is "on",
-- and freezing it/being "off" when the input is /off/.
--
-- >>> let a1 = during (sumFrom 0) . onFor 2 . pure 1
-- >>> take 5 . streamAuto' a1 $ repeat ()
-- [Just 1, Just 2, Nothing, Nothing, Nothing]
--
-- >>> let a2 = during (sumFrom 0) . offFor 2 . pure 1
-- >>> take 5 . streamAuto' a2 $ repeat ()
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
-- >>> take 5 . streamAuto' a3 $ repeat ()
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
during :: Monad m
       => Auto m a b      -- ^ 'Auto' to lift to work over intervals
       -> Auto m (Maybe a) (Maybe b)
during = dimap to from . right
  where
    from = either (const Nothing) Just
    to   = maybe (Left ()) Right

-- | "Lifts" (more technically, "binds") an @'Interval' m a b@ into
-- an @'Interval' m ('Maybe' a) b@.
--
-- Does this by running the 'Auto' as normal when the input is "on", and
-- freezing it/being "off" when the input is /off/.
--
-- It's kind of like 'during', but the resulting @'Maybe' ('Maybe' b))@ is
-- "joined" back into a @'Maybe' b@.
--
-- prop> bindI a == fmap join (during a)
--
-- This is really an alternative formulation of 'compI'; typically, you
-- will be using 'compI' more often, but this form can also be useful (and
-- slightly more general).  Note that:
--
-- prop> bindI f == compI f id
--
-- This combinator allows you to properly "chain" ("bind") together series
-- of inhibiting 'Auto's.  If you have an @'Interval' m a b@ and an
-- @'Interval' m b c@, you can chain them into an @'Interval' m a c@.
--
-- @
-- f             :: 'Interval' m a b
-- g             :: 'Interval' m b c
-- 'bindI' g . f :: 'Interval' m a c
-- @
--
-- (Users of libraries with built-in inhibition semantics like Yampa and
-- netwire might recognize this as the "default" composition in those other
-- libraries)
--
-- See 'compI' for examples of this use case.
--
bindI :: Monad m
      => Interval m a b       -- ^ 'Interval' to bind
      -> Interval m (Maybe a) b
bindI = fmap join . during

-- | Composes two 'Interval's, the same way that '.' composes two 'Auto's:
--
-- @
-- (.)   :: Auto     m b c -> Auto     m a b -> Auto     m a c
-- compI :: Interval m b c -> Interval m a b -> Interval m a c
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
-- >>> let window' start dur = onFor dur `compI` offFor (start - 1)
-- >>> streamAuto' (window' 2 3)
-- [Nothing, Just 2, Just 3, Just 4, Nothing, Nothing]
--
compI :: Monad m
      => Interval m b c   -- ^ compose this 'Interval'...
      -> Interval m a b   -- ^ ...to this one
      -> Interval m a c
compI f g = fmap join (during f) . g
