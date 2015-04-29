{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Control.Auto.Blip
-- Description : Tools for generating and manipulating blip streams.
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
--
-- This module provides tools for generating and manipulating "blip
-- streams".  The blip stream abstraction is not fundamental to 'Auto', but
-- rather, like /interval/, is a very useful semantic tool for the
-- denotation of many programs, games, simulations, and computations in
-- general that you are likely to write with this library.
--

module Control.Auto.Blip (
  -- * 'Blip'
  -- $blip
  -- * The Blip type
    Blip
  , perBlip
  -- ** Merging
  , merge
  , mergeL
  , mergeR
  , mergeLs
  , mergeRs
  , foldrB
  , foldlB'
  -- ** "Raw" blip stream creation (dangerous!)
  , emitJusts
  , emitEithers
  , onJusts
  , onEithers
  , emitOn
  -- ** Blip stream collapse
  , fromBlips
  , fromBlipsWith
  , holdWith
  , holdWith_
  , substituteB
  , asMaybes
  -- * Step/"time" based Blip streams and generators
  , never
  , immediately
  , inB
  , every
  , eachAt
  , eachAt_
  , collectN
  , collectN_
  -- * Modifying Blip streams
  , tagBlips
  , modifyBlips
  , (<&)
  , (&>)
  , once
  , notYet
  , lagBlips
  , lagBlips_
  , filterB
  , splitB
  , collectB
  , collectB_
  , collectBs
  , collectBs_
  , joinB
  , mapMaybeB
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
import Control.Arrow
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Category
import Data.Monoid
import Data.Profunctor
import Data.List
import Data.Serialize
import Prelude hiding             ((.), id, sequence)

infixr 5 <&
infixl 5 &>

-- $blip
--
-- In the context of inputs/outputs of 'Auto', a @'Blip' a@ represents
-- a "blip stream" that occasionally, in isolated incidents, emits a value
-- of type @a@.
--
-- For example, @'Auto'' a ('Blip' b)@ is an 'Auto'' that a stream of @a@'s
-- as input and outputs a *blip stream* that occasionally emits with a @b@.
-- An @'Auto'' ('Blip' a) b@ is an 'Auto'' that takes a *blip stream* that
-- occasionally emits with a @a@ and outputs a stream of @b@'s.
--
-- If an 'Auto' takes or outputs a "blip stream", it comes with some
-- "semantic" contracts on to how the stream behaves.  The main contract is
-- that your blip stream should only output on (meaningfully) "isolated"
-- incidents, and never on continuous regions of the input stream.
--
-- By this, we mean that every emitted value is (conceptually) emitted
-- "alone", and not as a part of continuous on/off chunks.
--
-- == Example situations
--
-- A good example would be, say, a blip stream that emits every time
-- a user/player sends a certain type of command.  Or a blip stream that
-- emits every time a slowly-moving value crosses over from positive to
-- negative.
--
-- A bad example would be a blip stream that emits when a player /doesn't/
-- send a certain less-common type of command.  Or a blip stream that emits
-- whenever a slowly-moving value /is/ positive or negative.
--
-- == Contrast with /Intervals/
--
-- Blip streams are contrasted with another semantic tool: stream
-- _intervals_, manipulated with "Control.Auto.Interval".  /Intervals/ are
-- adjacent/contiguous "chunks" of on/off behavior, and are on or off for
-- contiguous "chunks" at a time.  So when deciding whether or not you want
-- to use the semantics of blip streams or the semantics of /Interval/,
-- consider: is this behavior going to be "on/off" for chunks at a time
-- (such as an interval that is on whenever a slowly-moving value is
-- positive)?  Or is it something that is usually "not on", but makes
-- separate, isolated, "blips" --- each emitted value alone and
-- (semantically) isolated from the rest.
--
-- == Motivations
--
-- The main motivations of the semantic concept of blip streams (and why they
-- even exist in the first place) is probably for how well they integrate
-- with /Interval/ semantics and, with intervals, the various powerful
-- switching combinators from "Control.Auto.Switch".  Many of the
-- combinators in that module are designed so that switches can be
-- "triggered" by blip stream emissions.
--
-- Blip streams have many usages, as will be explained later.  You'll also
-- find that blip streams work well with their cousins, /interval/ streams.
-- But perhaps the use case that stands out above all (and is alone enough
-- to motivate their existence) is in switching.
--
-- == "Blip semantics"
--
-- We say that a blip stream has "blip semantics" when it is used in
-- a way that its emitted values are "isolated", "alone", "discrete", in
-- this way.  When it is not, we say that the stream "breaks" blip
-- semantics.
--
-- Note that this can't really be enforced by the types, so if you're
-- a library or framework developer, it's up to you to take care that the
-- blip streams you offer all conform to blip semantics.  However, if
-- you're just making an application, you can use most of the combinators
-- in this library/module and not worry.
--
-- Also note that in many of these cases, "blip semantics" depends on how
-- the 'Auto's are /composed/, and what they are composed to.  If the value
-- in question is "almost always" positive and only negative at isolated
-- points in time, then such a "blip stream that emits whenever the value
-- is negative" has proper blip semantics.  If the value in question is
-- slowly-moving and meandering, and might spend a lot of time negative at
-- a time, then the same blip stream would /not/ preserve blip semantics.
--
-- === Why semantics are important
--
-- Why should you care?  I can't tell you want to do, right?
--
-- Well, for the same reason that typeclasses like 'Eq', 'Functor', and
-- 'Monad' have laws.  Yeah, you can make any instance you want that
-- satisfies the types.  But almost all of the usefulness of those
-- typeclasses comes from our ability to "reason" about the behavior of
-- their instances, and to be able to develop an intuition about their
-- usage.  We would be surprised if we had an 'Eq' instance where @x == x@
-- and @x /= x@ are both true...and it would completely break down any
-- attempt at understanding what 'Eq' code "means".
--
-- You can think of "blip semantics" as being the "laws" of blip streams.
-- If we assume that things follow blip semantics properly, then we can
-- reason about them in a unified and useful way.  If we can trust that
-- blip streams actually behave "like blip streams", then blip streams
-- become an extremely useful tool for denoting certain behaviors and
-- programs.
--
-- If we can't...then it becomes a lot less useful :)
--
-- In particular, one big use case for blip streams (the switching
-- mechanisms "Control.Auto.Switch") all only "work well" when your blip
-- streams follow proper semantics.
--
-- === Combinators preserve semantics
--
-- /Most/ of the combinators in this module try their best to preserve blip
-- semantics.  That is, you can't use them in a way that will produce
-- a non-semantic-abiding blip stream.  You can "trust" them, and if you
-- use only safe combinators, you don't ever have to worry.  Well. That
-- much, at least.
--
-- There are a few notable exceptions:
--
-- * 'every', 'eachAt', 'eachAt_', when you pass in an interval of 1.
-- * 'onChange', when the input value isn't ever expected to stay the same
-- between steps.
-- * 'emitOn', 'emitJusts', 'onJusts', in the cases mentioned in the
-- documentation for 'emitOn'.
--
--
-- == Practical examples
--
-- There are many practical examples of using blip streams in the various
-- examples in <https://github.com/mstksg/auto-examples auto-examples>,
-- especially from /chatbot/.  There, blip streams are used in many
-- situations, primarily streams for players sending certain commands. It's
-- also used in /hangman/, to signify player events such as victory,
-- good/bad guesses, etc.
--
-- Blip streams work very closely with the various switching combinators in
-- "Control.Auto.Switch".  If anything, if there is only one reason to use
-- blip streams, it's with the various switching mechanisms in that module.
-- All of the switching combinators rely on the fact that your blip streams
-- follow proper semantics, further emphasizing the importance of
-- conforming to the semantics.
--
-- == For library, framework, and back-end developers
--
-- Remember that this module is only meant to export "safe" combinators
-- that try their best to maintain blip semantics.  Think of this module
-- as a useful guideline to help programmers maintain semantics at
-- compile-time, by only exporting not-as-dangerous combinators.
--
-- However, all of these rules are for the denotation of your /program
-- logic/.  These rules are for the benefit of reasoning about the behavior
-- of your program at the logic level.
--
-- As a library or framework or back-end developer, however, you aren't
-- programming at the logic level, but rather at the gritty implementation
-- level.  So, you might want to provide blip streams and for your
-- library users or application developers or the game logic you are
-- writing.
--
-- For this, you might find the hidden constructors and tools in
-- "Control.Auto.Blip.Internal" helpful, and there is more information at
-- the documentation for that module.
--

-- | Merge all the blip streams together into one, favoring the first
-- emitted value.
mergeLs :: [Blip a] -> Blip a
mergeLs = foldr mergeL NoBlip

-- | Merge all the blip streams together into one, favoring the last
-- emitted value.
mergeRs :: [Blip a] -> Blip a
mergeRs = foldl' mergeR NoBlip

-- | Merge all of the blip streams together, using the given merging
-- function associating from the right.
--
-- __DEPRECATED__: In its current form, 'foldrB' will disappear in @0.5@.
-- The new version will be:
--
-- @
-- foldrB :: (a -> a -> a) -> [Blip a] -> Blip b
-- @
--
-- Which will not emit if nothing emits.  This really was supposed to be
-- the intended behavior originally.
--
-- For this reason, please do not use this anymore.  As it is currently
-- implemented, it doesn't really make any sense, either.
--
-- To begin using the new behavior, you can use:
--
-- @
-- foldr (merge f) mempty
-- @
--
foldrB :: (a -> a -> a) -> a -> [Blip a] -> Blip a
foldrB f x0 = foldr (merge f) (Blip x0)
{-# DEPRECATED foldrB "Starting in v0.5, will have new functionality." #-}


-- foldrB :: (a -> a -> a) -> [Blip a] -> Blip a
-- foldrB f = foldr (merge f) NoBlip

-- | Merge all of the blip streams together, using the given merging
-- function associating from the left.
--
-- __DEPRECATED__: In its current form, 'foldlB'' will disappear in @0.5@.
-- The new version will be:
--
-- @
-- foldlB' :: (a -> a -> a) -> [Blip a] -> Blip b
-- @
--
-- Which will not emit if nothing emits.  This really was supposed to be
-- the intended behavior originally.
--
-- For this reason, please do not use this anymore.  As it is currently
-- implemented, it doesn't really make any sense, either.
--
-- To begin using the new behavior, you can use:
--
-- @
-- foldl' (merge f) mempty
-- @
foldlB' :: (a -> a -> a) -> a -> [Blip a] -> Blip a
foldlB' f x0 = foldl' (merge f) (Blip x0)
{-# DEPRECATED foldlB' "Starting in v0.5, will have new functionality." #-}

-- foldlB' :: (a -> a -> a) -> [Blip a] -> Blip a
-- foldlB' f = foldl' (merge f) NoBlip


-- | Takes two 'Auto's producing blip streams and returns a "merged"
-- 'Auto' that emits when either of the original 'Auto's emit.  When both
-- emit at the same time, the left (first) one is favored.
--
-- prop> a1 <& a2 == mergeL <$> a1 <*> a2
(<&) :: Monad m
     => Auto m a (Blip b)
     -> Auto m a (Blip b)
     -> Auto m a (Blip b)
(<&) = liftA2 mergeL

-- | Takes two 'Auto's producing blip streams and returns a "merged"
-- 'Auto' that emits when either of the original 'Auto's emit.  When both
-- emit at the same time, the right (second) one is favored.
--
-- prop> a1 &> a2 == mergeR <$> a1 <*> a2
(&>) :: Monad m
     => Auto m a (Blip b)
     -> Auto m a (Blip b)
     -> Auto m a (Blip b)
(&>) = liftA2 mergeR


-- | An 'Auto' that ignores its input and produces a blip stream never
-- emits.
never :: Auto m a (Blip b)
never = mkConst NoBlip

-- | Produces a blip stream that emits with the first received input value,
-- and never again after that.
--
-- Often used with 'pure':
--
-- > immediately . pure "Emit me!"
--
-- Or, in proc notation:
--
-- > blp <- immediately -< "Emit me!"
--
-- to get a blip stream that emits a given value (eg., "Emit me!") once
-- and stops emitting ever again.
--
-- >>> streamAuto' (immediately . pure "Emit me!") [1..5]
-- [Blip "Emit Me!", NoBlip, NoBlip, NoBlip, NoBlip]
--
immediately :: Auto m a (Blip a)
immediately = mkState f False
  where
    f _ True  = (NoBlip, True)
    f x False = (Blip x, True)

-- | Produces a blip stream that only emits once, with the input value on
-- the given step number.  It emits the input /on/ that many steps.
--
-- prop> immediately == inB 1
inB :: Int                -- ^ number of steps before value is emitted.
    -> Auto m a (Blip a)
inB = mkState f . Just
  where
    f _ Nothing              = (NoBlip, Nothing)
    f x (Just i) | i <= 1    = (Blip x, Nothing)
                 | otherwise = (NoBlip, Just (i - 1))

-- | A blip stream of lists that emits after the given number of steps,
-- with a list of all of the received values in that period.
collectN :: Serialize a => Int -> Auto m a (Blip [a])
collectN (max 1 -> n) = mkState (_collectBsF n . Blip) (n, [])

-- | The non-serializing/non-resuming version of 'collectN'
collectN_ :: Int -> Auto m a (Blip [a])
collectN_ (max 1 -> n) = mkState_ (_collectBsF n . Blip) (n, [])

-- | A blip stream that listens to an input blip stream and emits after the
-- input stream emits a given number of times.  Emits with a list of all
-- received emitted values.
collectBs :: Serialize a => Int -> Auto m (Blip a) (Blip [a])
collectBs (max 1 -> n) = mkState (_collectBsF n) (n, [])

-- | The non-serializing/non-resuming version of 'collectBs'.
collectBs_ :: Int -> Auto m (Blip a) (Blip [a])
collectBs_ (max 1 -> n) = mkState_ (_collectBsF n) (n, [])

-- n expected to be strictly positive
_collectBsF :: Int -> Blip a -> (Int, [a]) -> (Blip [a], (Int, [a]))
_collectBsF n = f
  where
    f (Blip x) (i, xs) | i == 1    = (Blip (reverse (x:xs)), (n    , []  ))
                       | otherwise = (NoBlip               , (i - 1, x:xs))
    f _        s       = (NoBlip, s)

-- | Produces a blip stream that emits the input value whenever the input
-- satisfies a given predicate.
--
-- Warning!  This 'Auto' has the capability of "breaking" blip semantics.
-- Be sure you know what you are doing when using this.  Blip streams are
-- semantically supposed to only emit at discrete, separate occurrences.
-- Do not use this for interval-like (on and off for chunks at a time)
-- things; each input should be dealt with as a separate thing.
--
-- For interval semantics, we have 'Interval' from "Control.Auto.Interval".
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
-- > emitOn (const True) . foo
--
-- These bad examples would be good use cases of 'Interval'.
--
-- Can be particularly useful with prisms from the /lens/ package, where
-- things like @emitOn (has _Right)@ and @emitOn (hasn't _Right)@ will emit
-- the input @Either a b@ whenever it is or isn't a 'Right'.  See
-- 'emitJusts' for more common uses with /lens/.
--
emitOn :: (a -> Bool)   -- ^ predicate to emit on
       -> Auto m a (Blip a)
emitOn p = mkFunc $ \x -> if p x then Blip x else NoBlip

-- | An 'Auto' that runs every input through a @a -> 'Maybe' b@ test and
-- produces a blip stream that emits the value inside every 'Just' result.
--
-- Particularly useful with prisms from the /lens/ package, where things
-- like @emitJusts (preview _Right)@ will emit the @b@ whenever the input
-- @Either a b@ stream is a @Right@.
--
-- Warning!  Carries all of the same dangers of 'emitOn'.  You can easily
-- break blip semantics with this if you aren't sure what you are doing.
-- Remember to only emit at discrete, separate occurences, and not for
-- interval-like (on and off for chunks at a time) things.  For interval
-- semantics, we have "Control.Auto.Interval".
--
-- See the examples of 'emitOn' for more concrete good/bad use cases.
emitJusts :: (a -> Maybe b)     -- ^ "predicate" to emit on.
          -> Auto m a (Blip b)
emitJusts p = mkFunc (maybe NoBlip Blip . p)


-- | @'every' n@ is an 'Auto' that emits with the incoming inputs on every
-- @n@th input value.  First emitted value is on the @n@th step.
--
-- Will obviously break blip semantics when you pass in 1.
--
every :: Int    -- ^ emit every @n@ steps.
      -> Auto m a (Blip a)
every (max 1 -> n) = mkState f n
  where
    f x i | i <= 1    = (Blip x, n    )
          | otherwise = (NoBlip, i - 1)

-- | @'eachAt' n xs@ is an 'Auto' that ignores its input and creates
-- a blip stream that emits each element of @xs@ one at a time, evey @n@
-- steps.  First emitted value is at step @n@.
--
-- Once the list is exhausted, never emits again.
--
-- Obviously breaks blip semantics when you pass in 1.
--
-- The process of serializing and resuming this 'Auto' is O(n) space and
-- time with the length of @xs@.  So don't serialize this if you plan on
-- passing an infinite list :)  See "Control.Auto.Generate" for more
-- options.
--
-- prop> eachAt n xs == perBlip (fromList xs) . every n
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
                            | otherwise -> (NoBlip, (i - 1, xs))

-- | Suppress all upstream emissions when the predicate (on the emitted
-- value) fails.
filterB :: (a -> Bool)      -- ^ filtering predicate
        -> Auto m (Blip a) (Blip a)
filterB p = mkFunc $ \x -> case x of
                             Blip x' | p x' -> x
                             _              -> NoBlip

-- | "Splits" a blip stream based on a predicate.  Takes in one blip stream
-- and produces two: the first emits whenever the input emits with a value
-- that passes the predicate, and the second emits whenever the input emits
-- with a value that doesn't.
splitB :: (a -> Bool)
      -> Auto m (Blip a) (Blip a, Blip a)
splitB p = mkFunc $ \x -> case x of
                            Blip x' | p x'      -> (x, NoBlip)
                                    | otherwise -> (NoBlip, x)
                            _                   -> (NoBlip, NoBlip)

-- | "Collapses" a blip stream of blip streams into single blip stream.
-- that emits whenever the inner-nested stream emits.
joinB :: Auto m (Blip (Blip a)) (Blip a)
joinB = mkFunc (blip NoBlip id)

collectB :: (Serialize a, Serialize b)
         => Auto m (Blip a, Blip b) (Blip (a, b))
collectB = mkState _collectBF (Nothing, Nothing)

collectB_ :: Auto m (Blip a, Blip b) (Blip (a, b))
collectB_ = mkState_ _collectBF (Nothing, Nothing)

_collectBF :: (Blip a, Blip b)
           -> (Maybe a, Maybe b)
           -> (Blip (a, b), (Maybe a, Maybe b))
_collectBF (b1, b2) (st1, st2) =
    case liftA2 (,) st1' st2' of
      Just (x, y) -> (Blip (x, y), (Nothing, Nothing))
      Nothing     -> (NoBlip     , (st1'   , st2'   ))
  where
    st1' = st1 <|> blip Nothing Just b1
    st2' = st2 <|> blip Nothing Just b2


-- | Applies the given function to every emitted value, and suppresses all
-- those for which the result is 'Nothing'.  Otherwise, lets it pass
-- through with the value in the 'Just'.
mapMaybeB :: (a -> Maybe b)
          -> Auto m (Blip a) (Blip b)
mapMaybeB f = mkFunc $ \x -> case x of
                               Blip x' -> maybe NoBlip Blip $ f x'
                               _       -> NoBlip

-- | Supress all upstream emitted values except for the very first.
once :: Auto m (Blip a) (Blip a)
once = mkState f False
  where
    f _          True  = (NoBlip, True )
    f e@(Blip _) False = (e,       True )
    f _          False = (NoBlip, False)

-- | Suppress only the first emission coming from upstream, and let all the
-- others pass uninhibited.
notYet :: Auto m (Blip a) (Blip a)
notYet = mkState f False
  where
    f e        True  = (e      , True )
    f (Blip _) False = (NoBlip, True )
    f _        False = (NoBlip, False)

-- | @'takeB' n@ allows only the first @n@ emissions to pass; it suppresses
-- all of the rest.
takeB :: Int    -- ^ number of emissions to allow to pass
      -> Auto m (Blip a) (Blip a)
takeB = mkState f . max 0
  where
    f _ 0          = (NoBlip, 0  )
    f e@(Blip _) i = (e      , i-1)
    f _          i = (NoBlip, i  )

-- | Allow all emitted valuesto pass until the first that fails the
-- predicate.
takeWhileB :: (a -> Bool)       -- ^ filtering predicate
           -> Auto m (Blip a) (Blip a)
takeWhileB p = mkState f False
  where
    f _          True        = (NoBlip, True )
    f e@(Blip x) False | p x = (e      , False)
    f _          False       = (NoBlip, True )

-- | @'dropB' n@ suppresses the first @n@ emissions from upstream and
-- passes through the rest uninhibited.
dropB :: Int      -- ^ number of emissions to suppress initially
      -> Auto m (Blip a) (Blip a)
dropB = mkState f . max 0
  where
    f x        0 = (x      , 0  )
    f (Blip _) i = (NoBlip, i-1)
    f _        i = (NoBlip, i  )

-- | Suppress all emited values until the first one satisfying the
-- predicate, then allow the rest to pass through.
dropWhileB :: (a -> Bool)     -- ^ filtering predicate
           -> Auto m (Blip a) (Blip a)
dropWhileB p = mkState f False
  where
    f e          True              = (e      , True )
    f e@(Blip x) False | p x       = (NoBlip, False)
                       | otherwise = (e      , True )
    f _          False             = (NoBlip, False)

-- | Takes in a blip stream and outputs a blip stream where each emission
-- is delayed/lagged by one step.
--
-- >>> streamAuto' (emitOn (\x -> x `mod` 3 == 0)) [1..9]
-- >>> [NoBlip, NoBlip, Blip 3, NoBlip, NoBlip, Blip 6, NoBlip, NoBlip, Blip 9]
-- >>> streamAuto' (lagBlips . emitOn (\x -> x `mod` 3 == 0)) [1..9]
-- >>> [NoBlip, NoBlip, NoBlip, Blip 3, NoBlip, NoBlip, Blip 6, NoBlip, NoBlip]
--
lagBlips :: Serialize a => Auto m (Blip a) (Blip a)
lagBlips = mkState (\x s -> (s, x)) NoBlip

-- | The non-serializing/non-resuming version of 'lagBlips'.
lagBlips_ :: Auto m (Blip a) (Blip a)
lagBlips_ = mkState_ (\x s -> (s, x)) NoBlip

-- | Accumulates all emissions in the incoming blip stream with
-- a "folding function", with a given starting value.  @b -> a -> b@, with
-- a starting @b@, gives @'Auto' m ('Blip' a) ('Blip' b)@.
--
-- The resulting blip stream will emit every time the input stream emits,
-- but with the "accumulated value".
--
-- Basically 'accum', but on blip stream emissions.
--
-- prop> accumB f x0 == perBlip (accum f x0)
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

-- | The output is the result of folding up every emitted value seen thus
-- far, with the given folding function and initial value.
--
-- prop> scanB f x0 == holdWith x0 . accumB f x0
--
-- >>> let a = scanB (+) 0 . eachAt 2 [1,2,3]
-- >>> take 8 . streamAuto' a $ repeat ()
-- [0, 1, 1, 3, 3, 6, 6, 6, 6]
scanB :: Serialize b
      => (b -> a -> b)      -- ^ folding function
      -> b                  -- ^ initial value
      -> Auto m (Blip a) b
scanB f = accum (_scanBF f)

-- | The non-serializing/non-resuming version of 'scanB'.
scanB_ :: (b -> a -> b)
       -> b                   -- ^ folding function
       -> Auto m (Blip a) b   -- ^ initial value
scanB_ f = accum_ (_scanBF f)

_scanBF :: (b -> a -> b) -> b -> Blip a -> b
_scanBF f y0 = blip y0 (f y0)

-- | The output is the 'mconcat' (monoid sum) of all emitted values seen
-- this far.
mscanB :: (Monoid a, Serialize a)
       => Auto m (Blip a) a
mscanB = scanB (<>) mempty

-- | The non-serializing/non-resuming version of 'mscanB'.
mscanB_ :: Monoid a
        => Auto m (Blip a) a
mscanB_ = scanB_ (<>) mempty

-- | The output is the number of emitted values received from the upstream
-- blip stream so far.
countB :: Auto m (Blip a) Int
countB = accum (\i -> (i +) . blip 0 (const 1)) 0

-- | Blip stream that emits whenever the predicate applied to the input
-- switches from false to true.  Emits with the triggering input value.
became :: Serialize a
       => (a -> Bool)       -- ^ change condition
       -> Auto m a (Blip a)
became p = accum (_becameF p) NoBlip

-- | Blip stream that emits whenever the predicate applied to the input
-- switches from true to false.  Emits with the triggering input value.
noLonger :: Serialize a
         => (a -> Bool)     -- ^ change condition
         -> Auto m a (Blip a)
noLonger p = became (not . p)

-- | Blip stream that emits whenever the predicate applied to the input
-- switches from true to false or false to true.  Emits with the triggering
-- input value.
onFlip :: (Serialize a, Monad m)
       => (a -> Bool)       -- ^ change condition
       -> Auto m a (Blip a)
onFlip p = became p &> noLonger p

-- | The non-serializing/non-resumable version of 'became'.
became_ :: (a -> Bool)      -- ^ change condition
        -> Auto m a (Blip a)
became_ p = accum_ (_becameF p) NoBlip

-- | The non-serializing/non-resumable version of 'noLonger'.
noLonger_ :: (a -> Bool)    -- ^ change condition
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
--
-- Useful because it can be serialized without the output needing
-- a 'Serialize' instance.
became' :: (a -> Bool)        -- ^ change condition
        -> Auto m a (Blip ())
became' p = accum f NoBlip
  where
    f e x | p x       = blip (Blip ()) (const NoBlip) e
          | otherwise = NoBlip

-- | Like 'noLonger', but emits a '()' instead of the triggering input
-- value.
--
-- Useful because it can be serialized without the output needing
-- a 'Serialize' instance.
noLonger' :: (a -> Bool)        -- ^ change condition
          -> Auto m a (Blip ())
noLonger' p = became' (not . p)

-- | Like 'onFlip', but emits a 'Bool' instead of the triggering input
-- value.  An emitted 'True' indicates that the predicate just became true;
-- an emitted 'False' indicates that the predicate just became false.
--
-- Useful because it can be serialized without the output needing
-- a 'Serialize' instance.
onFlip' :: Monad m
        => (a -> Bool)            -- ^ change condition
        -> Auto m a (Blip Bool)
onFlip' p = fmap (True <$) (became' p) &> fmap (False <$) (noLonger' p)

-- | Blip stream that emits whenever the input value changes. Emits with
-- the new value.
--
-- Warning: Note that, when composed on a value that is never expected to
-- keep the same value twice, this technically breaks blip semantics.
onChange :: (Serialize a, Eq a) => Auto m a (Blip a)
onChange = mkState _onChangeF Nothing

-- | The non-serializing/non-resumable version of 'onChange'.
onChange_ :: Eq a => Auto m a (Blip a)
onChange_ = mkState_ _onChangeF Nothing

_onChangeF :: Eq a => a -> Maybe a -> (Blip a, Maybe a)
_onChangeF x Nothing               = (NoBlip, Just x )
_onChangeF x (Just x') | x == x'   = (NoBlip, Just x')
                       | otherwise = (Blip x, Just x )

-- | An 'Auto' that emits whenever it receives a 'Just' input, with the
-- value inside the 'Just'.
--
-- Warning!  Carries all of the same dangers of 'emitOn'.  You can easily
-- break blip semantics with this if you aren't sure what you are doing.
-- Remember to only emit at discrete, separate occurences, and not for
-- interval-like (on and off for chunks at a time) things.  For interval
-- semantics, we have "Control.Auto.Interval".
--
-- See the examples of 'emitOn' for more concrete good/bad use cases.
--
-- prop> onJusts == emitJusts id
onJusts :: Auto m (Maybe a) (Blip a)
onJusts = mkFunc (maybe NoBlip Blip)

-- | Like 'onJusts', except forks into two streams depending on if the
-- input is 'Left' or 'Right'.
--
-- Is only meaningful if you expect every 'Left'/'Right' choice to be
-- independent of the last.
--
-- prop> onEithers == emitEithers id
onEithers :: Auto m (Either a b) (Blip a, Blip b)
onEithers = mkFunc $ \ex -> case ex of
                              Left x  -> (Blip x, NoBlip)
                              Right x -> (NoBlip, Blip x)

-- | Like 'emitJusts', except forks into two streams depending on the
-- function's result being 'Left' or 'Right'.
--
-- Is only meaningful if you expect every 'Left'/'Right' choice to be
-- independent of the last.
emitEithers :: (a -> Either b c) -> Auto m a (Blip b, Blip c)
emitEithers f = mkFunc $ \x -> case f x of
                                 Left y  -> (Blip y, NoBlip)
                                 Right y -> (NoBlip, Blip y)

-- | @'fromBlips' d@ is an 'Auto' that decomposes the incoming blip
-- stream by constantly outputting @d@ except when the stream emits, and
-- outputs the emitted value when it does.
fromBlips :: a  -- ^ the "default value" to output when the input is not
                --   emitting.
          -> Auto m (Blip a) a
fromBlips d = mkFunc $ blip d id

-- | @'fromBlipsWith' d f@ is an 'Auto' that decomposes the incoming blip
-- stream by constantly outputting @d@ except when the stream emits, and
-- outputs the result of applying @f@ to the emitted value when it does.
fromBlipsWith :: b          -- ^ the 'default value" to output when the input is not
                            --   emitting.
              -> (a -> b)   -- ^ the function to apply to the emitted value
                            --   whenever input is emitting.
              -> Auto m (Blip a) b
fromBlipsWith d f = mkFunc $ blip d f

-- | Collapse a blip stream of `a`s into a stream of `Maybe a`'s
asMaybes :: Auto m (Blip a) (Maybe a)
asMaybes = mkFunc $ blip Nothing Just

-- | Take in a normal stream and a blip stream.  Behave like the normal
-- stream when the blip stream doesn't emit...but when it does, output the
-- emitted value instead.
substituteB :: Auto m (a, Blip a) a
substituteB = mkFunc $ \(x, b) -> blip x id b

-- | @'holdWith' y0@ is an 'Auto' whose output is always the /most recently
-- emitted/ value from the input blip stream.  Before anything is emitted,
-- @y0@ is outputted as a placeholder.
--
-- Contrast with 'hold' from "Control.Auto.Interval".
holdWith :: Serialize a
         => a
         -> Auto m (Blip a) a
holdWith = accum f
  where
    f x = blip x id

-- | A non-serializing/non-resumable version of 'holdWith'.
holdWith_ :: a
          -> Auto m (Blip a) a
holdWith_ = accum_ f
  where
    f x = blip x id


-- | Re-emits every emission from the input blip stream, but replaces its
-- value with the given value.
--
-- prop> tagBlips x == modifyBlips (const x)
tagBlips :: b             -- ^ value to replace every emitted value with
         -> Auto m (Blip a) (Blip b)
tagBlips y = mkFunc (y <$)

-- | Re-emits every emission from the input blip stream, but applies the
-- given function to the emitted value.
modifyBlips :: (a -> b)     -- ^ function to modify emitted values with
            -> Auto m (Blip a) (Blip b)
modifyBlips f = mkFunc (fmap f)

-- | Takes an @'Auto' m a b@ (an 'Auto' that turns incoming @a@s into
-- outputting @b@s) into an @'Auto' m ('Blip' a) ('Blip' b)@; the original
-- 'Auto' is lifted to only be applied to emitted contents of a blip
-- stream.
--
-- When the stream emits, the original 'Auto' is "stepped" with the emitted
-- value; when it does not, it is paused and frozen until the next
-- emission.
--
-- >>> let sums = perBlip (sumFrom 0)
-- >>> let blps = eachAt 2 [1,5,2]
-- >>> take 8 . streamAuto' blps $ repeat ()
-- [NoBlip, Blip 1, NoBlip, Blip 5, NoBlip, Blip 2, NoBlip, NoBlip]
-- >>> take 8 . streamAuto' (sums . blps) $ repeat ()
-- [NoBlip, Blip 1, NoBlip, Blip 6, NoBlip, Blip 8, NoBlip, NoBlip]
--
perBlip :: Monad m => Auto m a b -> Auto m (Blip a) (Blip b)
perBlip = dimap to from . right
  where
    to   = blip (Left ()) Right
    from = either (const NoBlip) Blip
