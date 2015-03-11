-- |
-- Module      : Control.Auto.Switch
-- Description : Combinators for dynamically switching between and
--               sequencing 'Auto's.
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
--
-- A collection of versatile switching mechanisms.  Switching is really
-- a core mechanic at the heart of how to structure a lot of program
-- logics.  Switching from one "mode" to another, from dead to alive, from
-- room to room, menu to menu...switching between 'Auto's is a core part
-- about how many programs are built.
--
-- All of the switches here take advantage of either blip semantics (from
-- "Control.Auto.Blip") or /Interval/ semantics (from
-- "Control.Auto.Interval")...so this is where maintaining semantically
-- meaningful blip streams and intervals pays off!
--
-- Each switch here has various examples, and you'll find many of these in
-- use in the <https://github.com/mstksg/auto-examples example projects>.
--
-- Note the naming convention going on here (also used in
-- "Control.Auto.Serialize"):  A switch "from" a blip stream is triggered
-- "internally" by the 'Auto' being switched itself; a switch "on" a blip
-- stream is triggered "externally" by an 'Auto' that is /not/ swiched.
--

module Control.Auto.Switch (
  -- * Sequential switching
    (-->)
  , (-?>)
  -- * Arbitrary switching
  , switchFrom_
  , switchOn_
  -- * Function-based switches
  , switchOnF
  , switchOnF_
  , switchFromF
  , switchFromF_
  -- * Resetting
  , resetOn
  , resetFrom
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Auto.Interval
import Control.Category
import Data.Maybe
import Data.Serialize
import Prelude hiding             ((.), id)

infixr 1 -->
infixr 1 -?>

-- | "This, then that".  Behave like the first 'Interval' (and run its
-- effects) as long as it is "on" (outputting 'Just').  As soon as it turns
-- off (is 'Nothing), it'll "switch over" and begin behaving like the
-- second 'Auto' forever, running the effects of the second 'Auto', too.
-- Works well if the 'Auto's follow interval semantics from
-- "Control.Auto.Interval".
--
-- >>> let a1 = whileI (<= 4) --> pure 0
-- >>> streamAuto' a1 [1..10]
-- [1, 2, 3, 4, 0, 0, 0, 0, 0, 0]
--
-- ('whileI' only lets items satisfying the predicate pass through as "on",
-- and is "off" otherwise; 'pure' is the 'Auto' that always produces the
-- same output)
--
-- Association works in a way that you can "chain" '-->'s, as long as you
-- have an appropriate 'Auto' (and not 'Interval') at the end:
--
-- >>> let a2 = onFor 3 . sumFrom 0
--          --> onFor 3 . sumFrom 100
--          --> pure 0
-- >>> streamAuto' a2 [1..10]
-- [1,3,6,104,109,115,0,0,0,0]
--
-- @a --> b --> c@ associates as @a --> (b --> c)@
--
-- This is pretty invaluable for having 'Auto's "step" through a series of
-- different 'Auto's, progressing their state from one stage to the next.
-- 'Auto's can control when they want to be "moved on" from by turning
-- "off" (outputting 'Nothing').
--
-- Note that recursive bindings work just fine, so:
--
-- >>> let a3 = onFor 2 . pure "hello"
--          --> onFor 2 . pure "goodbye"
--          --> a3
-- >>> let (res3, _) = stepAutoN' 8 a3 ()
-- >>> res3
-- ["hello", "hello", "world", "world", "hello", "hello", "world", "world"]
--
-- the above represents an infinite loop between outputting "hello" and
-- outputting "world".
--
-- For serialization, an extra byte cost is incurred per invocation of
-- '-->'.  For cyclic switches like @a3@, every time the cycle "completes",
-- it adds another layer of '-->' byte costs.  For example, initially,
-- saving @a3@ incurs a cost for the two '-->'s.  After @a3@ loops once,
-- it incurs a cost for another two '-->'s, so it costs four '-->'s.  After
-- @a3@ loops another time, it is like a cost of six '-->'s.  So be aware
-- that for cyclic bindings like @a3@, space for serialization grows at
-- O(n).
--
-- By the way, it might be worth contrasting this with '<|!>' and '<|?>'
-- from "Control.Auto.Interval", which have the same type signatures.
-- Those alternative-y operators always /feed the input to both sides/,
-- /run both sides/, and output the first 'Just'.  With '<|!>', you can
-- "switch back and forth" to the first 'Auto' as soon as the first 'Auto'
-- is "on" ('Just') again.
--
-- '-->', in contrast, runs /only/ the first 'Auto' until it is
-- off ('Nothing')...then runs /only/ the second 'Auto'.  This transition is
-- one-way, as well.
(-->) :: Monad m
      => Interval m a b     -- ^ initial behavior
      -> Auto m a b         -- ^ final behavior, when the initial
                            --   behavior turns off.
      -> Auto m a b
a1 --> a2 = fmap fromJust (a1 -?> fmap Just a2)

-- | A variation of '-->', where the right hand side can also be an
-- interval/'Maybe'.  The entire result is, then, a 'Maybe'.  Probably less
-- useful than '-->' in most situations.
(-?>) :: Monad m
      => Interval m a b   -- ^ initial behavior
      -> Interval m a b   -- ^ final behavior, when the initial
                          --   behavior turns off.
      -> Interval m a b
a1 -?> a2 = mkAutoM l s t
  where
    l = do
      flag <- get
      if flag
        then resumeAuto (switched a2)
        else (-?> a2) <$> resumeAuto a1
    s = put False *> saveAuto a1
    t x = do
      (y1, a1') <- stepAuto a1 x
      case y1 of
        Just _  ->
          return (y1, a1' -?> a2)
        Nothing -> do
          (y, a2') <- stepAuto a2 x
          return (y, switched a2')
    switched a = mkAutoM (switched <$> resumeAuto a)
                         (put True  *> saveAuto a)
                       $ \x -> do
                           (y, a') <- stepAuto a x
                           return (y, switched a')
-- TODO: Add tests for the serialization here.

-- | Takes an 'Auto' who has both a normal output stream and a blip stream
-- output stream, where the blip stream emits new 'Auto's.
--
-- You can imagine 'switchFrom_' as a box containing a single 'Auto' like
-- the one just described.  It feeds its input into the contained 'Auto',
-- and its output stream is the "normal value" output stream of the
-- contained 'Auto'.
--
-- However, as soon as the blip stream of the contained 'Auto' emits a new
-- 'Auto'...it immediately /replaces/ the contained 'Auto' with the /new/
-- one.  And the whole thing starts all over again.
--
-- @'switchFrom_' a0@ will "start" with @a0@ already in the box.
--
-- This is mostly useful to allow 'Auto's to "replace themselves" or
-- control their own destiny, or the behavior of their successors.
--
-- In the following example, @a1@ is an 'Auto' that behaves like
-- a cumulative sum but also outputs a blip stream that will emit an 'Auto'
-- containing @'pure' 100@ (the 'Auto' that always emits 100) after three
-- steps.
--
-- @
-- a1 :: Auto' Int (Int, Blip (Auto' Int Int))
-- a1 = proc x -> do
--     sums       <- sumFrom 0 -< x
--     switchBlip <- inB 4     -< pure 100
--     id -< (sums, switchBlip)
--
-- -- alternatively
-- a1' = sumFrom 0 &&& (tagBlips (pure 100) . inB 4)
-- @
--
-- So, @'switchFrom_' a1@ will be the output of 'count' for three steps,
-- and then switch to @'pure' 100@ afterwards (when the blip stream
-- emits):
--
-- >>> streamAuto' (switchFrom_ a1) [1..10]
-- [1,3,6,10,100,100,100,100,100,100]
--
-- This is fun to use with recursion, so you can get looping switches:
--
-- @
-- a2 :: Auto' Int (Int, Blip (Auto' Int Int))
-- a2 = proc x -> do
--     sums       <- sumFrom 0 -< x
--     switchBlip <- inB 3     -< switchFrom_ a2
--     id -< (c, switchBlip)
--
-- -- alternatively
-- a2' = sumFrom 0 &&& (tagBlips (switchFrom_ a2') . inB 3)
-- @
--
-- >>> streamAuto' (switchFrom_ a2) [101..112]
-- [ 101, 203, 306  -- first 'sumFrom', on first three items
-- , 104, 209, 315  -- second 'sumFrom', on second three items
-- , 107, 215, 324  -- third 'sumFrom', on third three items (107, 108, 109)
-- , 110, 221, 333] -- final 'sumFrom', on fourht three items (110, 111, 112)
--
-- Note that this combinator is inherently unserializable, so you are going
-- to lose all serialization capabilities if you use this.  So sad, I know!
-- :(  This fact is reflected in the underscore suffix, as per convention.
--
-- If you want to use switching /and/ have serialization, you can use the
-- perfectly serialization-safe alternative, 'switchFromF', which slightly
-- less powerful in ways that are unlikely to be missed in practical usage.
-- That is, almost all non-contrived real life usages of 'switchFrom_' can
-- be recovered using 'switchFromF'.
--
switchFrom_ :: Monad m
            => Auto m a (b, Blip (Auto m a b))  -- ^ 'Auto' outputting a
                                                --   normal output (@b@)
                                                --   and a blip stream
                                                --   containing the 'Auto'
                                                --   to replace itself
                                                --   with.
            -> Auto m a b
switchFrom_ a0 = mkAutoM_ $ \x -> do
                              ((y, ea1), a0') <- stepAuto a0 x
                              return $ case ea1 of
                                Blip a1 -> (y, a1)
                                NoBlip  -> (y, switchFrom_ a0')

-- | You can think of this as a little box containing a single 'Auto'
-- inside.  Takes two input streams: an input stream of normal values, and
-- a blip stream containing 'Auto's.  It feeds the input stream into the
-- contained 'Auto'...but every time the input blip stream emits with a new
-- 'Auto', /replaces/ the contained 'Auto' with the emitted one.  Then
-- starts the cycle all over, immediately giving the new 'Auto' the
-- received input.
--
-- Useful for being able to externally "swap out" 'Auto's for a given
-- situation by just emitting a new 'Auto' in the blip stream.
--
-- For example, here we push several 'Auto's one after the other into the
-- box: @'sumFrom' 0@, @'productFrom' 1@, and 'count'. @'eachAt_' 4@ emits
-- each 'Auto' in the given list every four steps, starting on the fourth.
--
-- @
-- newAutos :: Auto' Int (Blip (Auto' Int Int))
-- newAutos = eachAt_ 4 [sumFrom 0, productFrom 1, count]
--
-- a :: Auto' Int Int
-- a = proc i -> do
--     blipAutos <- newAutos -< ()
--     switchOn_ (pure 0)    -< (i, blipAutos)
--
-- -- alternatively
-- a' = switchOn_ (pure 0) . (id &&& newAutos)
-- @
--
-- >>> streamAuto' a [1..12]
-- [ 1,  3,   6           -- output from sumFrom 0
-- , 4, 20, 120           -- output from productFrom 1
-- , 0,  1,   2, 3, 4, 5] -- output from count
--
-- Like 'switchFrom_', this combinator is inherently unserializable.  So if
-- you use it, you give up serialization for your 'Auto's.  This is
-- reflected in the underscore suffix.
--
-- If you wish to have the same switching devices but keep serialization,
-- you can use 'switchOnF', which is slightly less powerful, but should be
-- sufficient for all practical use cases.
--
switchOn_ :: Monad m
          => Auto m a b     -- ^ initial 'Auto'
          -> Auto m (a, Blip (Auto m a b)) b
switchOn_ a0 = mkAutoM_ $ \(x, ea1) -> do
                            let a = case ea1 of
                                      NoBlip  -> a0
                                      Blip a1 -> a1
                            (y, a') <- stepAuto a x
                            return (y, switchOn_ a')

-- | Essentially identical to 'switchFrom_', except insead of the 'Auto'
-- outputting a blip stream of new 'Auto's to replace itself with, it emits
-- a blip stream of @c@ --- and 'switchFromF' uses the @c@ to create the
-- new 'Auto'.
--
-- Here is the equivalent of the two examples from 'switchFrom_',
-- implemented with 'switchFromF'; see the documentatino for 'switchFrom_'
-- for a description of what they are to do.
--
-- @
-- a1 :: Auto' Int (Int, Blip Int)
-- a1 = proc x -> do
--     sums       <- sumFrom 0 -< x
--     switchBlip <- inB 4     -< 100
--     id -< (sums, switchBlip)
--
-- -- alternatively
-- a1' = sumFrom 0 &&& (tagBlips 100 . inB 4)
-- @
--
-- >>> streamAuto' (switchFrom_ pure a1) [1..10]
-- [1,3,6,10,100,100,100,100,100,100]
--
-- @
-- a2 :: Auto' Int (Int, Blip ())
-- a2 = proc x -> do
--     sums       <- sumFrom 0 -< x
--     switchBlip <- inB 3     -< ()
--     id -< (c, switchBlip)
--
-- -- alternatively
-- a2' = sumFrom 0 &&& (tagBlips () . inB 3)
-- @
--
-- >>> streamAuto' (switchFromF (const a2) a2) [101..112]
-- [ 101, 203, 306  -- first 'sumFrom', on first three items
-- , 104, 209, 315  -- second 'sumFrom', on second three items
-- , 107, 215, 324  -- third 'sumFrom', on third three items (107, 108, 109)
-- , 110, 221, 333] -- final 'sumFrom', on fourht three items (110, 111, 112)
--
-- Or, if you're only ever going to use @a2@ in switching form:
--
-- @
-- a2s :: Auto' Int Int
-- a2s = switchFromF (const a2s) $ proc x -> do
--           sums       <- sumFrom 0 -< x
--           switchBlip <- inB 3     -< ()
--           id -< (c, swichBlip)
--
-- -- or
-- a2s' = switchFromF (const a2s')
--      $ sumFrom 0 &&& (tagBlips () . inB 3)
-- @
--
-- >>> streamAuto' a2s [101..112]
-- [101, 203, 306, 104, 209, 315, 107, 215, 324, 110, 221, 333]
--
-- As you can see, all of the simple examples from 'switchFrom_' can be
-- implemented in 'switchFromF'...and so can most real-life examples.  The
-- advantage is that 'switchFromF' is serializable, and 'switchFrom_' is
-- not.
--
-- Note that for the examples above, instead of using 'const', we could
-- have actually used the input parameter to create a new 'Auto' based on
-- what we outputted.
--
switchFromF :: (Monad m, Serialize c)
            => (c -> Auto m a (b, Blip c))  -- ^ function to generate the
                                            --   next 'Auto' to behave like
            -> Auto m a (b, Blip c)         -- ^ initial 'Auto'.  the @b@
                                            --   is the output, and the
                                            --   blip stream triggers new
                                            --   'Auto's to replace this
                                            --   one.
            -> Auto m a b
switchFromF f = go Nothing
  where
    go mz a = mkAutoM (l a) s t
      where
        s   = put mz
           *> saveAuto a
        t x = do
          ((y, ez), a') <- stepAuto a x
          return $ case ez of
            Blip z -> (y, go (Just z) (f z))
            NoBlip -> (y, go mz       a'   )
    l a = do
      mz <- get
      case mz of
        Just z  -> go mz <$> resumeAuto (f z)
        Nothing -> go mz <$> resumeAuto a

-- | The non-serializing/non-resuming version of 'switchFromF'.
switchFromF_ :: Monad m
             => (c -> Auto m a (b, Blip c)) -- ^ function to generate the
                                            --   next 'Auto' to behave like
             -> Auto m a (b, Blip c)        -- ^ initial 'Auto'.  the @b@
                                            --   is the output, and the
                                            --   blip stream triggers new
                                            --   'Auto's to replace this
                                            --   one.
             -> Auto m a b
switchFromF_ f a0 = mkAutoM_ $ \x -> do
                                 ((y, ez), a0') <- stepAuto a0 x
                                 return $ case ez of
                                   Blip z -> (y, switchFromF_ f (f z))
                                   NoBlip -> (y, switchFromF_ f a0'  )

-- | Gives an 'Auto' the ability to "reset" itself on command
--
-- Basically acts like @'fmap' 'fst'@
--
-- @
-- fmap fst :: Monad m => Auto m a (b, Blip c) -> Auto m a b
-- @
--
-- But...whenever the blip stream emits..."resets" the 'Auto' back to the
-- original state, as if nothing ever happened.
--
-- Note that this resetting happens on the step /after/ the blip stream
-- emits.
--
-- Here is a summer that sends out a signal to reset itself whenever the
-- cumulative sum reaches 10 or higher:
--
-- @
-- limitSummer :: Auto' Int (Int, Blip ())
-- limitSummer = (id &&& became (>= 10)) . sumFrom 0
-- @
--
-- And now we throw it into 'resetFrom':
--
-- @
-- resettingSummer :: Auto' Int Int
-- resettingSummer = resetFrom limitSummer
-- @
--
-- >>> streamAuto' resettingSummer [1..10]
-- [ 1, 3, 6, 10    -- and...reset!
-- , 5, 11          -- and...reset!
-- , 7, 15          -- and...reset!
-- , 9, 19 ]
--
resetFrom :: Monad m
          => Auto m a (b, Blip c)   -- ^ The self-resetting 'Auto'
          -> Auto m a b
resetFrom a = switchFromF (const a') a'
  where
    a' = second (tagBlips ()) . a

-- | Essentially identical to 'switchOn_', except instead of taking in
-- a blip stream of new 'Auto's to put into the box, takes a blip stream
-- of @c@ --- and 'switchOnF' uses the @c@ to create the new 'Auto' to put
-- in the box.
--
-- Here is the equivalent of the two examples from 'switchOn_',
-- implemented with 'switchOnF'; see the documentatino for 'switchOn_'
-- for a description of what they are to do.
--
-- @
-- newAuto :: Int -> Auto' Int Int
-- newAuto 1 = sumFrom 0
-- newAuto 2 = productFrom 1
-- newAuto 3 = count
-- newAuto _ = error "Do you expect rigorous error handling from a toy example?"
--
-- a :: Auto' Int Int
-- a = proc i -> do
--     blipAutos <- eachAt 4 [1,2,3] -< ()
--     switchOnF_ newAuto (pure 0) -< (i, blipAutos)
-- @
--
-- >>> streamAuto' a [1..12]
-- [ 1,  3,   6           -- output from sumFrom 0
-- , 4, 20, 120           -- output from productFrom 1
-- , 0,  1,   2, 3, 4, 5] -- output from count
--
-- Instead of sending in the "replacement 'Auto'", sends in a number, which
-- corresponds to a specific replacement 'Auto'.
--
-- As you can see, all of the simple examples from 'switchOn_' can be
-- implemented in 'switchOnF'...and so can most real-life examples.  The
-- advantage is that 'switchOnF' is serializable, and 'switchOn_' is
-- not.
--
switchOnF :: (Monad m, Serialize c)
          => (c -> Auto m a b)    -- ^ function to generate the next 'Auto'
                                  --   to behave like
          -> Auto m a b           -- ^ initial starting 'Auto' to behave
                                  --   like
          -> Auto m (a, Blip c) b
switchOnF f = go Nothing
  where
    go mz a0 = mkAutoM (l a0) (s mz a0) (t mz a0)
    l a0 = do
      mz <- get
      case mz of
        Just z  -> go mz <$> resumeAuto (f z)
        Nothing -> go mz <$> resumeAuto a0
    s mz a0 = put mz
           *> saveAuto a0
    t mz a0 (x, ez) =
      case ez of
        NoBlip -> do
          (y, a0') <- stepAuto a0 x
          return (y, go mz a0')
        Blip z -> do
          (y, a1)  <- stepAuto (f z) x
          return (y, go (Just z) a1)

-- | The non-serializing/non-resuming version of 'switchOnF'.
switchOnF_ :: Monad m
           => (c -> Auto m a b)   -- ^ function to generate the next 'Auto'
                                  --   to behave like
           -> Auto m a b          -- ^ initial starting 'Auto' to behave
                                  --   like
           -> Auto m (a, Blip c) b
switchOnF_ f a0 = mkAutoM_ $ \(x, ez) ->
                              case ez of
                                NoBlip -> do
                                  (y, a0') <- stepAuto a0 x
                                  return (y, switchOnF_ f a0')
                                Blip z -> do
                                  (y, a1) <- stepAuto (f z) x
                                  return (y, switchOnF_ f a1)

-- | Takes an innocent 'Auto' and wraps a "reset button" around it.  It
-- behaves just like the original 'Auto' at first, but when the input blip
-- stream emits, the internal 'Auto' is reset back to the beginning.
--
-- Here we have 'sumFrom' wrapped around a reset button, and we send
-- in a blip stream that emits every 4 steps; so every 4th step, the whole
-- summer resets.
--
-- >>> let a = resetOn (sumFrom 0) . (id &&& every 4)
-- >>> streamAuto' a [101..112]
-- [ 101, 203, 306
-- , 104, 209, 315  -- resetted!
-- , 107, 215, 324  -- resetted!
-- , 110, 221, 333] -- resetted!
resetOn :: Monad m
        => Auto m a b   -- ^ 'Auto' to repeatedly reset
        -> Auto m (a, Blip c) b
resetOn a = switchOnF (const a) a . second (tagBlips ())
