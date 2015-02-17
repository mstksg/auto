-- |
-- Module      : Control.Auto.Switch
-- Description : Combinators for dynamically switching between and
--               sequencing 'Auto's.
-- Copyright   : (c) Justin Le 2014
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
-- All of the switches here take advantage of either 'Blip' semantics (from
-- "Control.Auto.Blip") or /Interval/ semantics (from
-- "Control.Auto.Interval")...so this is where maintaining semantically
-- meaningful 'Blip' streams and intervals pays off!
--
-- Each switch here has various examples, and you'll find many of these in
-- use in the <https://github.com/mstksg/auto-examples example projects>.
--
-- Note the naming convention going on here (also used in
-- "Control.Auto.Serialize"):  A switch "from" a 'Blip' stream is triggered
-- "internally" by the 'Auto' being switched itself; a switch "on" a 'Blip' stream is
-- triggered "externally" by an 'Auto' that is /not/ swiched.
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

-- | "One after the other".  Behave like the first 'Auto' (and run only its
-- effects) as long as it is "on" ('Just').  As soon as it is 'Nothing',
-- begin behaving like the second 'Auto' forevermore, and running the
-- second 'Auto''s effects, if any.  Works well if the 'Auto's follow
-- interval semantics from "Control.Auto.Interval".
--
-- >>> let a1 = (onFor 2 . pure "hello") --> pure "world"
-- >>> let Output res1 _ = stepAutoN' 5 a1 ()
-- >>> res1
-- ["hello", "hello", "world", "world", "world"]
--
-- (Recall that @'pure' "hello"@ is the 'Auto' that ignores its input and
-- constantly outputs "hello", and @'onFor' 2@ lets its input pass "on"
-- ('Just') for two steps, then is "off" ('Nothing') ever after.)
--
-- Association works in a way that you can "chain" '-->'s, as long as you
-- have a non-'Maybe' / non-"Interval" 'Auto' at the end.
--
-- >>> let a2 = onFor 2 . pure "hello"
--          --> onFor 2 . pure "world"
--          --> pure "goodbye!"
-- >>> let Output res2 _ = stepAutoN' 6 a2 ()
-- >>> res2
-- ["hello", "hello", "world", "world", "goodbye!", "goodbye!"]
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
--          --> onFor 2 . pure "world"
--          --> a3
-- >>> let (res3, _) = stepAutoN' 8 a3 ()
-- >>> res3
-- ["hello", "hello", "world", "world", "hello", "hello", "world", "world"]
--
-- the above represents an infinite loop between outputting "hello" and
-- outputting "world".
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
        then loadAuto (switched a2)
        else liftA2 (-?>) (loadAuto a1) (loadAuto a2)
    s = put False *> saveAuto a1 *> saveAuto a2
    t x = do
      Output y1 a1' <- stepAuto a1 x
      case y1 of
        Just _  ->
          return (Output y1 (a1' -?> a2))
        Nothing -> do
          Output y a2' <- stepAuto a2 x
          return (Output y (switched a2'))
    switched a = mkAutoM (switched <$> loadAuto a)
                         (put True  *> saveAuto a)
                         $ \x -> do
                             Output y a' <- stepAuto a x
                             return (Output y (switched a'))

-- | Takes an 'Auto' that emits an output with a 'Blip' stream emitting
-- 'Auto's.
--
-- @'switchFrom_' a0@ behaves like @'fst' '<$>' a0@ at first, outputting
-- the first part of the result tuple.  Then, as soon as the 'Blip' stream
-- emits with an 'Auto' @a1@, the whole thing behaves like @a1@ forever.
--
-- In the following example, @a1@ is an 'Auto' that counts upwards forever
-- and also outputs a 'Blip' stream that will emit an 'Auto' containing
-- @'pure' 100@ (the 'Auto' that always emits 100) after three steps.
--
-- @
--     a1 :: Auto' () (Int, Blip (Auto' () Int))
--     a1 = proc _ -> do
--         c          <- count -< ()
--         switchBlip <- inB 3 -< pure 100
--         id -< (c, switchBlip)
--
--     -- alternatively
--     a1' = count &&& (tagBlips (pure 100) . inB 3)
-- @
--
-- So, @'switchFrom_' a1@ will be the output of 'count' for three steps,
-- and then switch to @'pure' 100@ afterwards (when the 'Blip' stream
-- emits):
--
-- >>> let (res1, _) = stepAutoN' 6 (switchFrom_ a1) ()
-- >>> res1
-- [0, 1, 2, 100, 100, 100]
--
-- This is fun to use with recursion, so you can get looping switches:
--
-- @
--     a2 :: Auto' () (Int, Blip (Auto' () Int))
--     a2 = proc _ -> do
--         c <- count -< ()
--         switchBlip <- inB 3 -< switchFrom_ a2
--         id -< (c, switchBlip)
--
--    -- alternatively
--    a2' = count &&& (tagBlips (switchFrom_ a2) . inB 3)
-- @
--
-- >>> let (res2, _) = stepAutoN' 10 (switchFrom_ a2) ()
-- >>> res2
-- [0, 1, 2, 0, 1, 2, 0, 1, 2, 0]
--
-- Note that this combinator is inherently unserializable, so you are going
-- to lose all serialization capabilities if you use this.  So sad, I know!
-- :(  This fact is reflected in the underscore suffix, as per convention.
--
-- If you want to use switching /and/ have serialization, you can use the
-- perfectly serialization-safe alternative, 'switchFromF', which slightly
-- less powerful in ways that are unlikely to be missed in practical usage.
--
switchFrom_ :: Monad m
            => Auto m a (b, Blip (Auto m a b))  -- ^ 'Auto' outputting a
                                                --   normal output (@b@)
                                                --   and a 'Blip' stream
                                                --   containing the 'Auto'
                                                --   to replace itself
                                                --   with.
            -> Auto m a b
switchFrom_ a0 = mkAutoM_ $ \x -> do
                              Output (y, ea1) a0' <- stepAuto a0 x
                              return $ case ea1 of
                                Blip a1 -> Output y a1
                                NoBlip  -> Output y (switchFrom_ a0')

-- | Is a little 'Auto' box...whenever the input 'Blip' stream emits an
-- 'Auto', it begins to behave like that emitted 'Auto'.
--
-- Input to the contained 'Auto' is fed through the first field of the
-- tuple, and a 'Blip' stream emitting new 'Auto's is fed through the
-- second field.
--
-- When the stream emits, /immediately/ feeds the input to the new 'Auto'.
--
-- Given an initial 'Auto' to behave like.
--
-- For example, here we push several 'Auto's one after the other into the
-- box: @'pure' 100@, 'count', and @'iterator' (+3) 0@, starting with
-- @'pure' 0@.  @'eachAt_' 3@ emits each 'Auto' in the given list every
-- three steps, starting on the third.
--
-- @
--     newAutos :: Auto' () (Blip (Auto' () Int))
--     newAutos = eachAt_ 3 [pure 100, count, iterator (+3) 0]
--
--     a :: Auto' () Int
--     a = proc _ -> do
--         blipAutos <- newAutos -< ()
--         switchOn_ (pure 0)    -< ((), blipAutos)
--
--     -- alternatively
--     a' = switchOn_ (pure 0) . (pure () &&& newAutos)
-- @
--
-- >>> let (res, _) = stepAutoN' 14 a ()
-- >>> res
-- [0, 0, 100, 100, 100, 0, 1, 2, 0, 3, 6, 9, 12, 15]
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
                            Output y a' <- stepAuto a x
                            return (Output y (switchOn_ a'))

-- | Takes a function from a @c@ to a new (next) 'Auto', and an initial
-- 'Auto'.
--
-- These 'Auto's all taken in @a@ and output @b@, along with a 'Blip'
-- stream of @c@s.
--
-- @'switchFromF' f a0@ behaves like the current 'Auto' (beginning with
-- @a0@), taking in the @a@ and popping out the @b@.  However, as soon as
-- the 'Blip' stream emits a @c@, it replaces internal 'Auto' with a new
-- one, determined by the function.
--
-- Here is an example where the "next 'Auto'" function is @nextAuto@:
--
-- @
--     nextAuto :: Int -> Auto' () (String, Blip Int)
--     nextAuto i0 = proc _ -> do
--         c     <- sumFromD i0 -< 1
--         nextB <- inB 3       -< c
--         id    -< (show c, (+10) <$> nextB)
-- @
--
-- @nextAuto@ continually counts upwards from the given initial value @i0@.
-- Then, on the third step, the 'Blip' stream emits with the value of the
-- counter plus ten.
--
-- >>> let (res0, _) = stepAutoN' 5 (nextAuto 0) ()
-- >>> res0
-- [("0", NoBlip), ("1", NoBlip), ("2", Blip 13), ("3", NoBlip), ("4", NoBlip)]
--
-- Let's see what happens when we wrap it with 'switchFromF' and an initial
-- 'Auto' of @'nextAuto' 0@:
--
-- @
--     a :: Auto' () String
--     a = switchFromF nextAuto (nextAuto 0)
-- @
--
-- >>> let (res, _) = stepAutoN' 10 a ()
-- >>> res
-- [ "0", "1", "2", "12", "13", "14", "24", "25", "26", "36", "37"]
--
-- Every three steps, the the 'Auto' is replaced by a new one, skipping
-- ten steps ahead.  First it behaves like the first field of the output of
-- @nextAuto 0@, then like @nextAuto 12@, then like @nextAuto 24@, etc.
--
-- This is a more limited (but serializable) version of 'switchFrom_',
-- where you know the form of all the 'Auto's you might be switching to in
-- the future, and can generate them with a function conceived ahead of
-- time.
--
switchFromF :: (Monad m, Serialize c)
            => (c -> Auto m a (b, Blip c))  -- ^ function to generate the
                                            --   next 'Auto' to behave like
            -> Auto m a (b, Blip c)         -- ^ initial 'Auto'.  the @b@
                                            --   is the output, and the
                                            --   'Blip' stream triggers new
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
          Output (y, ez) a' <- stepAuto a x
          return $ case ez of
            Blip z -> Output y (go (Just z) (f z))
            NoBlip -> Output y (go mz       a'   )
    l a = do
      mz <- get
      case mz of
        Just z  -> go mz <$> loadAuto (f z)
        Nothing -> go mz <$> loadAuto a

-- | The non-serializing/non-resuming version of 'switchFromF'.
switchFromF_ :: Monad m
             => (c -> Auto m a (b, Blip c)) -- ^ function to generate the
                                            --   next 'Auto' to behave like
             -> Auto m a (b, Blip c)        -- ^ initial 'Auto'.  the @b@
                                            --   is the output, and the
                                            --   'Blip' stream triggers new
                                            --   'Auto's to replace this
                                            --   one.
             -> Auto m a b
switchFromF_ f a0 = mkAutoM_ $ \x -> do
                                 Output (y, ez) a0' <- stepAuto a0 x
                                 return $ case ez of
                                   Blip z -> Output y (switchFromF_ f (f z))
                                   NoBlip -> Output y (switchFromF_ f a0')

-- | Gives an 'Auto' the ability to "reset" itself on command
--
-- Basically acts like @'fmap' 'fst'@
--
-- @
--     fmap fst :: Monad m => Auto m a (b, Blip c) -> Auto m a b
-- @
--
-- But...whenever the 'Blip' stream emits..."resets" the 'Auto' back to the
-- original state, as if nothing ever happened.
--
-- Here is a counter that attempts to resets itself every three steps:
--
-- @
--     resetCounter :: Auto' () (Int, Blip ())
--     resetCounter = count &&& inB 3
-- @
--
-- And now we throw it into 'resetFrom':
--
-- @
--     resettingCounter :: Auto' () Int
--     resettingCounter = resetFrom resetCounter
-- @
--
-- >>> let (res, _) = stepAutoN' 11 resettingCounter ()
-- >>> res
-- [0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1]
resetFrom :: Monad m
          => Auto m a (b, Blip c)   -- ^ The self-resetting 'Auto'
          -> Auto m a b
resetFrom a = switchFromF (const a') a'
  where
    a' = second (tagBlips ()) . a

-- | Like 'switchOn_', @'switchOnF' f a0@ is like a little 'Auto' box
-- wrapping and running an 'Auto'.  It takes in both an input and a 'Blip'
-- stream of @c@'s.  It behaves just like the internal @'Auto' m a b@,
-- and initially behaves like @a0@, until the 'Blip' stream emits.
--
-- Then, it swaps the 'Auto' for a /new/ one, given by the function @f@.
-- @f@ is applied to the @c@ that the stream emits, and that new 'Auto' is
-- placed into the box and is immediately run on that same step.
--
-- In the following example we are using just @'iterate' (+1)@ for our
-- "next 'Auto'" function, and triggering it with a 'Blip' stream that
-- counts up from 0.
--
-- @
--     nextAuto :: Int -> Auto' () Int
--     nextAuto = iterator (+1)
--
--     triggerer :: Auto' () (Blip Int)
--     triggerer = stretchB 3 (iterator (+10) 0)
--
--     a :: Auto' () Int
--     a = proc _ -> do
--         triggers <- triggerer -< ()
--         switchOnF nextAuto (nextAuto 0) -< ((), triggers)
-- @
--
-- >>> let (res, _) = stepAutoN' 10 a ()
-- >>> res
-- [0,1,2,10,11,12,20,21,22,30,31]
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
        Just z  -> go mz <$> loadAuto (f z)
        Nothing -> go mz <$> loadAuto a0
    s mz a0 = put mz
           *> saveAuto a0
    t mz a0 (x, ez) =
      case ez of
        NoBlip -> do
          Output y a0' <- stepAuto a0 x
          return (Output y (go mz a0'))
        Blip z -> do
          Output y a1  <- stepAuto (f z) x
          return (Output y (go (Just z) a1))

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
                                  Output y a0' <- stepAuto a0 x
                                  return (Output y (switchOnF_ f a0'))
                                Blip z -> do
                                  Output y a1 <- stepAuto (f z) x
                                  return (Output y (switchOnF_ f a1))

-- | Takes an innocent 'Auto' and wraps a "reset button" around it.  It
-- behaves just like the original 'Auto' at first, but when the 'Blip'
-- stream emits, the internal 'Auto' is reset back to the beginning.
--
-- Here we have 'count', being reset every five steps:
--
-- >>> let a = resetOn count . (pure () &&& every 5)
-- >>> let (res, _) = stepAutoN' 15 a ()
-- >>> res
-- [0,1,2,3,0,1,2,3,4,0,1,2,3,4,0]
resetOn :: Monad m
        => Auto m a b   -- ^ 'Auto' to repeatedly reset
        -> Auto m (a, Blip c) b
resetOn a = switchOnF (const a) a . second (tagBlips ())
