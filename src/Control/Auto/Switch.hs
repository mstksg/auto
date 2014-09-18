module Control.Auto.Switch (
  -- * Sequential switching
    (-->)
  , (-?>)
  -- * Arbitrary switching
  , switchFrom_
  , switchOn_
  -- * Function-based switches
  , switchFromF
  , switchFromF_
  , resetFrom
  , switchOnF
  , switchOnF_
  , resetOn
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Core
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
-- off/'Nothing'...then runs /only/ the second 'Auto'.  This transition is
-- one-way, too.
(-->) :: Monad m
      => Auto m a (Maybe b)   -- ^ initial behavior
      -> Auto m a b           -- ^ final behavior, when the initial
                              --   behavior turns off.
      -> Auto m a b
a1 --> a2 = fmap fromJust (a1 -?> fmap Just a2)

-- | A variation of '-->', where the right hand side can also be an
-- interval/'Maybe'.  The entire result is, then, a 'Maybe'.  Probably less
-- useful than '-->' in most situations.
(-?>) :: Monad m
      => Auto m a (Maybe b)   -- ^ initial behavior
      -> Auto m a (Maybe b)   -- ^ final behavior, when the initial
                              --   behavior turns off.
      -> Auto m a (Maybe b)
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

-- consider revelation
-- store last state of first 'Auto'.
-- hm but O(n) on number of switches so far.
-- oh well

-- | Takes an 'Auto' that emits an output and a 'Blip' stream emitting
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
            => Auto m a (b, Blip (Auto m a b))
            -> Auto m a b
switchFrom_ a0 = mkAutoM_ $ \x -> do
                              Output (y, ea1) a0' <- stepAuto a0 x
                              return $ case ea1 of
                                Blip a1 -> Output y a1
                                NoBlip  -> Output y (switchFrom_ a0')

switchOn_ :: Monad m
          => Auto m a b
          -> Auto m (a, Blip (Auto m a b)) b
switchOn_ a0 = mkAutoM_ $ \(x, ea1) -> do
                            let a = case ea1 of
                                      NoBlip  -> a0
                                      Blip a1 -> a1
                            Output y a' <- stepAuto a x
                            return (Output y (switchOn_ a'))


switchFromF :: (Monad m, Serialize c)
            => (c -> Auto m a (b, Blip c))
            -> Auto m a (b, Blip c)
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

switchFromF_ :: Monad m
             => (c -> Auto m a (b, Blip c))
             -> Auto m a (b, Blip c)
             -> Auto m a b
switchFromF_ f a0 = mkAutoM_ $ \x -> do
                                 Output (y, ez) a0' <- stepAuto a0 x
                                 return $ case ez of
                                   Blip z -> Output y (switchFromF_ f (f z))
                                   NoBlip -> Output y (switchFromF_ f a0')

resetFrom :: Monad m
          => Auto m a (b, Blip c)
          -> Auto m a b
resetFrom a = switchFromF (const a') a'
  where
    a' = second (tagBlips ()) . a


switchOnF :: (Monad m, Serialize c)
          => (c -> Auto m a b)
          -> Auto m a b
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

switchOnF_ :: Monad m
           => (c -> Auto m a b)
           -> Auto m a b
           -> Auto m (a, Blip c) b
switchOnF_ f a0 = mkAutoM_ $ \(x, ez) ->
                              case ez of
                                NoBlip -> do
                                  Output y a0' <- stepAuto a0 x
                                  return (Output y (switchOnF_ f a0'))
                                Blip z -> do
                                  Output y a1 <- stepAuto (f z) x
                                  return (Output y (switchOnF_ f a1))

resetOn :: Monad m
        => Auto m a b
        -> Auto m (a, Blip c) b
resetOn a = switchOnF (const a) a . second (tagBlips ())
