{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Control.Auto.Time
-- Description : 'Auto's and 'Auto' transformers for observing and
--               manipulating the flow of time.
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- This module contains various 'Auto' transformers for manipulating the
-- flow of time/stepping rate of an 'Auto'.
--
-- Many of these are 'Auto' "transformers", meaning that they take in an
-- 'Auto' and return a transformed 'Auto', with new stepping behavior.
--
-- For example, there is 'accelerate':
--
-- @
-- 'accelerate' :: 'Monad' m => 'Int' -> 'Auto' m a b -> 'Auto' m a [b]
-- @
--
-- @'accelerate' n@ turns an 'Auto' into an 'Auto' that "steps itself" @n@
-- times for every single input/step.  The result is a list of the
-- results of each single step.
--
-- There are also various 'Auto's for observing the passage of time
-- ('count') and actiong as a "delay" or a way to access the previously
-- stepped values of an 'Auto'.
--

module Control.Auto.Time (
  -- * A counter
    count
  , count_
  -- * Manipulating time
  -- ** Delaying
  , lastVal
  , lastVal_
  , delay
  , delay_
  -- ** Stretching
  , stretch
  , stretchB
  , stretch_
  -- ** Accelerating
  , accelerate
  , accelerateWith
  , accelOverList
  -- ** Skipping
  , skipTo
  , fastForward
  , fastForwardEither
  ) where

import Control.Applicative
import Control.Auto.Blip.Internal
import Control.Arrow
import Control.Auto.Core
import Control.Auto.Generate
import Control.Monad
import Control.Monad.Loops
import Data.Serialize

-- | A simple 'Auto' that outputs the step count.  First output is 0.
count :: (Serialize b, Num b, Monad m) => Auto m a b
count = iterator (+1) 0

-- | A non-resuming/non-serializing version of 'count'.
count_ :: (Num b, Monad m) => Auto m a b
count_ = iterator_ (+1) 0

-- | An 'Auto' that returns the last value received by it.  Given an
-- "initial value" to output first.
--
-- This is a __very dangerous__ 'Auto', because its usage and its very
-- existence opens the door to breaking denotative/declarative style and
-- devolving into imperative style coding.
--
-- From my experience, the only meaningful usage of this is for recursive
-- bindings.  If you ever are laying out recursive bindings in
-- a high-level/denotative way and run into a @<<loop>>@, then you can use
-- 'lastVal' to explicitly state what depends on something in the past,
-- instead of right now (introducing a loop).
lastVal :: (Serialize a, Monad m) => a -> Auto m a a
lastVal = mkState $ \x s -> (s, x)

-- | The non-resuming/non-serializing version of 'lastVal'.
lastVal_ :: Monad m => a -> Auto m a a
lastVal_ = mkState_ $ \x s -> (s, x)

-- | An alias for 'lastVal'; used in contexts where "delay" is more
-- a meaningful description than "last value".
delay :: (Serialize a, Monad m) => a -> Auto m a a
delay = lastVal

-- | The non-resuming/non-serializing version of 'delay'.
delay_ :: Monad m => a -> Auto m a a
delay_ = lastVal_

-- | "stretch" an 'Auto' out, slowing time.  @'stretch' n a@ will take one
-- input, repeat the same output @n@ times (ignoring input), and then take
-- another.
--
-- >>> let a       = stretch 2 (mkAccum (+) 0)
-- >>> let (ys, _) = overList' a [1,8,5,4,3,7,2,0]
-- >>> ys
--    [1,1,6,6,9,9,11,11]
-- -- [1,_,5,_,3,_,2 ,_ ] <-- the inputs
stretch :: (Serialize b, Monad m) => Int -> Auto m a b -> Auto m a b
stretch n = go (1, undefined)
  where
    go (i, y) a = mkAutoM (go <$> get <*> loadAuto a)
                          (put (i, y) *> saveAuto a)
                          $ \x ->
                              if i <= 1
                                 then do
                                   Output y' a' <- stepAuto a x
                                   return (Output y' (go (n    , y') a'))
                                 else
                                   return (Output y  (go (i - 1, y ) a ))


-- | The non-resuming/non-serializing version of 'stretch'.
stretch_ :: Monad m => Int -> Auto m a b -> Auto m a b
stretch_ n = go (1, undefined)
  where
    go (i, y) a = mkAutoM_ $ \x ->
                               if i <= 1
                                  then do
                                    Output y' a' <- stepAuto a x
                                    return $ Output y (go (n    , y') a')
                                  else
                                    return $ Output y (go (i - 1, y ) a )

-- | Like 'stretch', but instead of holding the the "stretched" outputs,
-- emits a 'Blip' every time the stretched 'Auto' "progresses" (every @n@
-- ticks)
--
-- See 'stretch' for more information.
--
-- >>> let a = stretchB 2 (mkAccum (+) 0)
-- >>> let (ys, _) = overList' a [1,8,5,4,3,7,2,0]
-- >>> ys
-- [Blip 1, NoBlip, Blip 6, NoBlip, Blip 9, NoBlip, Blip 11, NoBlip]
--
stretchB :: Monad m => Int -> Auto m a b -> Auto m a (Blip b)
stretchB (max 1 -> n) = go 1
  where
    go i a = mkAutoM (go <$> get <*> loadAuto a)
                     (put i *> saveAuto a)
                     $ \x ->
                         if i <= 1
                           then do
                             Output y a' <- stepAuto a x
                             return $ Output (Blip y) (go n       a')
                           else
                             return $ Output NoBlip   (go (i - 1) a )

-- | Turns an 'Auto' into an "accelerated" 'Auto'; an 'Auto' that takes
-- an @a@ and returns a @b@ turns into an 'Auto' that takes a /list/ of @a@
-- and returns a /list/ of @b@ from running that 'Auto' on all of those @a@
-- in order.
--
-- >>> let a = accelOverList (mkAccum (+) 0)
-- >>> let Output ys1 a' = stepAuto' a [3,9,2]
-- >>> ys1
-- [3, 12, 14]
-- >>> let Output ys2 _  = stepAuto' a' [8,5]
-- >>> ys2
-- [22, 30]
accelOverList :: Monad m => Auto m a b -> Auto m [a] [b]
accelOverList = go
  where
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \xs -> do
                        res <- liftM reverse (unfoldrM f (a0, xs))
                        case res of
                          []          ->
                            return $ Output [] (go a0)
                          ((_, a'):_) ->
                            return $ Output (map fst res) (go a')

    f (_, [])   = return Nothing
    f (a, x:xs) = do
      Output y a' <- stepAuto a x
      return $ Just ((y, a'), (a', xs))

-- | @'accelerate' n a@ turns an 'Auto' @a@ into an "accelerated" 'Auto',
-- where every input is fed into the 'Auto' @n@ times.  All of the results
-- are collected in the output.
--
-- The same input is fed repeatedly @n@ times.
--
-- >>> let a = accelerate 3 (mkAccum (+) 0)
-- >>> let Output ys1 a' = stepAuto a 5
-- >>> ys1
-- [5,10,15]
-- >>> let Output ys2 _  = stepAuto a' (-8)
-- >>> ys2
-- [7,-1,-9]
accelerate :: Monad m => Int -> Auto m a b -> Auto m a [b]
accelerate n = go
  where
    n'    = max n 1
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x0 -> do
                        yas <- flip (iterateM n') (undefined, a0)
                               $ \(_, a) -> do
                                   Output x a' <- stepAuto a x0
                                   x `seq` return (x, a')
                        let ys = map fst yas
                            a' = snd (last yas)
                        return (Output ys (go a'))
{-# INLINE accelerate #-}

-- | @'accelerateWith' xd n a@ behaves like @'accelerate' n a@, except
-- instead of /repeating/ the same intput @n@ times, that input is fed
-- once; the rest of the @n@ times, @a@ is fed @xd@, the "default" @x@.
--
-- >>> let a = accelerateWith (-1) 3 (mkAccum (+) 0)
-- >>> let Output ys1 a' = stepAuto a 5
-- >>> ys1
-- [5,4,3]    -- fed 5, then (-1) twice
-- >>> let Output ys2 _  = stepAuto a' 14
-- >>> ys2
-- [17,16,15] -- fed 14, then (-1) twice
accelerateWith :: Monad m => a -> Int -> Auto m a b -> Auto m a [b]
accelerateWith xd n | n <= 1    = fmap (:[])
                    | otherwise = go
  where
    n'    = n - 1
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x0 -> do
                        Output y0 a1  <- stepAuto a0 x0
                        yas <- flip (iterateM n') (undefined, a1)
                               $ \(_, a) -> do
                                   Output x a' <- stepAuto a xd
                                   return (x, a')
                        let ys = y0 : map fst yas
                            a' = snd (last yas)
                        return (Output ys (go a'))

skipTo :: forall m a b c. Monad m => a -> Auto m a (b, Blip c) -> Auto m a ([b], c)
skipTo x0 = go
  where
    go :: Auto m a (b, Blip c)
       -> Auto m a ([b], c)
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x -> do
                      ((ys, z), a1) <- skipOut a0 x []
                      return (Output (reverse ys, z) (go a1))
    skipOut :: Auto m a (b, Blip c)
            -> a
            -> [b]
            -> m (([b], c), Auto m a (b, Blip c))
    skipOut a0 x ys = do
      Output (y, bz) a1 <- stepAuto a0 x
      let ys' = y:ys
      case bz of
        Blip z -> return ((ys', z), a1)
        NoBlip -> skipOut a1 x0 ys'

fastForward :: forall m a b. Monad m
            => a                      -- ^ Default input
            -> Auto m a (Maybe b)
            -> Auto m a b
fastForward x0 = go
  where
    go :: Auto m a (Maybe b)
       -> Auto m a b
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    (skipNothings a0)
    skipNothings :: Auto m a (Maybe b) -> a -> m (Output m a b)
    skipNothings a0 x = do
      Output my a1 <- stepAuto a0 x
      case my of
        Nothing -> skipNothings a1 x0
        Just y  -> return (Output y (go a1))

fastForwardEither :: forall m a b c. Monad m
                  => a
                  -> Auto m a (Either c b)
                  -> Auto m a (b, [c])
fastForwardEither x0 = fmap (second reverse) . go
  where
    go :: Auto m a (Either c b)
       -> Auto m a (b, [c])
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    (skipNothings a0 [])
    skipNothings :: Auto m a (Either c b)
                 -> [c]
                 -> a
                 -> m (Output m a (b, [c]))
    skipNothings a0 zs x = do
      Output ey a1 <- stepAuto a0 x
      case ey of
        Left z  -> skipNothings a1 (z:zs) x0
        Right y -> return (Output (y, zs) (go a1))



iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM n f = go (max n 0)
  where
    go 0 _ = return []
    go i x = do
      x' <- f x
      xs <- go (i - 1) x'
      return (x' : xs)
