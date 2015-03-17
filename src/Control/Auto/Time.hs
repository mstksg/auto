{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Control.Auto.Time
-- Description : 'Auto's and 'Auto' transformers for observing and
--               manipulating the flow of "time".
-- Copyright   : (c) Justin Le 2015
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
  , arrD
  , arrD_
  , delay
  , delay_
  , delayList
  , delayList_
  , delayN
  , delayN_
  -- ** "Priming"
  , priming
  -- ** Stretching
  , stretch
  , stretch_
  , stretchB
  , stretchAccumBy
  , stretchAccumBy_
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
import Control.Arrow
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Auto.Generate
import Control.Auto.Interval
import Control.Auto.Run
import Control.Category
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Prelude hiding             ((.), id)

-- | A simple 'Auto' that ignores all input; its output stream counts
-- upwards from zero.
--
-- >>> take 10 . streamAuto' count $ repeat ()
-- [0,1,2,3,4,5,6,7,8,9]
count :: (Serialize b, Num b) => Auto m a b
count = iterator (+1) 0

-- | A non-resuming/non-serializing version of 'count'.
count_ :: Num b => Auto m a b
count_ = iterator_ (+1) 0

-- | An 'Auto' that returns the last value received by it.  Given an
-- "initial value" to output first.
--
-- From the signal processing world, this is known as the "lag operator"
-- /L/.
--
-- This is (potentially) a __very dangerous__ 'Auto', because its usage and
-- its very existence opens the door to breaking denotative/declarative
-- style and devolving into imperative style coding.  However, when used
-- where it is supposed to be used, it is more or less invaluable, and will
-- be an essential part of many programs.
--
-- Its main usage is for dealing with recursive bindings.  If you ever are
-- laying out recursive bindings in a high-level/denotative way, you need
-- to have at least one value be able to have a "initial output" without
-- depending on anything else.  'lastVal' and 'delay' allow you to do this.
--
-- See the <https://github.com/mstksg/auto-examples/blob/master/src/Recursive.hs recursive>
-- example for more information on the appropriate usage of 'lastVal' and
-- 'delay'.
--
-- >>> streamAuto' (lastVal 100) [1..10]
-- [100,1,2,3,4,5,6,7,8,9]
lastVal :: Serialize a
        => a              -- ^ initial value
        -> Auto m a a
lastVal = mkState $ \x s -> (s, x)
{-# INLINE lastVal #-}

-- | The non-resuming/non-serializing version of 'lastVal'.
lastVal_ :: a             -- ^ initial value
         -> Auto m a a
lastVal_ = mkState_ $ \x s -> (s, x)
{-# INLINE lastVal_ #-}

-- | Like 'arr', but applies the function to the /previous value/ of the
-- input, instead of the current value.  Used for the same purposes as
-- 'lastVal': to manage recursive bindings.
--
-- Warning: Don't use this to do imperative programming!
--
-- prop> arrD id == lastVal
--
-- >>> streamAuto' (arrD negate 100) [1..10]
-- [100,-1,-2,-3,-4,-5,-6,-7,-8,-9]
arrD :: Serialize b
     => (a -> b)        -- ^ function to apply
     -> b               -- ^ initial value
     -> Auto m a b
arrD f = mkState $ \x s -> (s, f x)

-- | The non-resuming/non-serializing version of 'arrD'.
arrD_ :: Serialize b
      => (a -> b)       -- ^ function to apply
      -> b              -- ^ initial value
      -> Auto m a b
arrD_ f = mkState_ $ \x s -> (s, f x)

-- | An alias for 'lastVal'; used in contexts where "delay" is more
-- a meaningful description than "last value".  All of the warnings for
-- 'lastVal' still apply, so you should probably read it if you haven't :)
delay :: Serialize a
      => a                -- ^ initial value
      -> Auto m a a
delay = lastVal
{-# INLINE delay #-}

-- | The non-resuming/non-serializing version of 'delay'.
delay_ :: a               -- ^ initial value
       -> Auto m a a
delay_ = lastVal_
{-# INLINE delay_ #-}

-- | Like 'delay', except has as many "initial values" as the input list.
-- Outputs every item in the input list in order before returning the first
-- received value.
--
-- prop> delayList [y0] = delay y0
--
-- >>> streamAuto' (delayList [3,5,7,11]) [1..10]
-- [3,5,7,11,1,2,3,4,5,6]
delayList :: (Serialize a, Monad m)
          => [a]            -- ^ items to delay with (initial values)
          -> Auto m a a
delayList = foldr (\x a -> delay x . a) id

-- | The non-resuming/non-serializing version of 'delayList'.
delayList_ :: Monad m
           => [a]           -- ^ items to delay with (initial values)
           -> Auto m a a
delayList_ = foldr (\x a -> delay_ x . a) id

-- | Like 'delay', except delays the desired number of steps with the same
-- initial output value.
--
-- prop> delayN n x0 = delayList (replicate n x0)
--
-- prop> delayN 1 x0 = delay x0
--
-- >>> streamAuto' (delayN 3 0) [1..10]
-- [0,0,0,1,2,3,4,5,6,7]
delayN :: (Serialize a, Monad m)
       => Int             -- ^ number of steps to delay
       -> a               -- ^ initial value(s)
       -> Auto m a a
delayN n y0 = iterate (delay y0 .) id !! n

-- | The non-resuming/non-serializing version of 'delayN'
delayN_ :: Monad m
        => Int            -- ^ number of steps to delay
        -> a              -- ^ initial value(s)
        -> Auto m a a
delayN_ n y0 = iterate (delay_ y0 .) id !! n

-- | "stretch" an 'Auto' out, slowing time.  @'stretch' n a@ will take one
-- input, repeat the same output @n@ times (ignoring input), and then take
-- another.  It ignores all inputs in between.
--
-- >>> let a = stretch 2 (sumFrom 0)
-- >>> streamAuto' a [1,8,5,4,3,7,2,0]
--    [1,1,6,6,9,9,11,11]
-- -- [1,_,5,_,3,_,2 ,_ ] <-- the inputs
stretch :: (Serialize b, Monad m)
        => Int              -- ^ stretching factor
        -> Auto m a b       -- ^ 'Auto' to stretch
        -> Auto m a b
stretch = stretchBy id


-- | The non-resuming/non-serializing version of 'stretch'.
stretch_ :: Monad m
         => Int               -- ^ stretching factor
         -> Auto m a b        -- ^ 'Auto' to stretch
         -> Auto m a b
stretch_ = stretchBy_ id

stretchBy :: (Serialize b, Monad m)
          => (b -> b)
          -> Int
          -> Auto m a b
          -> Auto m a b
stretchBy f n = go (1, Nothing)
  where
    go (i, y) a = mkAutoM (go <$> get <*> resumeAuto a)
                          (put (i, y) *> saveAuto a)
                        $ \x ->
                            if i <= 1
                               then do
                                 (y', a') <- stepAuto a x
                                 return (y', go (n, Just y') a')
                               else
                                 return (f (fromJust y), go (i - 1, y) a)

stretchBy_ :: (Monad m)
           => (b -> b)
           -> Int
           -> Auto m a b
           -> Auto m a b
stretchBy_ f n = go (1, Nothing)
  where
    go (i, y) a = mkAutoM_ $ \x ->
                               if i <= 1
                                  then do
                                    (y', a') <- stepAuto a x
                                    return (y', go (n, Just y') a')
                                  else
                                    return (f (fromJust y), go (i - 1, y) a)

-- | A more general version of 'stretch'; instead of just ignoring and
-- dropping the "stretched/skipped intervals", accumulate all of them up
-- with the given accumulating function and then "step" them all at once on
-- every @n@th tick.  Also, stead of returning exactly the same output
-- every time over the stretched interval, output a function of the
-- original output during the stretched intervals.
--
-- >>> streamAuto' (sumFrom 0) [1..10]
-- [1, 3, 6, 10, 15, 21, 28, 36, 45 ,55]
-- >>> streamAuto' (stretchAccumBy (+) negate 4 (sumFrom 0)) [1..10]
-- [1,-1,-1, -1, 15,-15,-15,-15, 45,-45]
--
-- Here, instead of feeding in a number every step, it "accumulates" all of
-- the inputs using '+' and "blasts them into" @'sumFrom' 0@ every 4 steps.
-- In between the blasts, it outputs the negated last seen result.
--
-- You can recover the behavior of 'stretch' with
-- @'stretchAccumBy' (flip const) id@.
--
stretchAccumBy :: (Serialize a, Serialize b, Monad m)
               => (a -> a -> a)
               -> (b -> b)
               -> Int
               -> Auto m a b
               -> Auto m a b
stretchAccumBy fx fy n = go (1, Nothing, Nothing)
  where
    go (i, x0, y) a = mkAutoM (go <$> get <*> resumeAuto a)
                              (put (i, x0, y) *> saveAuto a)
                            $ \x ->
                                if i <= 1
                                  then do
                                    (y', a') <- stepAuto a . fromJust $ acm x0 x
                                    return (y', go (n, Nothing, Just y') a')
                                  else
                                    return (fy (fromJust y), go (i-1, acm x0 x, y) a)
    acm Nothing  x = Just x
    acm (Just x) y = Just (fx x y)

-- | The non-serialized/non-resuming version of 'stretchAccumBy'.
stretchAccumBy_ :: Monad m
                => (a -> a -> a)
                -> (b -> b)
                -> Int
                -> Auto m a b
                -> Auto m a b
stretchAccumBy_ fx fy n = go (1, Nothing, Nothing)
  where
    go (i, x0, y) a = mkAutoM_ $ \x ->
                                   if i <= 1
                                     then do
                                       (y', a') <- stepAuto a . fromJust $ acm x0 x
                                       return (y', go (n, Nothing, Just y') a')
                                     else
                                       return (fy (fromJust y), go (i-1, acm x0 x, y) a)
    acm Nothing  x = Just x
    acm (Just x) y = Just (fx x y)


-- | Like 'stretch', but instead of holding the the "stretched" outputs,
-- outputs a blip stream that emits every time the stretched 'Auto'
-- "progresses" (every @n@ ticks)
--
-- See 'stretch' for more information.
--
-- >>> let a = stretchB 2 (accum (+) 0)
-- >>> streamAuto' a [1,8,5,4,3,7,2,0]
-- [Blip 1, NoBlip, Blip 6, NoBlip, Blip 9, NoBlip, Blip 11, NoBlip]
--
stretchB :: Monad m
         => Int                 -- ^ stretching factor
         -> Auto m a b          -- ^ 'Auto' to stretch
         -> Auto m a (Blip b)
stretchB (max 1 -> n) = go 1
  where
    go i a = mkAutoM (go <$> get <*> resumeAuto a)
                     (put i *> saveAuto a)
                     $ \x ->
                         if i <= 1
                           then do
                             (y, a') <- stepAuto a x
                             return (Blip y, go n       a')
                           else
                             return (NoBlip, go (i - 1) a )

-- | "Accelerates" the 'Auto', so instead of taking an @a@ and returning
-- a @b@, it takes a list of @a@, "streams" the 'Auto' over each one, and
-- returns a list of @b@ results.
--
-- For example, if you normally feed @'sumFrom' 0@ a 1, then a 2, then a 3,
-- you'd get a 1, then a 3, then a 6.  But if you feed
-- @'accelOverList' ('sumFrom' 0)@ a @[1,2]@, you'd get a @[1,3]@, and if
-- you fed it a @[3]@ after, you'd get a @[6]@.
--
-- Turns a @[a] -> [b]@ into an @[[a]] -> [[b]]@; if you "chunk up" the
-- input stream @a@s into chunks of input to feed all at once, the outputs
-- @b@ will be chunked up the same way.
--
-- >>> streamAuto' (sumFrom 0) [1,2,3,4,5,6,7,8]
-- [1,3,6,10,15,21,28,36]
-- >>> streamAuto' (accelOverList (sumFrom 0)) [[1,2],[],[3,4,5],[6],[7,8]]
-- [[1,3],[],[6,10,15],[21],[28,36]]
--
-- Mostly useful if you want to feed an 'Auto' multiple inputs in the same
-- step.  Note that if you always feed in singleton lists (lists with one
-- item), you'll more or less get the same behavior as normal.
--
accelOverList :: Monad m
              => Auto m a b       -- ^ 'Auto' to accelerate
              -> Auto m [a] [b]
accelOverList = go
  where
    go a0 = mkAutoM (go <$> resumeAuto a0)
                    (saveAuto a0)
                    $ \xs -> do
                        (a1, ysEndo) <- runWriterT (wr a0 xs)
                        let ys = appEndo ysEndo []
                        return (ys, go a1)
    wr a0 []     = return a0
    wr a0 (x:xs) = do
        (y, a1) <- lift $ stepAuto a0 x
        tell $ Endo (y:)      -- using a diff list for performace;
                              -- this is basically `tell [y]`
        wr a1 xs

-- | @'accelerate' n a@ turns an 'Auto' @a@ into an "accelerated" 'Auto',
-- where every input is fed into the 'Auto' @n@ times.  All of the results
-- are collected in the output.
--
-- The same input is fed repeatedly @n@ times.
--
-- >>> streamAuto' (accelerate 3 (sumFrom 0)) [2,3,4]
-- [[2,4,6],[9,12,15],[19,23,27]]
-- -- ^adding 2s  ^adding 3s ^adding 4s
--
accelerate :: Monad m
           => Int             -- ^ acceleration factor
           -> Auto m a b      -- ^ 'Auto' to accelerate
           -> Auto m a [b]
accelerate n = go
  where
    n'    = max n 1
    go a0 = mkAutoM (go <$> resumeAuto a0)
                    (saveAuto a0)
                    $ \x0 -> do
                        yas <- flip (iterateM n') (undefined, a0)
                               $ \(_, a) -> do
                                   (x, a') <- stepAuto a x0
                                   x `seq` return (x, a')
                        let ys = map fst yas
                            a' = snd (last yas)
                        return (ys, go a')
{-# INLINE accelerate #-}

-- | @'accelerateWith' xd n a@ is like @'accelerate' n a@, except instead
-- of feeding in the input @n@ times, it feeds the input in once and
-- repeats the "filler" @xd@ for the rest of the accelerating period.
--
-- >>> streamAuto' (accelerateWith (-1) 3 (sumFrom 0)) [1,10,100]
-- [[1,0,-1],[9,8,7],[107,106,105]]
-- -- ^ feed in 1 once and -1 twice
-- --          ^ feed in 10 once and -1 twice
-- --                  ^ feed in 100 once and -1 twice
accelerateWith :: Monad m
               => a               -- ^ default input value, during acceleration periods
               -> Int             -- ^ acceleration factor
               -> Auto m a b      -- ^ 'Auto' to accelerate
               -> Auto m a [b]
accelerateWith xd n | n <= 1    = fmap (:[])
                    | otherwise = go
  where
    n'    = n - 1
    go a0 = mkAutoM (go <$> resumeAuto a0)
                    (saveAuto a0)
                    $ \x0 -> do
                        (y0, a1) <- stepAuto a0 x0
                        yas <- flip (iterateM n') (undefined, a1)
                               $ \(_, a) -> do
                                   (x, a') <- stepAuto a xd
                                   return (x, a')
                        let ys = y0 : map fst yas
                            a' = snd (last yas)
                        return (ys, go a')

-- | Takes an 'Auto' that produces @(b, 'Blip' c)@, and turns it into an
-- 'Auto' that produces @([b], c)@.
--
-- Basically, the new 'Auto' "squishes together" the periods of output
-- between each time the blip stream emits.  All outputs between each
-- emitted value are accumulated and returned in the resulting @[b]@.
--
-- It "does this" in the same manner as 'accelerateWith' and 'fastForward':
-- first feed the input, then step repeatedly with the default input value.
--
-- >>> let a :: Auto' Int (Int, Blip String)
--         a = proc i -> do
--                 sums <- sumFrom 0 -< i
--                 blp  <- every 3   -< i     -- emits every 3 ticks.
--                 id    -< (sums, sums <& blp) -- replace emitted value
--                                              -- with the running sum
-- >>> let skipA :: Auto' Int ([Int], String)
--         skipA = skipTo (-1) a
-- >>> let (res1, skipA') = stepAuto' skipA 8
-- >>> res1
-- ([8,7,6], 6)     -- fed 8 first, then (-1) repeatedly
-- >>> let (res2, _     ) = evalAuto skipA' 5
-- >>> res2
-- ([11,10,9], 9)   -- fed 5 first, then (-1) repeatedly
--
-- If the blip stream never emits then stepping this and getting the result
-- or the next/updated 'Auto' never terminates...so watch out!
--
skipTo :: Monad m
       => a                       -- ^ default input value, during
                                  --     skipping periods
       -> Auto m a (b, Blip c)    -- ^ 'Auto' to skip over, until each time
                                  --     the blip stream emits
       -> Auto m a ([b], c)
skipTo x0 = go
  where
    -- go :: Auto m a (b, Blip c)
    --    -> Auto m a ([b], c)
    go a0 = mkAutoM (go <$> resumeAuto a0)
                    (saveAuto a0)
                    $ \x -> do
                      ((ys, z), a1) <- skipOut a0 x []
                      return ((reverse ys, z), go a1)
    -- skipOut :: Auto m a (b, Blip c)
    --         -> a
    --         -> [b]
    --         -> m (([b], c), Auto m a (b, Blip c))
    skipOut a0 x ys = do
      ((y, bz), a1) <- stepAuto a0 x
      let ys' = y:ys
      case bz of
        Blip z -> return ((ys', z), a1)
        NoBlip -> skipOut a1 x0 ys'

-- | Turns an @'Interval' m a b@ into an @'Auto' m a b@ --- that is, an
-- @'Auto' m a (Maybe b)@ into an @'Auto' m a b@.
--
-- It does this by "skipping over" all "off"/'Nothing' input.  When the
-- result "should" be a 'Nothing', it re-runs the 'Interval' over and over
-- again with the given default input until the 'Auto' turns back "on"
-- again (outputs a 'Just').
--
-- If the 'Interval' reaches a point where it will never be "on" again,
-- stepping this and getting the result or the next/updated 'Auto' won't
-- terminate...so watch out!
--
-- >>> let a1 = offFor 3 . sumFrom 0
-- >>> streamAuto' a1 [1..10]
-- [Nothing, Nothing, Nothing, Just 10, Just 15, Just 21]
-- >>> streamAuto' (fastForward 0 a1) [1..6]
-- [1,3,6,10,15,21]
-- >>> streamAuto' (fastForward (-10) a1) [1..6]
-- [-29,-27,-24,-20,-15,-9]
--
-- In that last example, the first input is 1, then it inputs (-10) until
-- it is "on"/'Just' again (on the fourth step).  Then continues imputing
-- 2, 3, 4 etc.
--
fastForward :: Monad m
            => a                  -- ^ default input
            -> Interval m a b     -- ^ 'Interval' to fastforward (past each "off" period, or 'Nothing')
            -> Auto m a b
fastForward x0 = go
  where
    -- go :: Auto m a (Maybe b)
    --    -> Auto m a b
    go a0 = mkAutoM (go <$> resumeAuto a0)
                    (saveAuto a0)
                    (skipNothings a0)
    -- skipNothings :: Auto m a (Maybe b) -> a -> m (b, Auto m a b)
    skipNothings a0 x = do
      (my, a1) <- stepAuto a0 x
      case my of
        Nothing -> skipNothings a1 x0
        Just y  -> return (y, go a1)

-- | Same behavior as 'fastForward', except accumulates all of the @'Left'
-- c@ outputs in a list.
fastForwardEither :: Monad m
                  => a                        -- ^ default input
                  -> Auto m a (Either c b)    -- ^ 'Auto' to fast-forward (past each 'Left')
                  -> Auto m a (b, [c])
fastForwardEither x0 = fmap (second reverse) . go
  where
    -- go :: Auto m a (Either c b)
    --    -> Auto m a (b, [c])
    go a0 = mkAutoM (go <$> resumeAuto a0)
                    (saveAuto a0)
                    (skipNothings a0 [])
    -- skipNothings :: Auto m a (Either c b)
    --              -> [c]
    --              -> a
    --              -> m ((b, [c]), Auto m a (b, [c]))
    skipNothings a0 zs x = do
      (ey, a1) <- stepAuto a0 x
      case ey of
        Left z  -> skipNothings a1 (z:zs) x0
        Right y -> return ((y, zs), go a1)

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM n f = go (max n 0)
  where
    go 0 _ = return []
    go i x = do
      x' <- f x
      xs <- go (i - 1) x'
      return (x' : xs)

-- | When first asked for output, "primes" the 'Auto' first by streaming it
-- with all of the given inputs first before processing the first input.
-- Aterwards, behaves like normal.
--
-- >>> streamAuto' (priming [1,2,3] (sumFrom 0)) [1..10]
-- [7,9,12,16,21,27,34,42,51,61]
--
-- The 'Auto' behaves as if it had already "processed" the @[1,2,3]@,
-- resulting in an accumulator of 6, before it starts taking in any input.
--
-- Normally this would be silly with an 'Auto'', because the above is the
-- same as:
--
-- >>> let (_, a) = overList' (sumFrom 0) [1,2,3]
-- >>> streamAuto' a [1..10]
-- [7,9,12,16,21,27,34,42,51,61]
--
-- This becomes somewhat more useful when you have "monadic" 'Auto's, and
-- want to defer the execution until during normal stepping:
--
-- >>> _ <- streamAuto (priming [1,2,3] (arrM print)) [10,11,12]
-- 1    -- IO effects
-- 2
-- 3
-- 10
-- 11
-- 12
priming :: Monad m
        => [a]          -- ^ inputs to prime with
        -> Auto m a b   -- ^ 'Auto' to prime
        -> Auto m a b
priming xs a0 = mkAutoM l
                        (put False)
                      $ \x -> do
                          (_, a1) <- overList a0 xs
                          (y, a2) <- stepAuto a1 x
                          return (y, primed a2)
  where
    primed a1 = mkAutoM l
                (put True *> saveAuto a1)
              $ \x -> do
                  (y, a2) <- stepAuto a1 x
                  return (y, primed a2)
    l = do
      flag <- get
      if flag
        then primed <$> resumeAuto a0
        else return $ priming xs a0

