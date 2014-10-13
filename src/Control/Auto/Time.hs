{-# LANGUAGE ViewPatterns #-}

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
-- TODO: dropping first few elements
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
  , delayList
  , delayList_
  , delayN
  , delayN_
  -- ** Stretching
  , stretch
  , stretch_
  , stretchB
  -- ** Accelerating
  , accelerate
  , accelerateWith
  , accelOverList
  -- ** Skipping
  -- $skippers
  , skipTo
  , fastForward
  , fastForwardEither
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Auto.Generate
import Control.Category
import Control.Monad
import Control.Monad.Loops
import Data.Serialize
import Prelude hiding             ((.), id)

-- | A simple 'Auto' that outputs the step count.  First output is 0.
--
-- TODO: should be 1?
count :: (Serialize b, Num b) => Auto m a b
count = iterator (+1) 0

-- | A non-resuming/non-serializing version of 'count'.
count_ :: Num b => Auto m a b
count_ = iterator_ (+1) 0

-- | An 'Auto' that returns the last value received by it.  Given an
-- "initial value" to output first.
--
-- This is (potentially) a __very dangerous__ 'Auto', because its usage and
-- its very existence opens the door to breaking denotative/declarative
-- style and devolving into imperative style coding.  However, when used
-- where it is supposed to be used, it is more or less invaluable, and will
-- be an essential part of many programs.
--
-- Its main usage is for dealing with bindings.  If you ever are laying out
-- recursive bindings in a high-level/denotative way, you need to have at
-- least one value be able to have a "initial output" without depending on
-- anything else.  'lastVal' and 'delay' allow you to do this.
--
-- See the <https://github.com/mstksg/auto-examples/blob/master/src/Recursive.hs recursive>
-- example for more information on the appropriate usage of 'lastVal' and
-- 'delay'.
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
delayList :: (Serialize a, Monad m)
          => [a]
          -> Auto m a a
delayList = foldr ((>>>) . delay) id

-- | The non-resuming/non-serializing version of 'delayList'.
delayList_ :: Monad m
           => [a]
           -> Auto m a a
delayList_ = foldr ((>>>) . delay_) id

-- | Like 'delay', except delays the desired number of steps with the same
-- initial output value.
--
-- prop> delayN n x0 = delayList (replicate n x0)
--
-- prop> delayN 1 x0 = delay x0
delayN :: (Serialize a, Monad m)
       => Int
       -> a
       -> Auto m a a
delayN n y0 = iterate (delay y0 .) id !! n

-- | The non-resuming/non-serializing version of 'delayN'
delayN_ :: Monad m
        => Int
        -> a
        -> Auto m a a
delayN_ n y0 = iterate (delay_ y0 .) id !! n

-- | "stretch" an 'Auto' out, slowing time.  @'stretch' n a@ will take one
-- input, repeat the same output @n@ times (ignoring input), and then take
-- another.
--
-- >>> let a       = stretch 2 (mkAccum (+) 0)
-- >>> let (ys, _) = overList' a [1,8,5,4,3,7,2,0]
-- >>> ys
--    [1,1,6,6,9,9,11,11]
-- -- [1,_,5,_,3,_,2 ,_ ] <-- the inputs
stretch :: (Serialize b, Monad m)
        => Int              -- ^ stretching factor
        -> Auto m a b       -- ^ 'Auto' to stretch
        -> Auto m a b
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
stretch_ :: Monad m
         => Int               -- ^ stretching factor
         -> Auto m a b        -- ^ 'Auto' to stretch
         -> Auto m a b
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
stretchB :: Monad m
         => Int                 -- ^ stretching factor
         -> Auto m a b          -- ^ 'Auto' to stretch
         -> Auto m a (Blip b)
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
accelOverList :: Monad m
              => Auto m a b       -- ^ 'Auto' to accelerate
              -> Auto m [a] [b]
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
-- >>> let Output ys1 a' = stepAuto' a 5
-- >>> ys1
-- [5,10,15]
-- >>> let Output ys2 _  = stepAuto' a' (-8)
-- >>> ys2
-- [7,-1,-9]
accelerate :: Monad m
           => Int             -- ^ acceleration factor
           -> Auto m a b      -- ^ 'Auto' to accelerate
           -> Auto m a [b]
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
-- >>> let Output ys1 a' = stepAuto' a 5
-- >>> ys1
-- [5,4,3]    -- fed 5, then (-1) twice
-- >>> let Output ys2 _  = stepAuto' a' 14
-- >>> ys2
-- [17,16,15] -- fed 14, then (-1) twice
accelerateWith :: Monad m
               => a               -- ^ default input value, during acceleration periods
               -> Int             -- ^ acceleration factor
               -> Auto m a b      -- ^ 'Auto' to accelerate
               -> Auto m a [b]
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

-- $skippers
--
-- TODO: say something

-- | Takes an 'Auto' that produces @(b, 'Blip' c)@, and turns it into an
-- 'Auto' that produces @([b], c)@.
--
-- Basically, the new 'Auto' "squishes together" the periods of output
-- between each 'Blip' occurrnece.  All outputs between each 'Blip'
-- occurrence are accumulated and returned in the resulting @[b]@.
--
-- It "does this" in the same manner as 'accelerateWith' and 'fastForward':
-- first feed the input, then step repeatedly with the default input value.
--
-- >>> let a :: Auto' Int (Int, Blip String)
--         a = proc i -> do
--                 sums <- mkAccum (+) 0 -< i
--                 blp  <- inB 3         -< i     -- Blip at every 3 ticks.
--                 id    -< (sums, show <$> blp)
-- >>> let skipA :: Auto' Int ([Int], String)
--         skipA = skipTo (-1) a
-- >>> let Output res1 skipA' = stepAuto' skipA 8
-- >>> res1
-- ([8,7,6], "6")     -- fed 8 first, then (-1) repeatedly
-- >>> let Output res2 _      = stepAuto' skipA' 5
-- >>> res2
-- ([11,10,9], "9")   -- fed 5 first, then (-1) repeatedly
--
skipTo :: Monad m
       => a                       -- ^ default input value, during skipping periods
       -> Auto m a (b, Blip c)    -- ^ 'Auto' to skip over, until each 'Blip'.
       -> Auto m a ([b], c)
skipTo x0 = go
  where
    -- go :: Auto m a (b, Blip c)
    --    -> Auto m a ([b], c)
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x -> do
                      ((ys, z), a1) <- skipOut a0 x []
                      return (Output (reverse ys, z) (go a1))
    -- skipOut :: Auto m a (b, Blip c)
    --         -> a
    --         -> [b]
    --         -> m (([b], c), Auto m a (b, Blip c))
    skipOut a0 x ys = do
      Output (y, bz) a1 <- stepAuto a0 x
      let ys' = y:ys
      case bz of
        Blip z -> return ((ys', z), a1)
        NoBlip -> skipOut a1 x0 ys'

-- | Takes an 'Auto' and basically "removes" the periods in which it
-- returns 'Nothing', by re-running the 'Auto' with the same input until it
-- outputs a 'Just'.
--
-- Basically, it magically turns an @'Auto' m a ('Maybe' b)@ into an
-- @'Auto' m a b@, by skipping over each 'Nothing'.
--
-- It does this in the same manner as 'accelerateWith' and 'skipTo': first
-- feed in the input, then step repeatedly with the default input value.
--
-- >>> let a :: Auto' Int (Maybe Int)
--         a = proc i -> do
--                 sums <- mkAccum (+) 0 -< i
--                 id    -< if (i `mod` 5) == 0
--                            then Just i
--                            else Nothing
-- >>> let ffA :: Auto' Int Int
--         ffA = fastForward (-1) a
-- >>> let Output y1 ffA' = stepAuto' ffA 7
-- >>> y1
-- 5          -- went from 7 (Nothing), to 6 (Nothing), to 5 (Just 5)
-- >>> let Output y2 _    = stepAuto' ffA' (-9)
-- -5         -- went from -4 (Nothing) to -5 (Just (-5))
--
fastForward :: Monad m
            => a                      -- ^ default input
            -> Auto m a (Maybe b)     -- ^ 'Auto' to fastforward (past each 'Nothing')
            -> Auto m a b
fastForward x0 = go
  where
    -- go :: Auto m a (Maybe b)
    --    -> Auto m a b
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    (skipNothings a0)
    -- skipNothings :: Auto m a (Maybe b) -> a -> m (Output m a b)
    skipNothings a0 x = do
      Output my a1 <- stepAuto a0 x
      case my of
        Nothing -> skipNothings a1 x0
        Just y  -> return (Output y (go a1))

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
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    (skipNothings a0 [])
    -- skipNothings :: Auto m a (Either c b)
    --              -> [c]
    --              -> a
    --              -> m (Output m a (b, [c]))
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
