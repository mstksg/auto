-- |
-- Module      : Control.Auto.Process
-- Description : 'Auto's useful for various commonly occurring processes.
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Various 'Auto's for miscellaneous common processes.
--
-- Note that all of these can be turned into an equivalent version acting
-- on 'Blip' streams, with 'perBlip':
--
-- @
-- 'sumFrom'         :: ('Serialize' a, 'Num' a) => a -> 'Auto' m a a
-- 'perBlip' 'sumFrom' :: ('Serialize' a, 'Num' a) => a -> 'Auto' m ('Blip' a) ('Blip' a)
-- @
--
module Control.Auto.Process (
  -- * Numerical
    sumFrom
  , sumFrom_
  , sumFromD
  , sumFromD_
  , deltas
  , deltas_
  -- * Monoidal/Semigroup
  , mappender
  , mappender_
  , mappendFrom
  , mappendFrom_
  ) where

import Control.Auto.Core
import Data.Semigroup
import Data.Serialize

-- | Outputs the running sum of all items passed so far, starting with an
-- initial count.
--
-- The first output is the sum of the first input with the initial count.
-- See the documentation for 'sumFromD' for more information.
--
-- prop> sumFrom x0 = mkAccum (+) x0
sumFrom :: (Serialize a, Num a)
        => a             -- ^ initial count
        -> Auto m a a
sumFrom = mkAccum (+)

-- | The non-resuming/non-serializing version of 'sumFrom'.
sumFrom_ :: Num a
         => a             -- ^ initial count
         -> Auto m a a
sumFrom_ = mkAccum_ (+)

-- | Like 'sumFrom', except the first output is the starting count.
--
-- >>> let a = sumFromD 5
-- >>> let Output y1 a' = stepAuto' a 10
-- >>> y1
-- 5
-- >>> let Output y2 _  = stepAuto' a' 3
-- >>> y2
-- 10
--
-- It's 'sumFrom', but "delayed".
--
-- Useful for recursive bindings, where you need at least one value to be
-- able to produce its "first output" without depending on anything else.
--
-- prop> sumFromD x0 = delay x0 . sumFrom x0
sumFromD :: (Serialize a, Num a)
         => a             -- ^ initial count
         -> Auto m a a
sumFromD = mkAccumD (+)

-- | The non-resuming/non-serializing version of 'sumFromD'.
sumFromD_ :: Num a
          => a             -- ^ initial count
          -> Auto m a a
sumFromD_ = mkAccumD_ (+)

-- | Returns the difference between the received input and the previous
-- input.  The first result is 'Nothing'; if you have something you want
-- the first result to be, you can use '<|!>' from
-- "Control.Auto.Interval", or just 'fromMaybe' or 'maybe' from
-- "Data.Maybe".
--
-- >>> let a = deltas
-- >>> let Output y1 a'  = stepAuto' a 5
-- >>> y1
-- Nothing
-- >>> let Output y2 a'' = stepAuto' a' 7
-- >>> y2
-- Just 2
-- >>> let Output y3 _   = stepAuto' a'' 4
-- >>> y3
-- Just (-3)
--
-- Usage with '<|!>':
--
-- >>> let a = deltas <|!> pure 100
-- >>> let (ys, _) = overList' a [5,7,4]
-- >>> ys
-- [100, 2, -3]
--
-- Usage with 'fromMaybe':
--
-- >>> let a = fromMaybe 100 <$> deltas
-- >>> let (ys, _) = overList' a [5,7,4]
-- >>> ys
-- [100, 2, -3]
--
deltas :: (Serialize a, Num a) => Auto m a (Maybe a)
deltas = mkState _deltasF Nothing

-- | The non-resuming/non-serializing version of 'deltas'.
deltas_ :: Num a => Auto m a (Maybe a)
deltas_ = mkState_ _deltasF Nothing

_deltasF :: Num a => a -> Maybe a -> (Maybe a, Maybe a)
_deltasF x s = case s of
                 Nothing -> (Nothing     , Just x)
                 Just y  -> (Just (y - x), Just y)

-- | Accumulates the monoid sum of all inputs, starting with 'mempty'.
--
-- >>> let a = mappender
-- >>> let Output y1 a'  = stepAuto' a (Last (Just 4))
-- >>> y1
-- Last (Just 4)
-- >>> let Output y2 a'' = stepAuto' a' (Last Nothing)
-- >>> y2
-- Last (Just 4)
-- >>> let Output y3 _   = stepAuto' a'' (Last (Just 2))
-- >>> y3
-- Last (Just 2)
--
-- prop> mappender = mkAccum mappend mempty
mappender :: (Serialize a, Monoid a) => Auto m a a
mappender = mkAccum mappend mempty

-- | The non-resuming/non-serializing version of 'mappender'.
mappender_ :: Monoid a => Auto m a a
mappender_ = mkAccum_ mappend mempty

-- | Accumulates the semigroup sum of all of its inputs; the same as
-- 'mappender', but you have to provide a "starting" value.
--
-- >>> let a = mappendFrom (Max 0)
-- >>> let Output y1 a' = stepAuto' a (Max 3)
-- >>> y1
-- Max 3
-- >>> let Output y2 _  = stepAuto' a' (Max (-2))
-- >>> y2
-- Max 3
--
-- prop> mappendFrom m0 = mkAccum (<>) m0
mappendFrom :: (Serialize a, Semigroup a) => a -> Auto m a a
mappendFrom = mkAccum (<>)

-- | The non-resuming/non-serializing version of 'mappender'.
mappendFrom_ :: Semigroup a => a -> Auto m a a
mappendFrom_ = mkAccum_ (<>)
