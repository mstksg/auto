-- |
-- Module      : Control.Auto.Process
-- Description : 'Auto's useful for various commonly occurring processes.
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Various 'Auto's describing relationships following common processes,
-- like 'sumFrom', whose output is the cumulative sum of the input.
--
-- Note that all of these can be turned into an equivalent version acting
-- on blip streams, with 'perBlip':
--
-- @
-- 'sumFrom' n           :: 'Num' a => 'Auto' m a a
-- 'perBlip' ('sumFrom' n) :: 'Num' a => 'Auto' m ('Blip' a) ('Blip' a)
-- @
--
module Control.Auto.Process (
  -- * Numerical
    sumFrom
  , sumFrom_
  , sumFromD
  , sumFromD_
  , productFrom
  , productFrom_
  , deltas
  , deltas_
  -- * Monoidal/Semigroup
  , mappender
  , mappender_
  , mappendFrom
  , mappendFrom_
  ) where

import Control.Auto.Core
import Control.Auto.Interval
import Data.Semigroup
import Data.Serialize

-- | The stream of outputs is the cumulative/running sum of the inputs so
-- far, starting with an initial count.
--
-- The first output takes into account the first input.  See 'sumFromD' for
-- a version where the first output is the initial count itself.
--
-- prop> sumFrom x0 = accum (+) x0
sumFrom :: (Serialize a, Num a)
        => a             -- ^ initial count
        -> Auto m a a
sumFrom = accum (+)

-- | The non-resuming/non-serializing version of 'sumFrom'.
sumFrom_ :: Num a
         => a             -- ^ initial count
         -> Auto m a a
sumFrom_ = accum_ (+)

-- | Like 'sumFrom', except the first output is the starting count.
--
-- >>> let a = sumFromD 5
-- >>> let (y1, a') = stepAuto' a 10
-- >>> y1
-- 5
-- >>> let (y2, _ ) = stepAuto' a' 3
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
sumFromD = accumD (+)

-- | The non-resuming/non-serializing version of 'sumFromD'.
sumFromD_ :: Num a
          => a             -- ^ initial count
          -> Auto m a a
sumFromD_ = accumD_ (+)

-- | The output is the running/cumulative product of all of the inputs so
-- far, starting from an initial product.
--
-- prop> productFrom x0 = accum (*) x0
productFrom :: (Serialize a, Num a)
            => a            -- ^ initial product
            -> Auto m a a
productFrom = accum (*)

-- | The non-resuming/non-serializing version of 'productFrom'.
productFrom_ :: Num a
             => a           -- ^ initial product
             -> Auto m a a
productFrom_ = accum_ (*)

-- | The output is the the difference between the input and the previously
-- received input.
--
-- First result is a 'Nothing', so you can use '<|!>' or 'fromInterval' or
-- 'fromMaybe' to get a "default first value".
--
-- >>> streamAuto' deltas [1,6,3,5,8]
-- >>> [Nothing, Just 5, Just (-3), Just 2, Just 3]
--
-- Usage with '<|!>':
--
-- >>> let a = deltas <|!> pure 100
-- >>> streamAuto' (deltas <|!> pure 100) [1,6,3,5,8]
-- [100, 5, -3, 2, 3]
--
-- Usage with 'fromMaybe':
--
-- >>> streamAuto' (fromMaybe 100 <$> deltas) [1,6,3,5,8]
-- [100, 5, -3, 2, 3]
--
deltas :: (Serialize a, Num a) => Interval m a a
deltas = mkState _deltasF Nothing

-- | The non-resuming/non-serializing version of 'deltas'.
deltas_ :: Num a => Interval m a a
deltas_ = mkState_ _deltasF Nothing

_deltasF :: Num a => a -> Maybe a -> (Maybe a, Maybe a)
_deltasF x s = case s of
                 Nothing -> (Nothing     , Just x)
                 Just y  -> (Just (y - x), Just y)

-- | The output is the running/cumulative 'mconcat' of all of the input
-- seen so far, starting with 'mempty'.
--
-- >>> streamauto' mappender . map Last $ [Just 4, Nothing, Just 2, Just 3]
-- [Last (Just 4), Last (Just 4), Last (Just 2), Last (Just 3)]
-- >>> streamAuto' mappender ["hello","world","good","bye"]
-- ["hello","helloworld","helloworldgood","helloworldgoodbye"]
--
-- prop> mappender = accum mappend mempty
mappender :: (Serialize a, Monoid a) => Auto m a a
mappender = accum mappend mempty

-- | The non-resuming/non-serializing version of 'mappender'.
mappender_ :: Monoid a => Auto m a a
mappender_ = accum_ mappend mempty

-- | The output is the running '<>'-sum ('mappend' for 'Semigroup') of all
-- of the input values so far, starting with a given starting value.
-- Basically like 'mappender', but with a starting value.
--
-- >>> streamAuto' (mappendFrom (Max 0)) [Max 4, Max (-2), Max 3, Max 10]
-- [Max 4, Max 4, Max 4, Max 10]
--
-- prop> mappendFrom m0 = accum (<>) m0
mappendFrom :: (Serialize a, Semigroup a)
            => a            -- ^ initial value
            -> Auto m a a
mappendFrom = accum (<>)

-- | The non-resuming/non-serializing version of 'mappender'.
mappendFrom_ :: Semigroup a
             => a           -- ^ initial value
             -> Auto m a a
mappendFrom_ = accum_ (<>)
