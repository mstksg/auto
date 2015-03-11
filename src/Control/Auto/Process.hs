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
  -- ** Numerical signal transformations
  , movingAverage
  , movingAverage_
  , impulseResponse
  , impulseResponse_
  , autoRegression
  , autoRegression_
  , arma
  , arma_
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
                 Nothing   -> (Nothing        , Just x)
                 Just prev -> (Just (x - prev), Just x)

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

-- | The output is the sum of the past inputs, multiplied by a moving
-- window of weights.
--
-- For example, if the last received inputs are @[1,2,3,4]@ (from most
-- recent to oldest), and the window of weights is @[2,0.5,4]@, then the
-- output will be @1*2 + 0.5*2 + 4*3@, or @15@.  (The weights are assumed
-- to be zero past the end of the weight window)
--
-- The immediately received input is counted as a part of the history.
--
-- Mathematically,
-- @y_n = w_0 * x_(n-0) + w_1 + x_(n-1) + w_2 * x_(n-1) + ...@, for all
-- @w@s in the weight window, where the first item is @w_0@.  @y_n@ is the
-- @n@th output, and @x_n@ is the @n@th input.
--
-- Note that this serializes the history of the input...or at least the
-- history as far back as the entire window of weights.  (A weight list of
-- five items will serialize the past five received items)  If your weight
-- window is very long (or infinite), then serializing is a bad idea!
--
-- The second parameter is a list of a "starting history", or initial
-- conditions, to be used when the actual input history isn't long enough.
-- If you want all your initial conditions/starting history to be @0@, just
-- pass in @[]@.
--
-- Minus serialization, you can implement 'sumFrom' as:
--
-- @
-- sumFrom n = movingAverage (repeat 1) [n]
-- @
--
-- And you can implement a version of 'deltas' as:
--
-- @
-- deltas = movingAverage [1,-1] []
-- @
--
-- It behaves the same, except the first step outputs the initially
-- received value.  So it's realy a bit like
--
-- @
-- (movingAverage [1,-1] []) == (deltas <|!> id)
-- @
--
-- Where for the first step, the actual input is used instead of the delta.
--
-- Name comes from the statistical model.
--
movingAverage :: (Num a, Serialize a)
              => [a]          -- ^ weights to apply to previous inputs,
                              --     from most recent
              -> [a]          -- ^ starting history/initial conditions
              -> Auto m a a
movingAverage weights = mkState (_movingAverageF weights)

-- | The non-serializing/non-resuming version of 'movingAverage'.
movingAverage_ :: Num a
               => [a]         -- ^ weights to apply to previous inputs,
                              --     from most recent
               -> [a]         -- ^ starting history/initial conditions
               -> Auto m a a
movingAverage_ weights = mkState_ (_movingAverageF weights)

_movingAverageF :: Num a => [a] -> a -> [a] -> (a, [a])
_movingAverageF weights x hist = (sum (zipWith (*) weights hist'), hist')
  where
    hist' = zipWith const (x:hist) weights

-- | Any linear time independent stream transformation can be encoded by
-- the response of the transformation when given @[1,0,0,0...]@, or @1
-- : 'repeat' 0@.  So, given an "LTI" 'Auto', if you feed it @1 : 'repeat'
-- 0@, the output is what is called an "impulse response function".
--
-- For any "LTI" 'Auto', we can reconstruct the behavior of the original
-- 'Auto' given its impulse response.  Give 'impulseResponse' an impulse
-- response, and it will recreate/reconstruct the original 'Auto'.
--
-- >>> let getImpulseResponse a = streamAuto' a (1 : repeat 0)
-- >>> let sumFromImpulseResponse = getImpulseResponse (sumFrom 0)
-- >>> streamAuto' (sumFrom 0) [1..10]
-- [1,3,6,10,15,21,28,36,45,55]
-- >>> streamAuto' (impulseResponse sumFromImpulseResponse) [1..10]
-- [1,3,6,10,15,21,28,36,45,55]
--
-- Use this function to create an LTI system when you know its impulse
-- response.
--
-- >>> take 10 . streamAuto' (impulseResponse (map (2**) [0,-1..])) $ repeat 1
-- [1.0,1.5,1.75,1.875,1.9375,1.96875,1.984375,1.9921875,1.99609375,1.998046875]
--
-- All impulse response after the end of the given list is assumed to be
-- zero.
--
-- Mathematically,
-- @y_n = h_0 * x_(n-0) + h_1 + x_(n-1) + h_2 * x_(n-1) + ...@, for all
-- @h_n@ in the input response, where the first item is @h_0@.
--
-- Note that when this is serialized, it must serialize a number of input
-- elements equal to the length of the impulse response list...so if you give
-- an infinite impulse response, you might want to use 'impulseResponse_',
-- or not serialize.
--
-- By the way, @'impulseResponse' ir == 'movingAverage' ir []@.
--
impulseResponse :: (Num a, Serialize a)
                => [a]        -- ^ the impulse response function
                -> Auto m a a
impulseResponse weights = movingAverage weights []

-- | The non-serializing/non-resuming version of 'impulseResponse'.
impulseResponse_ :: Num a
                 => [a]       -- ^ the impulse response function
                 -> Auto m a a
impulseResponse_ weights = movingAverage_ weights []

-- | The output is the sum of the past outputs, multiplied by a moving
-- window of weights.  Ignores all input.
--
-- For example, if the last outputs are @[1,2,3,4]@ (from most recent to
-- oldest), and the window of weights is @[2,0.5,4]@, then the output will
-- be @1*2 + 0.5*2 + 4*3@, or @15@.  (The weights are assumed to be zero
-- past the end of the weight window)
--
-- Mathematically, @y_n = w_1 * y_(n-1) + w_2 * y_(n-2) + ...@, for all @w@
-- in the weight window, where the first item is @w_1@.
--
-- Note that this serializes the history of the outputs...or at least the
-- history as far back as the entire window of weights.  (A weight list of
-- five items will serialize the past five outputted items)  If your weight
-- window is very long (or infinite), then serializing is a bad idea!
--
-- The second parameter is a list of a "starting history", or initial
-- conditions, to be used when the actual output history isn't long enough.
-- If you want all your initial conditions/starting history to be @0@, just
-- pass in @[]@.
--
-- You can use this to implement any linear recurrence relationship, like
-- he fibonacci sequence:
--
-- >>> evalAutoN' 10 (autoRegression [1,1] [1,1]) ()
-- [2,3,5,8,13,21,34,55,89,144]
-- >>> evalAutoN' 10 (fromList [1,1] --> autoRegression [1,1] [1,1]) ()
-- [1,1,2,3,5,8,13,21,34,55]
--
-- Which is 1 times the previous value, plus one times the value before
-- that.
--
-- You can create a series that doubles by having it be just twice the
-- previous value:
--
-- >>> evalAutoN' 10 (autoRegression [2] [1]) ()
-- [2,,4,8,16,32,64,128,256,512,1024]
--
-- Name comes from the statistical model.
--
autoRegression :: (Num b, Serialize b)
               => [b]         -- ^ weights to apply to previous outputs,
                              --     from most recent
               -> [b]         -- ^ starting history/initial conditions
               -> Auto m a b
autoRegression weights = mkState (const (_autoRegressionF weights))

-- | The non-serializing/non-resuming version of 'autoRegression'.
autoRegression_ :: Num b
                => [b]        -- ^ weights to apply to previous outputs,
                              --     from most recent
                -> [b]        -- ^ starting history/initial conditions
                -> Auto m a b
autoRegression_ weights = mkState_ (const (_autoRegressionF weights))

_autoRegressionF :: Num b => [b] -> [b] -> (b, [b])
_autoRegressionF weights hist = (result, hist')
  where
    result = sum (zipWith (*) weights hist)
    hist'  = zipWith const (result:hist) weights

-- | A combination of 'autoRegression' and 'movingAverage'.  Inspired by
-- the statistical model.
--
-- Mathematically:
--
-- @
-- y_n = wm_0 * x_(n-0) + wm_1 * x_(n-1) + wm_2 * x_(n-2) + ...
--                      + wa_1 * y_(n-1) + wa_2 * y_(n-1) + ...
-- @
--
-- Where @wm_n@s are all of the "moving average" weights, where the first
-- weight is @wm_0@, and @wa_n@s are all of the "autoregression" weights,
-- where the first weight is @wa_1@.
arma :: (Num a, Serialize a)
     => [a]
     -> [a]
     -> [a]
     -> [a]
     -> Auto m a a
arma arWeights maWeights arHist maHist =
        mkState (_armaF arWeights maWeights) (arHist, maHist)

-- | The non-serializing/non-resuming version of 'arma'.
arma_ :: Num a
      => [a]
      -> [a]
      -> [a]
      -> [a]
      -> Auto m a a
arma_ arWeights maWeights arHist maHist =
        mkState_ (_armaF arWeights maWeights) (arHist, maHist)

_armaF :: Num a => [a] -> [a] -> a -> ([a], [a]) -> (a, ([a], [a]))
_armaF arWeights maWeights x (arHist, maHist) = (y, (arHist', maHist'))
  where
    maHist' = zipWith const (x:maHist) maWeights
    ma      = sum (zipWith (*) maWeights maHist')

    ar      = sum (zipWith (*) arWeights arHist)

    y       = ar + ma

    arHist' = zipWith const (y:arHist) arWeights


