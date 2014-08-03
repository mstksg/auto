module Control.Auto.Process (
  -- * Numerical
    summer
  , summer_
  , summerD
  , summerD_
  , differ
  , differ_
  -- * Monoidal/Semigroup
  , mappender
  , mappender_
  , mappendFrom
  , mappendFrom_
  ) where

import Control.Auto
import Data.Serialize

-- | Outputs the running sum of all items passed so far, starting with an
-- initial count.
--
-- prop> summer = mkAccum (+)
summer :: (Serialize a, Num a)
       => a             -- ^ initial count
       -> Auto m a a
summer = mkAccum (+)

-- | The non-resuming/non-serializing version of 'summer'.
summer_ :: Num a
        => a             -- ^ initial count
        -> Auto m a a
summer_ = mkAccum_ (+)

-- | Like 'summer', except the first output is the starting count.
--
-- >>> let a = summerD 5
-- >>> let Output y1 a' = stepAuto' a 10
-- >>> y1
-- 5
-- >>> let Output y2 _  = stepAuto' a' 3
-- >>> y2
-- 10
--
-- It's 'summer', but "delayed".
--
-- Useful for recursive bindings, where you need at least one value to be
-- able to produce its "first output" without depending on anything else.
--
-- prop> summerD x0 = delay x0 . summer x0
summerD :: (Serialize a, Num a)
        => a             -- ^ initial count
        -> Auto m a a
summerD = mkAccumD (+)

-- | The non-resuming/non-serializing version of 'summerD'.
summerD_ :: Num a
         => a             -- ^ initial count
         -> Auto m a a
summerD_ = mkAccumD_ (+)

-- | Returns the difference between the received input and the previous
-- input.  The first result is 'Nothing'; if you have something you want
-- the first result to be, you can use '(<|!>)' from
-- "Control.Auto.Interval", or just 'fromMaybe'/'maybe' from "Data.Maybe".
--
-- >>> let a = differ
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
-- Usage with '(<|!>)':
--
-- >>> let a = differ <|!> pure 100
-- >>> let (ys, _) = overList' a [5,7,4]
-- >>> ys
-- [100, 2, -3]
--
-- Usage with 'fromMaybe':
--
-- >>> let a = fromMaybe 100 <$> differ
-- >>> let (ys, _) = overList' a [5,7,4]
-- >>> ys
-- [100, 2, -3]
--
differ :: (Serialize a, Num a) => Auto m a (Maybe a)
differ = mkState _differF Nothing

-- | The non-resuming/non-serializing version of 'differ'.
differ_ :: Num a => Auto m a (Maybe a)
differ_ = mkState_ _differF Nothing

_differF :: Num a => a -> Maybe a -> (Maybe a, Maybe a)
_differF x s = case s of
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
mappendFrom :: (Serialize a, Semigroup a) => a -> Auto m a a
mappendFrom = mkAccum (<>)


-- | The non-resuming/non-serializing version of 'mappender'.
mappendFrom_ :: Semigroup a => a -> Auto m a a
mappendFrom_ = mkAccum_ (<>)
