module Control.Auto.Process.Numerical (
    summer
  , summer_
  , summerD
  , summerD_
  , multiplier
  , multiplier_
  , multiplierD
  , multiplierD_
  , differ
  , differ_
  ) where

import Control.Auto
import Data.Serialize

summer :: (Serialize a, Num a) => a -> Auto m a a
summer = mkAccum (+)

summer_ :: Num a => a -> Auto m a a
summer_ = mkAccum_ (+)

summerD :: (Serialize a, Num a) => a -> Auto m a a
summerD = mkAccumD (+)

summerD_ :: Num a => a -> Auto m a a
summerD_ = mkAccumD_ (+)

multiplier :: (Serialize a, Num a) => a -> Auto m a a
multiplier = mkAccum (*)

multiplier_ :: Num a => a -> Auto m a a
multiplier_ = mkAccum_ (*)

multiplierD :: (Serialize a, Num a) => a -> Auto m a a
multiplierD = mkAccumD (*)

multiplierD_ :: Num a => a -> Auto m a a
multiplierD_ = mkAccumD_ (*)

differ :: (Serialize a, Num a) => Auto m a (Maybe a)
differ = mkState _differF Nothing

differ_ :: Num a => Auto m a (Maybe a)
differ_ = mkState_ _differF Nothing

_differF :: Num a => a -> Maybe a -> (Maybe a, Maybe a)
_differF x s = case s of
                 Nothing -> (Nothing     , Just x)
                 Just y  -> (Just (y - x), Just y)

