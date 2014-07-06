module Control.Auto.Process.Random (
    rands
  , rands_
  ) where

import Control.Auto.Core
import System.Random

rands :: Monad m
      => (StdGen -> (b, StdGen)) -- ^ Random function
      -> StdGen                  -- ^ Initial generator
      -> Auto m a b
rands r g = mkState f (show g)
  where
    f _ sg = let (res, g') = r (read sg)
             in  (res, show g')

rands_ :: Monad m
       => (StdGen -> (b, StdGen)) -- ^ Random function
       -> StdGen                  -- ^ Initial generator
       -> Auto m a b
rands_ r = mkState_ (\_ g -> r g)

