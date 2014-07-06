{-# LANGUAGE TupleSections #-}

module Control.Auto.Effects (
  -- Running effects
    effect
  , effectE
  , exec
  , exec'
  , execE
  , execE'
  -- One-time effects
  , cache
  , cache_
  , execOnce
  , arrM
  ) where

import Control.Auto.Core
import Control.Auto.Event
import Data.Binary
import Control.Monad
import Control.Applicative
import Control.Auto.Generate

cache :: (Binary b, Monad m) => m b -> Auto m a b
cache m = snd <$> iteratorM (_cacheF m) (False, undefined)

cache_ :: Monad m => m b -> Auto m a b
cache_ m = snd <$> iteratorM_ (_cacheF m) (False, undefined)

_cacheF :: Monad m => m b -> (Bool, b) -> m (Bool, b)
_cacheF m (False, _) = liftM (True,) m
_cacheF _ (True , x) = return (True, x)

execOnce :: Monad m => m b -> Auto m a ()
execOnce = cache_ . liftM (const ())

effect :: Monad m => m b -> Auto m a b
effect = mkConstM

exec :: Monad m => m b -> Auto m a a
exec m = mkFuncM $ \x -> m >> return x

exec' :: Monad m => m b -> Auto m a ()
exec' = mkConstM . liftM (const ())

arrM :: Monad m => (a -> m b) -> Auto m a b
arrM = mkFuncM

effectE :: Monad m => m b -> Auto m (Event a) (Event b)
effectE = perEvent . effect

execE :: Monad m => m b -> Auto m (Event a) (Event a)
execE = perEvent . exec

execE' :: Monad m => m b -> Auto m (Event a) (Event ())
execE' = perEvent . exec'

