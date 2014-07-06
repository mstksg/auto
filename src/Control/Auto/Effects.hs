{-# LANGUAGE TupleSections #-}

module Control.Auto.Effects (
  -- Running effects
    effect
  , effectB
  , exec
  , exec'
  , execB
  , execB'
  -- One-time effects
  , cache
  , cache_
  , execOnce
  , arrM
  ) where

import Control.Auto.Core
import Control.Auto.Blip
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

effectB :: Monad m => m b -> Auto m (Blip a) (Blip b)
effectB = perBlip . effect

execB :: Monad m => m b -> Auto m (Blip a) (Blip a)
execB = perBlip . exec

execB' :: Monad m => m b -> Auto m (Blip a) (Blip ())
execB' = perBlip . exec'

