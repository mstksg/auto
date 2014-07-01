module Control.Auto.Switch where

import Control.Applicative
import Control.Auto.Core
import Control.Auto.Event.Internal
import Data.Binary
import Data.Maybe

(-!>) :: Monad m => Auto m a (Maybe b) -> Auto m a b -> Auto m a b
a1 -!> a2 = fmap fromJust (a1 -?> fmap Just a2)

(-?>) :: Monad m => Auto m a (Maybe b) -> Auto m a (Maybe b) -> Auto m a (Maybe b)
a1 -?> a2 = mkAutoM l s t
  where
    l = do
      flag <- get
      if flag
        then loadAuto (switched a2)
        else liftA2 (-?>) (loadAuto a1) (loadAuto a2)
    s = put False *> saveAuto a1 *> saveAuto a2
    t x = do
      Output y1 a1' <- stepAuto a1 x
      case y1 of
        Just _  ->
          return (Output y1 (a1' -?> a2))
        Nothing -> do
          Output y a2' <- stepAuto a2 x
          return (Output y (switched a2'))
    switched a = mkAutoM (switched <$> loadAuto a)
                         (put True  *> saveAuto a)
                         $ \x -> do
                             Output y a' <- stepAuto a x
                             return (Output y (switched a'))


-- um.  yeah, this is going to be a problem.

switch :: Monad m
       => Auto m a (b, Event (Auto m a b))
       -> Auto m a b
switch a = mkAutoM l s t
  where
    l = undefined
    s = undefined
    t = undefined

