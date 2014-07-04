{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Switch (
    (-->)
  , (-?>)
  , switchF
  , rSwitchF
  ) where

import Control.Applicative
import Control.Auto.Core
import Control.Auto.Event.Internal
import Data.Binary
import Data.Maybe

(-->) :: Monad m => Auto m a (Maybe b) -> Auto m a b -> Auto m a b
a1 --> a2 = fmap fromJust (a1 -?> fmap Just a2)

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
-- switch :: Monad m
--        => Auto m a (b, Event (Auto m a b))
--        -> Auto m a b
-- switch a = mkAutoM l s t
--   where
--     l = undefined
--     s = undefined
--     t = undefined

switchF :: forall m a b c. (Monad m, Binary c) => (c -> Auto m a b) -> Auto m a (b, Event c) -> Auto m a b
switchF f a0 = mkAutoM l s t
  where
    l = do
      mz <- get
      case mz of
        Just z  -> switched z <$> loadAuto (f z)
        Nothing -> switchF f  <$> loadAuto a0
    s = put (Nothing :: Maybe c)
     *> saveAuto a0
    t x = do
      Output (y, ez) a0' <- stepAuto a0 x
      return $ case ez of
        Event z -> Output y (switched z (f z))
        NoEvent -> Output y (switchF f a0')
    switched z a = mkAutoM (switched z  <$> loadAuto a)
                           (put (Just z) *> saveAuto a)
                           $ \x -> do
                               Output y a' <- stepAuto a x
                               return (Output y (switched z a'))

rSwitchF :: forall m a b c. (Monad m, Binary c) => (c -> Auto m a b) -> Auto m a b -> Auto m (a, Event c) b
rSwitchF f = go Nothing
  where
    go mz a0 = mkAutoM (l a0) (s mz a0) (t mz a0)
    l a0 = do
      mz <- get
      case mz of
        Just z  -> go mz <$> loadAuto (f z)
        Nothing -> go mz <$> loadAuto a0
    s mz a0 = put mz
           *> saveAuto a0
    t mz a0 (x, ez) =
      case ez of
        NoEvent -> do
          Output y a0' <- stepAuto a0 x
          return (Output y (go mz a0'))
        Event z -> do
          Output y a1  <- stepAuto (f z) x
          return (Output y (go (Just z) a1))
