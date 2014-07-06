{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Switch (
  -- * Sequential switching
    (-->)
  , (-?>)
  -- * Dynamic switches
  , switch_
  , rSwitch_
  -- * Function-based switches
  , switchF
  , switchF_
  , rSwitchF
  , rSwitchF_
  ) where

import Control.Applicative
import Control.Auto.Core
import Control.Auto.Blip.Internal
import Data.Serialize
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


switch_ :: Monad m
        => Auto m a (b, Blip (Auto m a b))
        -> Auto m a b
switch_ a0 = mkAutoM_ $ \x -> do
                          Output (y, ea1) a0' <- stepAuto a0 x
                          return $ case ea1 of
                            Blip a1 -> Output y a1
                            NoBlip  -> Output y (switch_ a0')

rSwitch_ :: forall m a b. Monad m
         => Auto m a b
         -> Auto m (a, Blip (Auto m a b)) b
rSwitch_ a0 = mkAutoM_ $ \(x, ea1) -> do
                           let a = case ea1 of
                                     NoBlip  -> a0
                                     Blip a1 -> a1
                           Output y a' <- stepAuto a x
                           return (Output y (rSwitch_ a'))


switchF :: forall m a b c. (Monad m, Serialize c)
        => (c -> Auto m a (b, Blip c))
        -> Auto m a (b, Blip c)
        -> Auto m a b
switchF f = go Nothing
  where
    go mz a = mkAutoM (l a) s t
      where
        s   = put mz
           *> saveAuto a
        t x = do
          Output (y, ez) a' <- stepAuto a x
          return $ case ez of
            Blip z -> Output y (go (Just z) (f z))
            NoBlip -> Output y (go mz       a'   )
    l a = do
      mz <- get
      case mz of
        Just z  -> go mz <$> loadAuto (f z)
        Nothing -> go mz <$> loadAuto a

switchF_ :: forall m a b c. Monad m
         => (c -> Auto m a (b, Blip c))
         -> Auto m a (b, Blip c)
         -> Auto m a b
switchF_ f a0 = mkAutoM_ $ \x -> do
                             Output (y, ez) a0' <- stepAuto a0 x
                             return $ case ez of
                               Blip z -> Output y (switchF_ f (f z))
                               NoBlip -> Output y (switchF_ f a0')

rSwitchF :: forall m a b c. (Monad m, Serialize c) => (c -> Auto m a b) -> Auto m a b -> Auto m (a, Blip c) b
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
        NoBlip -> do
          Output y a0' <- stepAuto a0 x
          return (Output y (go mz a0'))
        Blip z -> do
          Output y a1  <- stepAuto (f z) x
          return (Output y (go (Just z) a1))

rSwitchF_ :: forall m a b c. Monad m => (c -> Auto m a b) -> Auto m a b -> Auto m (a, Blip c) b
rSwitchF_ f a0 = mkAutoM_ $ \(x, ez) ->
                              case ez of
                                NoBlip -> do
                                  Output y a0' <- stepAuto a0 x
                                  return (Output y (rSwitchF_ f a0'))
                                Blip z -> do
                                  Output y a1 <- stepAuto (f z) x
                                  return (Output y (rSwitchF_ f a1))
