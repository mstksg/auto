{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Switch (
  -- * Sequential switching
    (-->)
  , (-?>)
  -- * Dynamic switches
  , switchFrom_
  , switchOn_
  -- * Function-based switches
  , switchFromF
  , switchFromF_
  , resetFrom
  , switchOnF
  , switchOnF_
  , resetOn
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Category
import Data.Maybe
import Data.Serialize
import Prelude hiding             ((.), id)

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


switchFrom_ :: Monad m
            => Auto m a (b, Blip (Auto m a b))
            -> Auto m a b
switchFrom_ a0 = mkAutoM_ $ \x -> do
                              Output (y, ea1) a0' <- stepAuto a0 x
                              return $ case ea1 of
                                Blip a1 -> Output y a1
                                NoBlip  -> Output y (switchFrom_ a0')

switchOn_ :: forall m a b. Monad m
          => Auto m a b
          -> Auto m (a, Blip (Auto m a b)) b
switchOn_ a0 = mkAutoM_ $ \(x, ea1) -> do
                            let a = case ea1 of
                                      NoBlip  -> a0
                                      Blip a1 -> a1
                            Output y a' <- stepAuto a x
                            return (Output y (switchOn_ a'))


switchFromF :: forall m a b c. (Monad m, Serialize c)
            => (c -> Auto m a (b, Blip c))
            -> Auto m a (b, Blip c)
            -> Auto m a b
switchFromF f = go Nothing
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

switchFromF_ :: forall m a b c. Monad m
             => (c -> Auto m a (b, Blip c))
             -> Auto m a (b, Blip c)
             -> Auto m a b
switchFromF_ f a0 = mkAutoM_ $ \x -> do
                                 Output (y, ez) a0' <- stepAuto a0 x
                                 return $ case ez of
                                   Blip z -> Output y (switchFromF_ f (f z))
                                   NoBlip -> Output y (switchFromF_ f a0')

{-# ANN resetFrom "HLint: ignore Use const" #-}
resetFrom :: Monad m
          => Auto m a (b, Blip c)
          -> Auto m a b
resetFrom a = switchFromF (\_ -> a') a'
  where
    a' = second (tagBlips ()) . a


switchOnF :: forall m a b c. (Monad m, Serialize c)
          => (c -> Auto m a b)
          -> Auto m a b
          -> Auto m (a, Blip c) b
switchOnF f = go Nothing
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

switchOnF_ :: forall m a b c. Monad m
           => (c -> Auto m a b)
           -> Auto m a b
           -> Auto m (a, Blip c) b
switchOnF_ f a0 = mkAutoM_ $ \(x, ez) ->
                              case ez of
                                NoBlip -> do
                                  Output y a0' <- stepAuto a0 x
                                  return (Output y (switchOnF_ f a0'))
                                Blip z -> do
                                  Output y a1 <- stepAuto (f z) x
                                  return (Output y (switchOnF_ f a1))

{-# ANN resetOn "HLint: ignore Use const" #-}
resetOn :: Monad m => Auto m a b -> Auto m (a, Blip c) b
resetOn a = switchOnF (\_ -> a) a . second (tagBlips ())
