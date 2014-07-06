{-# LANGUAGE ViewPatterns #-}

module Control.Auto.Time (
  -- * A counter
    count
  -- * Manipulating time
  , delay
  , delay_
  , stretch
  , stretch_
  , stretchB
  , accelerate
  , maccelerate
  ) where

import Control.Applicative
import Data.Monoid
import Control.Auto.Core
import Control.Auto.Blip.Internal
import Control.Auto.Generate
import Data.Binary

count :: Monad m => Auto m a Int
count = iterator (+1) 0

delay :: (Binary a, Monad m) => a -> Auto m a a
delay = mkState (flip (,))

delay_ :: Monad m => a -> Auto m a a
delay_ = mkState_ (flip (,))


stretch :: (Binary b, Monad m) => Int -> Auto m a b -> Auto m a b
stretch n = go (1, undefined)
  where
    go (i, y) a = mkAutoM (go <$> get <*> loadAuto a)
                          (put (i, y) *> saveAuto a)
                          $ \x ->
                              if i <= 1
                                 then do
                                   Output y' a' <- stepAuto a x
                                   return (Output y' (go (n    , y') a'))
                                 else
                                   return (Output y  (go (i - 1, y ) a ))


stretch_ :: Monad m => Int -> Auto m a b -> Auto m a b
stretch_ n = go (1, undefined)
  where
    go (i, y) a = mkAutoM_ $ \x ->
                               if i <= 1
                                  then do
                                    Output y' a' <- stepAuto a x
                                    return (Output y (go (n    , y') a'))
                                  else
                                    return (Output y (go (i - 1, y ) a ))

stretchB :: Monad m => Int -> Auto m a b -> Auto m a (Blip b)
stretchB (max 1 -> n) = go 1
  where
    go i a = mkAutoM (go <$> get <*> loadAuto a)
                     (put i *> saveAuto a)
                     $ \x ->
                         if i <= 1
                           then do
                             Output y a' <- stepAuto a x
                             return (Output (Blip y) (go n       a'))
                           else
                             return (Output NoBlip   (go (i - 1) a ))

accelerate :: Monad m => Int -> Auto m a b -> Auto m a [b]
accelerate n = go
  where
    n'       = max n 1
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x0 -> do
                        yas <- flip (iterateM n') (undefined, a0)
                               $ \(_, a) -> do
                                   Output x a' <- stepAuto a x0
                                   return (x, a')
                        let ys = map fst yas
                            a' = snd (last yas)
                        return (Output ys (go a'))

maccelerate :: (Monad m, Monoid a) => Int -> Auto m a b -> Auto m a [b]
maccelerate n | n <= 1    = fmap (:[])
              | otherwise = go
  where
    n'    = n - 1
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x0 -> do
                        Output y0 a1  <- stepAuto a0 x0
                        yas <- flip (iterateM n') (undefined, a1)
                               $ \(_, a) -> do
                                   Output x a' <- stepAuto a mempty
                                   return (x, a')
                        let ys = y0 : map fst yas
                            a' = snd (last yas)
                        return (Output ys (go a'))

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM n f = go (max n 0)
  where
    go 0 _ = return []
    go i x = do
      x' <- f x
      xs <- go (i - 1) x'
      return (x' : xs)
