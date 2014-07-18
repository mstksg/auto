{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , accelOverList
  , skipTo
  ) where

import Control.Applicative
import Data.Monoid
import Control.Monad.Loops
import Control.Monad
import Control.Auto.Core
import Control.Auto.Blip.Internal
import Control.Auto.Generate
import Data.Serialize

count :: Monad m => Auto m a Int
count = iterator (+1) 0

delay :: (Serialize a, Monad m) => a -> Auto m a a
delay = mkState $ \x s -> (s, x)

delay_ :: Monad m => a -> Auto m a a
delay_ = mkState_ $ \x s -> (s, x)


stretch :: (Serialize b, Monad m) => Int -> Auto m a b -> Auto m a b
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
                                    return $ Output y (go (n    , y') a')
                                  else
                                    return $ Output y (go (i - 1, y ) a )

stretchB :: Monad m => Int -> Auto m a b -> Auto m a (Blip b)
stretchB (max 1 -> n) = go 1
  where
    go i a = mkAutoM (go <$> get <*> loadAuto a)
                     (put i *> saveAuto a)
                     $ \x ->
                         if i <= 1
                           then do
                             Output y a' <- stepAuto a x
                             return $ Output (Blip y) (go n       a')
                           else
                             return $ Output NoBlip   (go (i - 1) a )

accelOverList :: Monad m => Auto m a b -> Auto m [a] [b]
accelOverList = go
  where
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \xs -> do
                        res <- liftM reverse (unfoldrM f (a0, xs))
                        case res of
                          []          ->
                            return $ Output [] (go a0)
                          ((_, a'):_) ->
                            return $ Output (map fst res) (go a')

    f (_, [])   = return Nothing
    f (a, x:xs) = do
      Output y a' <- stepAuto a x
      return $ Just ((y, a'), (a', xs))


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

skipTo :: forall m a b c. Monad m => Auto m (Maybe a) (b, Blip c) -> Auto m a ([b], c)
skipTo = go
  where
    go :: Auto m (Maybe a) (b, Blip c)
       -> Auto m a ([b], c)
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x0 -> do
                      ((ys, z), a1) <- skipOut a0 (Just x0) []
                      return (Output (reverse ys, z) (go a1))
    skipOut :: Auto m (Maybe a) (b, Blip c)
            -> Maybe a
            -> [b]
            -> m (([b], c), Auto m (Maybe a) (b, Blip c))
    skipOut a0 x0 ys = do
      Output (y, bz) a1 <- stepAuto a0 x0
      let ys' = y:ys
      case bz of
        Blip z -> return ((ys', z), a1)
        NoBlip -> skipOut a1 Nothing ys'

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM n f = go (max n 0)
  where
    go 0 _ = return []
    go i x = do
      x' <- f x
      xs <- go (i - 1) x'
      return (x' : xs)
