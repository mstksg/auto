{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Time (
  -- * A counter
    count
  -- * Manipulating time
  , lastVal
  , lastVal_
  , delay
  , delay_
  , stretch
  , stretch_
  , stretchB
  , accelerate
  , accelerateWith
  , accelOverList
  , skipTo
  , fastForward
  , fastForwardEither
  ) where

import Control.Applicative
import Control.Auto.Blip.Internal
import Control.Arrow
import Control.Auto.Core
import Control.Auto.Generate
import Control.Monad
import Control.Monad.Loops
import Data.Serialize

count :: Monad m => Auto m a Int
count = iterator (+1) 0

lastVal :: (Serialize a, Monad m) => a -> Auto m a a
lastVal = mkState $ \x s -> (s, x)

lastVal_ :: Monad m => a -> Auto m a a
lastVal_ = mkState_ $ \x s -> (s, x)

delay :: (Serialize a, Monad m) => a -> Auto m a a
delay = lastVal

delay_ :: Monad m => a -> Auto m a a
delay_ = lastVal_



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

accelerateWith :: Monad m => a -> Int -> Auto m a b -> Auto m a [b]
accelerateWith dx n | n <= 1    = fmap (:[])
                    | otherwise = go
  where
    n'    = n - 1
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x0 -> do
                        Output y0 a1  <- stepAuto a0 x0
                        yas <- flip (iterateM n') (undefined, a1)
                               $ \(_, a) -> do
                                   Output x a' <- stepAuto a dx
                                   return (x, a')
                        let ys = y0 : map fst yas
                            a' = snd (last yas)
                        return (Output ys (go a'))

skipTo :: forall m a b c. Monad m => a -> Auto m a (b, Blip c) -> Auto m a ([b], c)
skipTo x0 = go
  where
    go :: Auto m a (b, Blip c)
       -> Auto m a ([b], c)
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x -> do
                      ((ys, z), a1) <- skipOut a0 x []
                      return (Output (reverse ys, z) (go a1))
    skipOut :: Auto m a (b, Blip c)
            -> a
            -> [b]
            -> m (([b], c), Auto m a (b, Blip c))
    skipOut a0 x ys = do
      Output (y, bz) a1 <- stepAuto a0 x
      let ys' = y:ys
      case bz of
        Blip z -> return ((ys', z), a1)
        NoBlip -> skipOut a1 x0 ys'

fastForward :: forall m a b. Monad m
            => a                      -- ^ Default input
            -> Auto m a (Maybe b)
            -> Auto m a b
fastForward x0 = go
  where
    go :: Auto m a (Maybe b)
       -> Auto m a b
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    (skipNothings a0)
    skipNothings :: Auto m a (Maybe b) -> a -> m (Output m a b)
    skipNothings a0 x = do
      Output my a1 <- stepAuto a0 x
      case my of
        Nothing -> skipNothings a1 x0
        Just y  -> return (Output y (go a1))

fastForwardEither :: forall m a b c. Monad m
             => a -> Auto m a (Either c b) -> Auto m a (b, [c])
fastForwardEither x0 = fmap (second reverse) . go
  where
    go :: Auto m a (Either c b)
       -> Auto m a (b, [c])
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                    (skipNothings a0 [])
    skipNothings :: Auto m a (Either c b)
                 -> [c]
                 -> a
                 -> m (Output m a (b, [c]))
    skipNothings a0 zs x = do
      Output ey a1 <- stepAuto a0 x
      case ey of
        Left z  -> skipNothings a1 (z:zs) x0
        Right y -> return (Output (y, zs) (go a1))



-- skip :: forall m a b. Monad m => Auto m (Maybe a) b -> Auto m a b
-- skip = undefined

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM n f = go (max n 0)
  where
    go 0 _ = return []
    go i x = do
      x' <- f x
      xs <- go (i - 1) x'
      return (x' : xs)
