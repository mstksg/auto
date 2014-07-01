module Control.Auto.Generate (
  -- * Generators
    fromList
  , fromList_
  , iterator
  , iteratorM
  , iterator_
  , iteratorM_
  -- * Manipulating generators
  , stretch
  , stretch_
  , stretchE
  ) where

import Control.Applicative
import Control.Auto.Core
import Control.Auto.Event.Internal
import Data.Binary

fromList :: (Binary b, Monad m) => [b] -> Auto m a (Maybe b)
fromList = mkState (const _uncons)

fromList_ :: Monad m => [b] -> Auto m a (Maybe b)
fromList_ = mkState_ (const _uncons)

_uncons :: [a] -> (Maybe a, [a])
_uncons []     = (Nothing, [])
_uncons (x:xs) = (Just x , xs)

iterator :: (Binary b, Monad m) => (b -> b) -> b -> Auto m a b
iterator f = a_
  where
    a_ y0 = mkAuto (a_ <$> get)
                   (put y0)
                   $ \_ -> let y1 = f y0
                           in  Output y1 (a_ y1)

iteratorM :: (Binary b, Monad m) => (b -> m b) -> b -> Auto m a b
iteratorM f = a_
  where
    a_ y0 = mkAutoM (a_ <$> get)
                    (put y0)
                    $ \_ -> do
                        y1 <- f y0
                        return (Output y1 (a_ y1))

iterator_ :: Monad m => (b -> b) -> b -> Auto m a b
iterator_ f = a_
  where
    a_ y0 = mkAuto_ $ \_ -> let y1 = f y0
                            in  Output y1 (a_ y1)

iteratorM_ :: Monad m => (b -> m b) -> b -> Auto m a b
iteratorM_ f = a_
  where
    a_ y0 = mkAutoM_ $ \_ -> do
                         y1 <- f y0
                         return (Output y1 (a_ y1))

stretch :: (Binary b, Monad m) => Int -> Auto m () b -> Auto m () b
stretch n = go (1, undefined)
  where
    go (i, y) a = mkAutoM (go <$> get <*> loadAuto a)
                          (put (i, y) *> saveAuto a)
                          $ \_ ->
                              if i <= 1
                                 then do
                                   Output y' a' <- stepAuto a ()
                                   return (Output y' (go (n    , y') a'))
                                 else
                                   return (Output y  (go (i - 1, y ) a ))


stretch_ :: Monad m => Int -> Auto m () b -> Auto m () b
stretch_ n = go (1, undefined)
  where
    go (i, y) a = mkAutoM_ $ \_ ->
                               if i <= 1
                                  then do
                                    Output y' a' <- stepAuto a ()
                                    return (Output y (go (n    , y') a'))
                                  else
                                    return (Output y (go (i - 1, y ) a ))

stretchE :: Monad m => Int -> Auto m () b -> Auto m () (Event b)
stretchE n = go 1
  where
    go i a = mkAutoM (go <$> get <*> loadAuto a)
                     (put i *> saveAuto a)
                     $ \_ ->
                         if i <= 1
                           then do
                             Output y a' <- stepAuto a ()
                             return (Output (Event y) (go n       a'))
                           else
                             return (Output NoEvent   (go (i - 1) a ))

