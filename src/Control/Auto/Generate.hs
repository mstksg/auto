{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Generate (
  -- * Generators
    fromList
  , fromList_
  , fromInfList_
  , unfold
  , iterator
  , iteratorM
  , iterator_
  , iteratorM_
  ) where

-- import Control.Auto.Event.Internal
import Control.Applicative
import Control.Auto.Core
import Data.Binary

fromList :: (Binary b, Monad m) => [b] -> Auto m a (Maybe b)
fromList = mkState (const _uncons)

fromList_ :: Monad m => [b] -> Auto m a (Maybe b)
fromList_ = mkState_ (const _uncons)

fromInfList_ :: Monad m => [b] -> Auto m a b
fromInfList_ []     = error "fromInfList: reached end of input list."
fromInfList_ (x:xs) = mkAuto_ (\_ -> Output x (fromInfList_ xs))

_uncons :: [a] -> (Maybe a, [a])
_uncons []     = (Nothing, [])
_uncons (x:xs) = (Just x , xs)

unfold :: forall m a b c. (Binary c, Monad m) => (c -> (Maybe b, c)) -> c -> Auto m a (Maybe b)
unfold f = mkState g . Just
  where
    g :: a -> Maybe c -> (Maybe b, Maybe c)
    g _ Nothing  = (Nothing, Nothing)
    g _ (Just x) = case f x of
                     (Just y , x') -> (Just y , Just x')
                     (Nothing, _ ) -> (Nothing, Nothing)

iterator :: (Binary b, Monad m) => (b -> b) -> b -> Auto m a b
iterator f = a_
  where
    a_ y0 = mkAuto (a_ <$> get)
                   (put y0)
                   $ \_ -> let y1 = f y0
                           in  Output y0 (a_ y1)

iteratorM :: (Binary b, Monad m) => (b -> m b) -> b -> Auto m a b
iteratorM f = a_
  where
    a_ y0 = mkAutoM (a_ <$> get)
                    (put y0)
                    $ \_ -> do
                        y1 <- f y0
                        return (Output y0 (a_ y1))

iterator_ :: Monad m => (b -> b) -> b -> Auto m a b
iterator_ f = a_
  where
    a_ y0 = mkAuto_ $ \_ -> let y1 = f y0
                            in  Output y0 (a_ y1)

iteratorM_ :: Monad m => (b -> m b) -> b -> Auto m a b
iteratorM_ f = a_
  where
    a_ y0 = mkAutoM_ $ \_ -> do
                         y1 <- f y0
                         return (Output y0 (a_ y1))

