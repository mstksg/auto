{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Generate (
  -- * Generators
    fromList
  , fromLongList
  , fromList_
  -- , fromInfList
  -- , fromInfList_
  , unfold
  , iterator
  , iteratorM
  , iterator_
  , iteratorM_
  ) where

-- import Control.Auto.Event.Internal
import Control.Applicative
import Control.Auto.Core
import Control.Category
import Data.Serialize
import Prelude hiding                 ((.), id)

-- | Construct an 'Auto' that ignores its input and just continually emits
-- elements from the given list.  Ouputs 'Nothing' forever after reaching
-- the end of the list.
--
-- Serializes itself by storing the entire rest of the list in binary, so
-- if your list is long, it might take up a lot of space upon
-- storage.  If your list is infinite, it makes an infinite binary, so be
-- careful!
--
--   * Storing: O(n) time and space on length of remaining list
--   * Loading: O(1) time in the number of times the 'Auto' has been
--   stepped + O(n) time in the length of the remaining list.
--
fromList :: (Serialize b, Monad m) => [b] -> Auto m a (Maybe b)
fromList = mkState (const _uncons)

-- | A version of 'fromList' that is safe for long or infinite lists, or
-- lists with unserializable elements.
--
-- There is a small cost in the time of loading/resuming, which is @O(n)@
-- on the number of times the Auto had been stepped at the time of
-- saving.  This is because it has to drop the @n@ first elements in the
-- list, to "resume" to the proper position.
--
--   * Storing: O(1) time and space on the length of the remaining list
--   * Loading: O(n) time on the number of times the 'Auto' has been
--   stepped, maxing out at O(n) on the length of the entire input list.
fromLongList :: Monad m => [b] -> Auto m a (Maybe b)
fromLongList xs = go 0 xs
  where
    loader = do
      stopped <- get
      if stopped
        then return finished
        else do
          i <- get
          return (go i (drop i xs))
    finished = mkAuto loader
                      (put True)
                      $ \_ -> Output Nothing finished
    go i ys  = mkAuto loader
                      (put (False, i))
                      $ \_ -> case ys of
                                (y':ys') -> Output (Just y') (go (i + 1) ys')
                                []       -> Output Nothing finished

-- | Like 'fromList', but doesn't attempt to serialize its current position
-- in the list.
fromList_ :: Monad m => [b] -> Auto m a (Maybe b)
fromList_ = mkState_ (const _uncons)

-- -- | Creates an Auto from an infinite
-- fromInfList :: Monad m => [b] -> Auto m a b
-- fromInfList xs = f <$> fromLongList xs
--   where
--     f (Just x) = x
--     f Nothing  = error "fromInfList: reached end of input list."

-- fromInfList_ :: Monad m => [b] -> Auto m a b
-- fromInfList_ []     = error "fromInfList_: reached end of input list."
-- fromInfList_ (x:xs) = mkAuto_ (\_ -> Output x (fromInfList_ xs))

_uncons :: [a] -> (Maybe a, [a])
_uncons []     = (Nothing, [])
_uncons (x:xs) = (Just x , xs)

unfold :: forall m a b c. (Serialize c, Monad m) => (c -> (Maybe b, c)) -> c -> Auto m a (Maybe b)
unfold f = mkState g . Just
  where
    g :: a -> Maybe c -> (Maybe b, Maybe c)
    g _ Nothing  = (Nothing, Nothing)
    g _ (Just x) = case f x of
                     (Just y , x') -> (Just y , Just x')
                     (Nothing, _ ) -> (Nothing, Nothing)

iterator :: (Serialize b, Monad m) => (b -> b) -> b -> Auto m a b
iterator f = a_
  where
    a_ y0 = mkAuto (a_ <$> get)
                   (put y0)
                   $ \_ -> Output y0 (a_ (f y0))

iteratorM :: (Serialize b, Monad m) => (b -> m b) -> b -> Auto m a b
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
    a_ y0 = mkAuto_ $ \_ -> Output y0 (a_ (f y0))

iteratorM_ :: Monad m => (b -> m b) -> b -> Auto m a b
iteratorM_ f = a_
  where
    a_ y0 = mkAutoM_ $ \_ -> do
                         y1 <- f y0
                         return (Output y0 (a_ y1))

