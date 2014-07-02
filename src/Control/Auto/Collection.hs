{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Auto.Collection (
  -- * Static collections
    zipAuto
  -- * Dynamic collections
  , dynZip_
  , dynMap_
  -- * Multiplexers
  , mux
  , mux_
  , muxI
  , muxI_
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Core
import Control.Auto.Event.Internal
import Control.Monad hiding         (mapM, mapM_, sequence)
import Data.Binary
import Data.Foldable
import Data.IntMap.Strict           (IntMap)
import Data.Map.Strict              (Map)
import Data.Maybe
import Data.Monoid
import Data.Profunctor
import Data.Traversable
import Prelude hiding               (mapM, mapM_, concat, sequence)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M

zipAuto :: Monad m => a -> [Auto m a b] -> Auto m [a] [b]
zipAuto x0 as = mkAutoM (zipAuto x0 <$> mapM loadAuto as)
                        (mapM_ saveAuto as)
                        $ \xs -> do
                            res <- zipWithM stepAuto as (xs ++ repeat x0)
                            let ys  = map outRes  res
                                as' = map outAuto res
                            return (Output ys (zipAuto x0 as'))


-- another problem
dynZip_ :: Monad m => a -> Auto m ([a], Event [Auto m a (Maybe b)]) [b]
dynZip_ x0 = go []
  where
    go as = mkAutoM_ $ \(xs, news) -> do
                         let newas = as ++ concat news
                         res <- zipWithM stepAuto newas (xs ++ repeat x0)
                         let (ys, as') = unzip [ (y, a) | (Output (Just y) a) <- res ]
                         return (Output ys (go as'))

dynMap_ :: Monad m => a -> Auto m (IntMap a, Event [Auto m a (Maybe b)]) (IntMap b)
dynMap_ x0 = go 0 mempty
  where
    go i as = mkAutoM_ $ \(xs, news) -> do
                           let newas  = zip [i..] (concat news)
                               newas' = IM.union as (IM.fromList newas)
                               newc   = i + length newas
                               resMap = zipIntMapWithDefaults stepAuto Nothing (Just x0) newas' xs
                           res <- sequence resMap
                           let res' = IM.filter (isJust . outRes) res
                               ys   = fromJust . outRes <$> res'
                               as'  = outAuto <$> res'
                           return (Output ys (go newc as'))


mux :: forall m a b k. (Binary k, Ord k, Monad m)
    => (k -> Auto m a b)
    -> Auto m (Map k a) (Map k b)
mux f = muxI (fmap Just . f)

mux_ :: forall m a b k. (Ord k, Monad m)
     => (k -> Auto m a b) -> Auto m (Map k a) (Map k b)
mux_ f = muxI_ (fmap Just . f)

mux1 :: forall m a b k. (Binary k, Ord k, Monad m)
     => (k -> Auto m a b)
     -> Auto m (k, a) b
mux1 f = dimap (uncurry M.singleton) (head . M.elems) (mux f)

mux1_ :: forall m a b k. (Ord k, Monad m)
      => (k -> Auto m a b)
      -> Auto m (k, a) b
mux1_ f = dimap (uncurry M.singleton) (head . M.elems) (mux_ f)


-- make Auto m (Map k a) (Map k b)
muxI :: forall m a b k. (Binary k, Ord k, Monad m)
     => (k -> Auto m a (Maybe b))
     -> Auto m (Map k a) (Map k b)
muxI f = go mempty
  where
    go :: Map k (Auto m a (Maybe b)) -> Auto m (Map k a) (Map k b)
    go as = mkAutoM l (s as) (t as)
    l     = do
      ks <- get
      let as' = M.fromList (map (id &&& f) ks)
      go <$> mapM loadAuto as'
    s as  = put (M.keys as) *> mapM_ saveAuto as
    t     = _muxIF f go

muxI_ :: forall m a b k. (Ord k, Monad m)
      => (k -> Auto m a (Maybe b))
      -> Auto m (Map k a) (Map k b)
muxI_ f = go mempty
  where
    go :: Map k (Auto m a (Maybe b)) -> Auto m (Map k a) (Map k b)
    go = mkAutoM_ . _muxIF f go

_muxIF :: forall k m a b inMap.  (Ord k, Monad m, inMap ~ (Auto m a (Maybe b)))
       => (k -> inMap)
       -> (Map k inMap -> Auto m (Map k a) (Map k b))
       -> Map k inMap
       -> Map k a
       -> m (Output m (Map k a) (Map k b))
_muxIF f go as xs = do
    let newas = M.mapWithKey (\k _ -> f k) (M.difference xs as)
        allas = M.union as newas
        steps :: Map k (m (Output m a (Maybe b)))
        steps = M.intersectionWith stepAuto allas xs
    outs <- sequence steps
    let (outs', rems) = M.partition (isJust . outRes) outs
        as'           = M.difference as rems
        as''          = M.union as' (fmap outAuto outs')
        ys            = fmap (fromJust . outRes) outs'
    return (Output ys (go as''))

-- dynMap :: forall m a b k. (Ord k, Binary k) => (k -> Auto m a (Maybe b)) -> Auto m (Map k a) (Map k b)
-- dynMap f = go mempty
--   where
--     go :: Map k (Auto m a (Maybe b)) -> Auto m (Map k a) (Map k b)
--     go as = mkAutoM l (s as) (t as)
--     l = do
--       ks <- get
--       let as' = M.fromList (map (id &&& f) ks)
--       go <$> mapM loadAuto as'
--     s as = put (M.keys as) *> mapM_ saveAuto as
--     t as xs = do
--       let stepped = M.foldrWithKey ff aS

-- mapMWithKey :: (Monad m, Ord k) => (k -> a -> m b) -> Map k a -> m (Map k b)
-- mapMWithKey f m = liftM M.fromList (mapM (\(k, v) -> liftM (k,) (f k v)) (M.toList m))




type MapMerge m k a b c = (k -> a -> b -> Maybe c) -> (m a -> m c) -> (m b -> m c) -> m a -> m b -> m c

genericZipMapWithDefaults :: (Monoid (m c), Functor m)
                          => MapMerge m k a b c
                          -> (a -> b -> c) -> Maybe a -> Maybe b
                          -> m a -> m b -> m c
genericZipMapWithDefaults mm f x0 y0 = mm f' zx zy
  where
    f' _ x y = Just (x `f` y)
    zx = case y0 of
           Nothing -> const mempty
           Just y' -> fmap (`f` y')
    zy = case x0 of
           Nothing -> const mempty
           Just x' -> fmap (x' `f`)

zipIntMapWithDefaults :: (a -> b -> c) -> Maybe a -> Maybe b -> IntMap a -> IntMap b -> IntMap c
zipIntMapWithDefaults = genericZipMapWithDefaults IM.mergeWithKey

_zipMapWithDefaults :: Ord k => (a -> b -> c) -> Maybe a -> Maybe b -> Map k a -> Map k b -> Map k c
_zipMapWithDefaults = genericZipMapWithDefaults M.mergeWithKey
