{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Collection where

import Control.Applicative
import Control.Arrow
import Control.Auto.Core
import Control.Auto.Event.Internal
import Control.Monad hiding        (mapM, mapM_)
import Data.Binary
import Data.Foldable
import Data.Map.Strict             (Map)
import Data.Monoid
import Data.Traversable
import Prelude hiding              (mapM, mapM_)
import qualified  Data.Map.Strict  as M

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
                         let newas = event [] id news
                         res <- zipWithM stepAuto (as ++ newas) (xs ++ repeat x0)
                         let (ys, as') = unzip [ (y, a) | (Output (Just y) a) <- res ]
                         return (Output ys (go as'))

mux :: forall m a b k. (Binary k, Ord k, Monad m)
    => (k -> Auto m a b)
    -> Auto m (k, a) b
mux f = go mempty
  where
    go :: Map k (Auto m a b) -> Auto m (k, a) b
    go as = _mkMux f go as (goF as)
    goF = _muxF f go

mux_ :: forall m a b k. (Ord k, Monad m) => (k -> Auto m a b) -> Auto m (k, a) b
mux_ f = go mempty
  where
    go :: Map k (Auto m a b) -> Auto m (k, a) b
    go as = mkAutoM_ (goF as)
    goF = _muxF f go

_muxF :: (Ord k, Monad m)
      => (k -> Auto m a b)
      -> (Map k (Auto m a b) -> Auto m (k, a) b)
      -> Map k (Auto m a b)
      -> (k, a)
      -> m (Output m (k, a) b)
_muxF f go as (k, x) = do
    let a = M.findWithDefault (f k) k as
    Output y a' <- stepAuto a x
    let as' = M.insert k a' as
    return (Output y (go as'))

muxI :: forall m a b k. (Binary k, Ord k, Monad m)
     => (k -> Auto m a (Maybe b))
     -> Auto m (k, a) (Maybe b)
muxI f = go mempty
  where
    go :: Map k (Auto m a (Maybe b)) -> Auto m (k, a) (Maybe b)
    go as = _mkMux f go as (goF as)
    goF = _muxIF f go


muxI_ :: forall m a b k. (Binary k, Ord k, Monad m)
      => (k -> Auto m a (Maybe b))
      -> Auto m (k, a) (Maybe b)
muxI_ f = go mempty
  where
    go :: Map k (Auto m a (Maybe b)) -> Auto m (k, a) (Maybe b)
    go as = mkAutoM_ (goF as)
    goF = _muxIF f go

_muxIF :: (Ord k, Monad m)
       => (k -> Auto m a (Maybe b))
       -> (Map k (Auto m a (Maybe b)) -> Auto m (k, a) (Maybe b))
       -> Map k (Auto m a (Maybe b))
       -> (k, a)
       -> m (Output m (k, a) (Maybe b))
_muxIF f go as (k, x) = do
    let a = M.findWithDefault (f k) k as
    Output y a' <- stepAuto a x
    let as' = case y of
                Just _  -> M.insert k a' as
                Nothing -> M.delete k as
    return (Output y (go as'))

_mkMux :: (Binary k, Ord k, Monad m)
       => (k -> Auto m a b)
       -> (Map k (Auto m a b) -> Auto m (k, a) b)
       -> Map k (Auto m a b)
       -> ((k, a) -> m (Output m (k, a) b))
       -> Auto m (k, a) b
_mkMux f go as = mkAutoM l s
  where
    l = do
      ks <- get
      let as' = M.fromList (map (id &&& f) ks)
      go <$> mapM loadAuto as'
    s = put (M.keys as) *> mapM_ saveAuto as
