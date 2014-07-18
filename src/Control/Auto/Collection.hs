{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}


module Control.Auto.Collection (
  -- * Static collections
    zipAuto
  , dZipAuto
  , dZipAuto_
  -- , zipAutoB
  -- , dZipAutoB
  -- , dZipAutoB_
  -- * Dynamic collections
  , dynZip_
  , dynMap_
  -- * Multiplexers
  -- ** Key-based
  -- *** Single input/output
  , mux
  , mux_
  , muxI
  , muxI_
  -- *** Parallel input/output
  , muxMany
  , muxMany_
  , muxManyI
  , muxManyI_
  -- ** Function-based
  -- *** Single input/output
  , muxF
  , muxF_
  , muxFI
  , muxFI_
  -- *** Parallel input/output
  , muxFMany
  , muxFMany_
  , muxFManyI
  , muxFManyI_
  -- * "Gathering"/accumulating collections
  -- ** Key-based
  , gather
  , gather_
  , gather__
  , gatherMany
  , gatherMany_
  , gatherMany__
  -- ** Function-based
  , gatherF
  , gatherF_
  , gatherF__
  , gatherFMany
  , gatherFMany_
  , gatherFMany__
  ) where

import Control.Applicative
import Control.Category
import Control.Arrow
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Auto.Time
import Control.Monad hiding         (mapM, mapM_, sequence, sequence_)
import Data.Foldable
import Data.IntMap.Strict           (IntMap)
import Data.Map.Strict              (Map)
import Data.Maybe
import Data.Monoid
import Data.Profunctor
import Data.Serialize
import Data.Traversable
import Prelude hiding               (mapM, mapM_, concat, sequence, (.), id, sequence_)
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

-- delay with nop
dZipAuto :: (Serialize a, Monad m) => a -> [Auto m a b] -> Auto m [a] [b]
dZipAuto x0 as = zipAuto x0 as . delay []

dZipAuto_ :: Monad m => a -> [Auto m a b] -> Auto m [a] [b]
dZipAuto_ x0 as = zipAuto x0 as . delay_ []

-- zipAutoB :: Monad m => [Auto m (Blip a) b] -> Auto m [Blip a] [b]
-- zipAutoB = zipAuto NoBlip

-- dZipAutoB :: (Serialize a, Monad m) => [Auto m (Blip a) b] -> Auto m [Blip a] [b]
-- dZipAutoB = dZipAuto NoBlip

-- dZipAutoB_ :: Monad m => [Auto m (Blip a) b] -> Auto m [Blip a] [b]
-- dZipAutoB_ = dZipAuto_ NoBlip


-- another problem
dynZip_ :: Monad m => a -> Auto m ([a], Blip [Auto m a (Maybe b)]) [b]
dynZip_ x0 = go []
  where
    go as = mkAutoM_ $ \(xs, news) -> do
                         let newas = as ++ blip [] id news
                         res <- zipWithM stepAuto newas (xs ++ repeat x0)
                         let (ys, as') = unzip [ (y, a) | (Output (Just y) a) <- res ]
                         return (Output ys (go as'))

dynMap_ :: Monad m => a -> Auto m (IntMap a, Blip [Auto m a (Maybe b)]) (IntMap b)
dynMap_ x0 = go 0 mempty
  where
    go i as = mkAutoM_ $ \(xs, news) -> do
                           let newas  = zip [i..] (blip [] id news)
                               newas' = IM.union as (IM.fromList newas)
                               newc   = i + length newas
                               resMap = zipIntMapWithDefaults stepAuto Nothing (Just x0) newas' xs
                           res <- sequence resMap
                           let res' = IM.filter (isJust . outRes) res
                               ys   = fromJust . outRes <$> res'
                               as'  = outAuto <$> res'
                           return (Output ys (go newc as'))

muxMany :: forall m a b k. (Serialize k, Ord k, Monad m)
    => (k -> Auto m a b)
    -> Auto m (Map k a) (Map k b)
muxMany f = muxManyI (fmap Just . f)

muxMany_ :: forall m a b k. (Ord k, Monad m)
     => (k -> Auto m a b) -> Auto m (Map k a) (Map k b)
muxMany_ f = muxManyI_ (fmap Just . f)

mux :: forall m a b k. (Serialize k, Ord k, Monad m)
     => (k -> Auto m a b)
     -> Auto m (k, a) b
mux f = dimap (uncurry M.singleton) (head . M.elems) (muxMany f)

mux_ :: forall m a b k. (Ord k, Monad m)
      => (k -> Auto m a b)
      -> Auto m (k, a) b
mux_ f = dimap (uncurry M.singleton) (head . M.elems) (muxMany_ f)

muxI :: forall m a b k. (Serialize k, Ord k, Monad m)
     => (k -> Auto m a (Maybe b))
     -> Auto m (k, a) (Maybe b)
muxI f = dimap (uncurry M.singleton) (listToMaybe . M.elems) (muxManyI f)

muxI_ :: forall m a b k. (Ord k, Monad m)
      => (k -> Auto m a (Maybe b))
      -> Auto m (k, a) (Maybe b)
muxI_ f = dimap (uncurry M.singleton) (listToMaybe . M.elems) (muxManyI_ f)


muxManyI :: forall m a b k. (Serialize k, Ord k, Monad m)
     => (k -> Auto m a (Maybe b))
     -> Auto m (Map k a) (Map k b)
muxManyI f = go mempty
  where
    go :: Map k (Auto m a (Maybe b)) -> Auto m (Map k a) (Map k b)
    go as = mkAutoM l (s as) (t as)
    l     = do
      ks <- get :: Get [k]
      let as = M.fromList (map (id &&& f) ks)
      go <$> mapM loadAuto as
    s as  = put (M.keys as) *> mapM_ saveAuto as
    t     = _muxManyIF f go

muxManyI_ :: forall m a b k. (Ord k, Monad m)
      => (k -> Auto m a (Maybe b))
      -> Auto m (Map k a) (Map k b)
muxManyI_ f = go mempty
  where
    go :: Map k (Auto m a (Maybe b)) -> Auto m (Map k a) (Map k b)
    go = mkAutoM_ . _muxManyIF f go

_muxManyIF :: forall k m a b inMap.  (Ord k, Monad m, inMap ~ (Auto m a (Maybe b)))
       => (k -> inMap)                                -- ^ f : make new Autos
       -> (Map k inMap -> Auto m (Map k a) (Map k b)) -- ^ go: make next step
       -> Map k inMap                                 -- ^ as: all current Autos
       -> Map k a                                     -- ^ xs: Inputs
       -> m (Output m (Map k a) (Map k b))            -- ^ Outputs, and next Auto.
_muxManyIF f go as xs = do
    -- all the outputs of the autos with the present inputs; autos without
    --   inputs are ignored.
    outs <- sequence steps :: m (Map k (Output m a (Maybe b)))
    let -- the successful outputs and the unsuccesful outputs.
        (outs', rems) = M.partition (isJust . outRes) outs
        -- strip the unsuccesful outputs from allas
        allas'        = M.difference allas rems
        -- replace items in allas' with the new changes
        allas''       = M.union (fmap outAuto outs') allas'
        -- all remaining outputs.
        ys            = fmap (fromJust . outRes) outs'
    return (Output ys (go allas''))
  where
    -- new Autos, from the function.  Only on new ones not found in `as`.
    newas = M.mapWithKey (\k _ -> f k) (M.difference xs as)
    -- all Autos, new and old.  Prefer the old ones.
    allas = M.union as newas
    -- Step all the autos with all the inputs.  Lose the Autos that have no
    --   corresponding input.
    steps = M.intersectionWith stepAuto allas xs

muxFMany :: forall m a b k c. (Serialize k, Serialize c, Ord k, Monad m)
         => (k -> Maybe c -> Auto m a b)
         -> Auto m (Map k (Either (c, a) a)) (Map k b)
muxFMany f = muxFManyI (\k mc -> fmap Just (f k mc))

muxFMany_ :: forall m a b k c. (Ord k, Monad m)
          => (k -> Maybe c -> Auto m a b)
          -> Auto m (Map k (Either (c, a) a)) (Map k b)
muxFMany_ f = muxFManyI_ (\k mc -> fmap Just (f k mc))

muxF :: forall m a b k c. (Serialize k, Serialize c, Ord k, Monad m)
     => (k -> Maybe c -> Auto m a b)
     -> Auto m (k, Either (c, a) a) b
muxF f = dimap (uncurry M.singleton) (head . M.elems) (muxFMany f)

muxF_ :: forall m a b k c. (Ord k, Monad m)
      => (k -> Maybe c -> Auto m a b)
      -> Auto m (k, Either (c, a) a) b
muxF_ f = dimap (uncurry M.singleton) (head . M.elems) (muxFMany_ f)

muxFI :: forall m a b k c. (Serialize k, Serialize c, Ord k, Monad m)
      => (k -> Maybe c -> Auto m a (Maybe b))
      -> Auto m (k, Either (c, a) a) (Maybe b)
muxFI f = dimap (uncurry M.singleton) (listToMaybe . M.elems) (muxFManyI f)

muxFI_ :: forall m a b k c. (Ord k, Monad m)
       => (k -> Maybe c -> Auto m a (Maybe b))
       -> Auto m (k, Either (c, a) a) (Maybe b)
muxFI_ f = dimap (uncurry M.singleton) (listToMaybe . M.elems) (muxFManyI_ f)

muxFManyI :: forall m a b c k. (Serialize k, Serialize c, Ord k, Monad m)
          => (k -> Maybe c -> Auto m a (Maybe b))
          -> Auto m (Map k (Either (c, a) a)) (Map k b)
muxFManyI f = go mempty
  where
    go :: Map k (Maybe c, Auto m a (Maybe b))
       -> Auto m (Map k (Either (c, a) a)) (Map k b)
    go as = mkAutoM l (s as) (t as)
    l    = do
      kszs <- get :: Get [(k, Maybe c)]
      let as  = M.fromList (map (\(k, mz) -> (k, (mz, f k mz))) kszs)
      go <$> mapM (mapM loadAuto) as
    s as = put (zip (M.keys as) (map fst (M.elems as)))
        *> mapM_ (saveAuto . snd) as
    t    = _muxFManyIF f go

muxFManyI_ :: forall m a b c k. (Ord k, Monad m)
           => (k -> Maybe c -> Auto m a (Maybe b))
           -> Auto m (Map k (Either (c, a) a)) (Map k b)
muxFManyI_ f = go mempty
  where
    go :: Map k (Maybe c, Auto m a (Maybe b))
       -> Auto m (Map k (Either (c, a) a)) (Map k b)
    go = mkAutoM_ . _muxFManyIF f go

_muxFManyIF :: forall k m a b c inAuto outAuto outOut.
                ( Ord k
                , Monad m
                , inAuto  ~ (Auto m a (Maybe b))
                , outAuto ~ (Auto m (Map k (Either (c, a) a)) (Map k b))
                , outOut  ~ (Output m (Map k (Either (c, a) a)) (Map k b))
                )
            => (k -> Maybe c -> inAuto)                 -- f
            -> (Map k (Maybe c, inAuto) -> outAuto)     -- go
            -> Map k (Maybe c, inAuto)                  -- as
            -> Map k (Either (c, a) a)                  -- xs
            -> m outOut
_muxFManyIF f go as xs = do
    outs <- sequence steps
    let (outs', rems) = M.partition (isJust . outRes . snd) outs
        as'           = M.difference as rems
        as''          = M.union as' (fmap (second outAuto) outs')
        ys            = fmap (fromJust . outRes . snd) outs'
    return (Output ys (go as''))
  where
    mzxs  = fmap eitherToMaybe xs
    newas = M.mapWithKey (_muxgathermapF f) (M.difference mzxs as)
    allas = M.union as newas
    steps = M.intersectionWith interf allas mzxs
    interf :: (Maybe c, Auto m a (Maybe b))
           -> (Maybe c, a)
           -> m (Maybe c, Output m a (Maybe b))
    interf (mc, a) (_, x) = sequence (mc, stepAuto a x)

eitherToMaybe :: Either (a, b) b -> (Maybe a, b)
eitherToMaybe (Left (x, y)) = (Just x , y)
eitherToMaybe (Right y)     = (Nothing, y)

_muxgathermapF :: (k -> Maybe c -> Auto m a (Maybe b)) -> k -> (Maybe c, a) -> (Maybe c, Auto m a (Maybe b))
_muxgathermapF f k (mz, _) = (mz, f k mz)

gather :: forall k a m b. (Ord k, Monad m, Serialize k, Serialize b)
       => (k -> Auto m a (Maybe b))
       -> Auto m (k, a) (Map k b)
gather f = gatherMany f <<^ uncurry M.singleton

gather_ :: forall k a m b. (Ord k, Monad m, Serialize k)
        => (k -> Auto m a (Maybe b))
        -> Auto m (k, a) (Map k b)
gather_ f = gatherMany_ f <<^ uncurry M.singleton

gather__ :: forall k a m b. (Ord k, Monad m)
         => (k -> Auto m a (Maybe b))
         -> Auto m (k, a) (Map k b)
gather__ f = gatherMany__ f <<^ uncurry M.singleton


gatherMany :: forall k a m b. (Ord k, Monad m, Serialize k, Serialize b)
           => (k -> Auto m a (Maybe b))
           -> Auto m (Map k a) (Map k b)
gatherMany f = gatherFMany f' <<^ fmap Right
  where
    f' :: k -> Maybe () -> Auto m a (Maybe b)
    f' k _ = f k

gatherMany_ :: forall k a m b. (Ord k, Monad m, Serialize k)
            => (k -> Auto m a (Maybe b))
            -> Auto m (Map k a) (Map k b)
gatherMany_ f = gatherFMany_ f' <<^ fmap Right
  where
    f' :: k -> Maybe () -> Auto m a (Maybe b)
    f' k _ = f k

gatherMany__ :: forall k a m b. (Ord k, Monad m)
             => (k -> Auto m a (Maybe b))
             -> Auto m (Map k a) (Map k b)
gatherMany__ f = gatherFMany__ f' <<^ fmap Right
  where
    f' :: k -> Maybe () -> Auto m a (Maybe b)
    f' k _ = f k


gatherF :: forall k a m b c. (Ord k, Monad m, Serialize c, Serialize k, Serialize b)
        => (k -> Maybe c -> Auto m a (Maybe b))
        -> Auto m (k, Either (c, a) a) (Map k b)
gatherF f = gatherFMany f <<^ uncurry M.singleton

gatherF_ :: forall k a m b c. (Ord k, Monad m, Serialize c, Serialize k)
         => (k -> Maybe c -> Auto m a (Maybe b))
         -> Auto m (k, Either (c, a) a) (Map k b)
gatherF_ f = gatherFMany_ f <<^ uncurry M.singleton

gatherF__ :: forall k a m b c. (Ord k, Monad m)
          => (k -> Maybe c -> Auto m a (Maybe b))
          -> Auto m (k, Either (c, a) a) (Map k b)
gatherF__ f = gatherFMany__ f <<^ uncurry M.singleton


gatherFMany :: forall k a m b c. (Ord k, Monad m, Serialize c, Serialize k, Serialize b)
            => (k -> Maybe c -> Auto m a (Maybe b))
            -> Auto m (Map k (Either (c, a) a)) (Map k b)
gatherFMany f = go mempty mempty
  where
    go :: Map k (Maybe c, Auto m a (Maybe b))
       -> Map k b
       -> Auto m (Map k (Either (c, a) a)) (Map k b)
    go as ys = mkAutoM l (s as ys) (t as ys)
    l    = go <$> _loadAs f <*> get
    s as ys = put (zip (M.keys as) (map fst (M.elems as)))
           *> mapM_ (saveAuto . snd) as
           *> put ys
    t    = _gatherFManyF f go

gatherFMany_ :: forall k a m b c. (Ord k, Monad m, Serialize c, Serialize k)
             => (k -> Maybe c -> Auto m a (Maybe b))
             -> Auto m (Map k (Either (c, a) a)) (Map k b)
gatherFMany_ f = go mempty mempty
  where
    go :: Map k (Maybe c, Auto m a (Maybe b))
       -> Map k b
       -> Auto m (Map k (Either (c, a) a)) (Map k b)
    go as ys = mkAutoM l (s as) (t as ys)
    l    = go <$> _loadAs f <*> pure mempty
    s as = put (zip (M.keys as) (map fst (M.elems as)))
        *> mapM_ (saveAuto . snd) as
    t    = _gatherFManyF f go

_loadAs :: forall k a m b c. (Serialize k, Serialize c, Ord k)
        => (k -> Maybe c -> Auto m a (Maybe b))
        -> Get (Map k (Maybe c, Auto m a (Maybe b)))
_loadAs f = do
    kszs <- get :: Get [(k, Maybe c)]
    let as = M.fromList (map (\(k, mz) -> (k, (mz, f k mz))) kszs)
    mapM (mapM loadAuto) as


gatherFMany__ :: forall k a m b c. (Ord k, Monad m)
              => (k -> Maybe c -> Auto m a (Maybe b))
              -> Auto m (Map k (Either (c, a) a)) (Map k b)
gatherFMany__ f = go mempty mempty
  where
    go :: Map k (Maybe c, Auto m a (Maybe b))
       -> Map k b
       -> Auto m (Map k (Either (c, a) a)) (Map k b)
    go as ys = mkAutoM_ (_gatherFManyF f go as ys)



_gatherFManyF :: forall k m a b c inAuto outAuto outOut.
                  ( Ord k
                  , Monad m
                  , inAuto  ~ (Auto m a (Maybe b))
                  , outAuto ~ (Auto m (Map k (Either (c, a) a)) (Map k b))
                  , outOut  ~ (Output m (Map k (Either (c, a) a)) (Map k b))
                  )
              => (k -> Maybe c -> inAuto)                 -- f
              -> (Map k (Maybe c, inAuto) -> Map k b -> outAuto)     -- go
              -> Map k (Maybe c, inAuto)                  -- as
              -> Map k b                                  -- ys
              -> Map k (Either (c, a) a)                  -- xs
              -> m outOut
_gatherFManyF f go as ys xs = do
    outs <- sequence steps :: m (Map k (Maybe c, Output m a (Maybe b)))
    let outs', rems   :: Map k (Maybe c, Output m a (Maybe b))
        (outs', rems) = M.partition (isJust . outRes . snd) outs
        as'           = M.difference allas rems
        ys'           = M.difference ys rems
        as''          = M.union (fmap (second outAuto) outs') as'
        newys         = fmap (fromJust . outRes . snd) outs'
        ys''          = M.union newys ys'
    return (Output ys'' (go as'' ys''))
  where
    _mzxs = fmap eitherToMaybe xs
    newas = M.mapWithKey (_muxgathermapF f) (M.difference _mzxs as)
    allas = M.union as newas
    steps :: Map k (m (Maybe c, Output m a (Maybe b)))
    steps = M.intersectionWith interf allas _mzxs
    interf :: (Maybe c, Auto m a (Maybe b))
           -> (Maybe c, a)
           -> m (Maybe c, Output m a (Maybe b))
    interf (mc, a) (_, x) = sequence (mc, stepAuto a x)


-- dynMap :: forall m a b k. (Ord k, Serialize k) => (k -> Auto m a (Maybe b)) -> Auto m (Map k a) (Map k b)
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
