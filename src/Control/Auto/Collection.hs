{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}


module Control.Auto.Collection (
  -- * Static collections
    zipAuto
  , dZipAuto
  , dZipAuto_
  , zipAutoB
  , dZipAutoB
  , dZipAutoB_
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

-- | Give a list of @'Auto' m a b@ and get back an @'Auto' m [a] [b]@  ---
-- take a list of @a@'s and feed them to each of the 'Auto's, and collects
-- their output @b@'s.
--
-- If the input list doesn't have enough items to give to all of the
-- 'Auto's wrapped, then use the given default value.  Any extra items in
-- the input list are ignored.
--
-- For an example, we're going to make a list of 'Auto's that output
-- a running sum of all of their inputs, but each starting at a different
-- beginning value:
--
-- @
--     summerList :: [Auto' Int Int]
--     summerList = map sumFrom [0, 10, 20, 30]
-- @
--
-- Then, let's throw it into 'zipAuto' with a sensible default value, 0:
--
-- @
--     summings0 :: Auto' [Int] [Int]
--     summings0 = zipAuto 0 summerList
-- @
--
-- Now let's try it out!
--
-- >>> let Output r1 summings1 = stepAuto' summings0 [1,2,3,4]
-- >>> r1
-- [ 1, 12, 23, 34]
-- >>> let Output r2 summings2 = stepAuto' summings1 [5,5]
-- >>> r2
-- [ 6, 17, 23, 34]
-- >>> let Output r3 _         = stepAuto' summings2 [10,1,10,1,10000]
-- >>> r3
-- [16, 18, 33, 35]
--
zipAuto :: Monad m
        => a                -- ^ default input value
        -> [Auto m a b]     -- ^ 'Auto's to zip up
        -> Auto m [a] [b]
zipAuto x0 as = mkAutoM (zipAuto x0 <$> mapM loadAuto as)
                        (mapM_ saveAuto as)
                        $ \xs -> do
                            res <- zipWithM stepAuto as (xs ++ repeat x0)
                            let ys  = map outRes  res
                                as' = map outAuto res
                            return (Output ys (zipAuto x0 as'))

-- | Like 'zipAuto', but delay the input by one step, using the default
-- value as the delaying value.
--
-- Let's try the example from 'zipAuto', except with 'dZipAuto' instead:
--
-- @
--     summerList :: [Auto' Int Int]
--     summerList = map sumFrom [0, 10, 20, 30]
--
--     summings0 :: Auto' [Int] [Int]
--     summings0 = dZipAuto 0 summerList
-- @
--
-- Trying it out:
--
-- >>> let Output r1 summings1 = stepAuto' summings0 [1,2,3,4]
-- >>> r1
-- [ 0, 10, 20, 30]
-- >>> let Output r2 summings2 = stepAuto' summings1 [5,5]
-- >>> r2
-- [ 1, 12, 23, 34]
-- >>> let Output r3 summings3 = stepAuto' summings2 [10,1,10,1,10000]
-- >>> r3
-- [ 6, 17, 23, 34]
-- >>> let Output r4 _         = stepAuto' summings3 [100,100,100,100]
-- >>> r4
-- [16, 18, 33, 35]
--
dZipAuto :: (Serialize a, Monad m) => a -> [Auto m a b] -> Auto m [a] [b]
dZipAuto x0 as = zipAuto x0 as . delay []

-- | The non-serializing/non-resuming version of 'dZipAuto'.
dZipAuto_ :: Monad m => a -> [Auto m a b] -> Auto m [a] [b]
dZipAuto_ x0 as = zipAuto x0 as . delay_ []

-- | Takes a bunch of 'Auto's that take 'Blip' streams, and turns them into
-- an 'Auto' that takes a bunch of 'Blip' streams and feeds them into each
-- one in order.
--
-- It's basically like 'zipAuto', except instead of taking in normal
-- streams of values, it takes in 'Blip' streams of values.
--
-- If the input streams ever number less than the number of 'Auto's zipped,
-- then the other 'Auto's are just stepped with a 'Blip' stream that
-- doesn't emit in that step.
zipAutoB :: Monad m => [Auto m (Blip a) b] -> Auto m [Blip a] [b]
zipAutoB = zipAuto NoBlip

-- | A delayed version of 'zipAutoB'
dZipAutoB :: (Serialize a, Monad m) => [Auto m (Blip a) b] -> Auto m [Blip a] [b]
dZipAutoB = dZipAuto NoBlip

-- | The non-serializing/non-resuming version of 'dZipAutoB'.
dZipAutoB_ :: Monad m => [Auto m (Blip a) b] -> Auto m [Blip a] [b]
dZipAutoB_ = dZipAuto_ NoBlip

-- another problem

-- | A dynamic box of 'Auto's.  Takes a list of inputs to feed to each one,
-- in the order that they were added.  Also takes a 'Blip' stream, which
-- emits with new 'Auto's to to the box.
--
-- Add new 'Auto's to the box however you want with the 'Blip' stream.  It
-- collects all of the 'Just' outputs and outputs them all.  Whenever any
-- of the wrapped 'Auto's begins to output 'Nothing', it is removed from
-- the box.
--
-- The adding/removing aside, the routing of the inputs (the first field of
-- the tuple) to the internal 'Auto's and the outputs behaves the same as
-- with 'zipAuto'.
--
-- This will be a pretty powerful collection if you ever imagine adding and
-- destroying behaviors dynamically...like spawning new enemies, or
-- something like that.
--
-- Let's see an example...here we are going to be throwing a bunch of
-- 'Auto's that count to five and then die into our 'dynZip_'...once every
-- other step.
--
-- @
--     -- count upwards, then die when you reach 5
--     countThenDie :: Auto' () (Maybe Int)
--     countThenDie = onFor 5 . iterator (+1) 1
--
--     -- emit a new `countThenDie` every two steps
--     throwCounters :: Auto' () (Blip [Auto' () (Maybe Int)])
--     throwCounters = tagBlips [countThenDie] . every 2
--
--     a :: Auto' () [Int]
--     a = proc _ -> do
--         newCounter <- throwCounters -< ()
--         dynZip_ ()  -< (repeat (), newCounter)
--
--     -- or
--     a' = dynZip_ () . (pure (repeat ()) &&& throwCounters)
-- @
--
-- >>> let (res, _) = stepAutoN' 15 a ()
-- >>> res
-- [[], [1            ]
--    , [2,           ]
--    , [3, 1         ]
--    , [4, 2         ]
--    , [5, 3, 1      ]
--    , [   4, 2      ]
--    , [   5, 3, 1   ]
--    , [      4, 2   ]
--    , [      5, 3, 1]
-- ]
--
-- This is a little unweildy, because 'Auto's maybe disappearing out of the
-- thing while you are trying to feed inputs into it.  You might be feeding
-- an input to an 'Auto'...but one of the 'Auto's before it on the list has
-- disappeared, so it accidentally goes to the wrong one.
--
-- Because of this, it is suggested that you use 'dynMap_', which allows
-- you to "target" labeled 'Auto's with your inputs.
--
-- TODO: Consider putting it in and running inputs immediately?
--
dynZip_ :: Monad m => a -> Auto m ([a], Blip [Auto m a (Maybe b)]) [b]
dynZip_ x0 = go []
  where
    go as = mkAutoM_ $ \(xs, news) -> do
                         let newas = as ++ blip [] id news
                         res <- zipWithM stepAuto newas (xs ++ repeat x0)
                         let (ys, as') = unzip [ (y, a) | (Output (Just y) a) <- res ]
                         return (Output ys (go as'))

-- | A dynamic box of 'Auto's, indexed by an 'Int'.  Takes an 'IntMap' of
-- inputs to feed into their corresponding 'Auto's, and collect all of the
-- outputs into an output 'IntMap'.
--
-- Whenever any of the internal 'Auto's return 'Nothing', they are removed
-- from the collection.
--
-- Toy examples here are of limited use, but let's try it out.  Here we
-- will have a 'dynMap_' that feeds each internal 'Auto' back to itself.
-- The result of each is sent directly back to itself.
--
-- @
--     looper :: Auto' () (IntMap Int)
--     looper = proc _ -> do
--         initAutos <- immediately -< sumFromD <$> [1,-1,0]
--         rec
--             outs <- dynMap_ 0 . delay mempty -< (outs, initAutos)
--         id -< outs
-- @
--
-- >>> let (res, _) = stepAutoN' 20 looper ()
-- >>> tail res
-- [ fromList []
-- , fromList [(0, 1 ), (1, -1 ), (2, 0 )]
-- , fromList [(0, 1 ), (1, -1 ), (2, 0 )]
-- , fromList [(0, 2 ), (1, -2 ), (2, 0 )]
-- , fromList [(0, 3 ), (1, -3 ), (2, 0 )]
-- , fromList [(0, 5 ), (1, -5 ), (2, 0 )]
-- , fromList [(0, 8 ), (1, -8 ), (2, 0 )]
-- , fromList [(0, 13), (1, -13), (2, 0 )]
-- , fromList [(0, 21), (1, -21), (2, 0 )]
-- , fromList [(0, 34), (1, -34), (2, 0 )]
-- ]
--
-- The first part of each tuple is the 'Int' "key", and the second part is
-- the value.
--
-- Basically, the twice-delayed results are fed into the each corresponding
-- 'Auto' --- the results from the 'Auto' starting at positive 1 is fed
-- back to the 'Auto' with a positive counter....results from the 'Auto'
-- starting at negative 1 are fed back into the 'Auto' with the negative
-- counter...and results of the 'Auto' starting at 0 is fed back into the
-- 'Auto' that stays at 0.
--
-- TODO: put in pointers to real examples
--
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

-- | The non-serializing/non-resuming version of 'muxMany'.
muxMany_ :: forall m a b k. (Ord k, Monad m)
     => (k -> Auto m a b) -> Auto m (Map k a) (Map k b)
muxMany_ f = muxManyI_ (fmap Just . f)

mux :: (Serialize k, Ord k, Monad m)
    => (k -> Auto m a b)
    -> Auto m (k, a) b
mux f = dimap (uncurry M.singleton) (head . M.elems) (muxMany f)

-- | The non-serializing/non-resuming version of 'mux'.
mux_ :: (Ord k, Monad m)
     => (k -> Auto m a b)
     -> Auto m (k, a) b
mux_ f = dimap (uncurry M.singleton) (head . M.elems) (muxMany_ f)

muxI :: (Serialize k, Ord k, Monad m)
     => (k -> Auto m a (Maybe b))
     -> Auto m (k, a) (Maybe b)
muxI f = dimap (uncurry M.singleton) (listToMaybe . M.elems) (muxManyI f)

-- | The non-serializing/non-resuming version of 'muxI'.
muxI_ :: (Ord k, Monad m)
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

-- | The non-serializing/non-resuming version of 'muxManyI'.
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

-- | The non-serializing/non-resuming version of 'muxFMany'.
muxFMany_ :: forall m a b k c. (Ord k, Monad m)
          => (k -> Maybe c -> Auto m a b)
          -> Auto m (Map k (Either (c, a) a)) (Map k b)
muxFMany_ f = muxFManyI_ (\k mc -> fmap Just (f k mc))

muxF :: forall m a b k c. (Serialize k, Serialize c, Ord k, Monad m)
     => (k -> Maybe c -> Auto m a b)
     -> Auto m (k, Either (c, a) a) b
muxF f = dimap (uncurry M.singleton) (head . M.elems) (muxFMany f)

-- | The non-serializing/non-resuming version of 'muxF'.
muxF_ :: forall m a b k c. (Ord k, Monad m)
      => (k -> Maybe c -> Auto m a b)
      -> Auto m (k, Either (c, a) a) b
muxF_ f = dimap (uncurry M.singleton) (head . M.elems) (muxFMany_ f)

muxFI :: forall m a b k c. (Serialize k, Serialize c, Ord k, Monad m)
      => (k -> Maybe c -> Auto m a (Maybe b))
      -> Auto m (k, Either (c, a) a) (Maybe b)
muxFI f = dimap (uncurry M.singleton) (listToMaybe . M.elems) (muxFManyI f)

-- | The non-serializing/non-resuming version of 'muxFI'.
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

-- | The non-serializing/non-resuming version of 'muxFManyI'.
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
