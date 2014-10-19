{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Control.Auto.Collection
-- Description : 'Auto's that represent collections of 'Auto's that can be
--               run in parallel, multiplexed, gathered...
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- The 'Auto's in this module are all dedicated to managing and working
-- with "collections" of 'Auto's: 'Auto's that can contain and manage and
-- feed/multiplex input through several internal 'Auto's, and collect the
-- results in the end.  These are particularly useful for managing
-- collection of 'Auto's that can be added to or deleted from, like
-- monsters in a map, or bullets.
--
-- A lot of these 'Auto's take advantaage /Interval/ semantics ('Maybe' for
-- continuous on/off periods) to signal when they want to be removed or
-- turned off.
--
-- For these, the best way to learn them is probably by seeing examples.
-- However, if there is a time when you might want collections of things
-- that can be added to or removed from dynamically, this might be what you
-- are looking for.
--

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
  -- ** Single input, single output
  , mux
  , mux_
  -- ** Multiple input, multiple output
  , muxMany
  , muxMany_
  -- -- ** Function-based
  -- -- *** Single input/output
  -- , muxF
  -- , muxF_
  -- -- *** Parallel input/output
  -- , muxFMany
  -- , muxFMany_
  -- * "Gathering"/accumulating collections
  -- ** Single input, multiple output
  , gather
  , gather_
  , gather__
  --- ** Multiple input, multiple output
  , gatherMany
  , gatherMany_
  , gatherMany__
  -- -- ** Function-based
  -- , gatherF
  -- , gatherF_
  -- , gatherF__
  -- , gatherFMany
  -- , gatherFMany_
  -- , gatherFMany__
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Auto.Interval
import Control.Auto.Time
import Control.Category
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
dZipAuto :: (Serialize a, Monad m)
         => a                 -- ^ default input value
         -> [Auto m a b]      -- ^ 'Auto's to zip up
         -> Auto m [a] [b]
dZipAuto x0 as = zipAuto x0 as . delay []

-- | The non-serializing/non-resuming version of 'dZipAuto'.
dZipAuto_ :: Monad m
          => a                  -- ^ default input value
          -> [Auto m a b]       -- ^ 'Auto's to zip up
          -> Auto m [a] [b]
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
zipAutoB :: Monad m
         => [Auto m (Blip a) b]   -- ^ 'Auto's to zip up
         -> Auto m [Blip a] [b]
zipAutoB = zipAuto NoBlip

-- | A delayed version of 'zipAutoB'
dZipAutoB :: (Serialize a, Monad m)
          => [Auto m (Blip a) b]    -- ^ 'Auto's to zip up
          -> Auto m [Blip a] [b]
dZipAutoB = dZipAuto NoBlip

-- | The non-serializing/non-resuming version of 'dZipAutoB'.
dZipAutoB_ :: Monad m
           => [Auto m (Blip a) b]   -- ^ 'Auto's to zip up
           -> Auto m [Blip a] [b]
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
--     countThenDie :: 'Interval'' () Int
--     countThenDie = onFor 5 . iterator (+1) 1
--
--     -- emit a new `countThenDie` every two steps
--     throwCounters :: Auto' () ('Blip' ['Interval'' () Int])
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
dynZip_ :: Monad m
        => a    -- "default" input to feed in
        -> Auto m ([a], Blip [Interval m a b]) [b]
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
dynMap_ :: Monad m
        => a    -- ^ "default" input to feed in
        -> Auto m (IntMap a, Blip [Interval m a b]) (IntMap b)
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

-- | 'Auto' multiplexer.  Takes a function mapping a key @k@ to an initial
-- 'Auto' to be stored at that key.  Then, every turn, takes a key-input
-- pair @(k, x) :: (k, a)@, and "runs" the 'Auto' at that key @k@ with that
-- input value @x@, and outputs the result.
--
-- TODO: talk about how you can "restart" arrows using combinators from
-- switch and stuff.
--
-- TODO: keys to use with adding things one after another, like in dynMap.
--
-- TODO: example
mux :: (Serialize k, Ord k, Monad m)
    => (k -> Auto m a b)    -- ^ function to create a new 'Auto' if none at
                            --   that key already exists.
    -> Auto m (k, a) b
mux f = dimap (uncurry M.singleton) (head . M.elems) (muxMany f)

-- | The non-serializing/non-resuming version of 'mux'.
mux_ :: (Ord k, Monad m)
     => (k -> Auto m a b)   -- ^ function to create a new 'Auto' if none at
                            --   that key already exists
     -> Auto m (k, a) b
mux_ f = dimap (uncurry M.singleton) (head . M.elems) (muxMany_ f)

-- | 'Auto' multiplexer, like 'mux', but with multple inputs and outputs at
-- a time.  Instead of giving in a single key-value pair of type @(k, a)@,
-- give an entire key-value collection, @Map k a@.  It runs all of the
-- given @a@ inputs with their corresponding 'Auto's indexed by @k@, and
-- outputs all of the results in a @Map k b@.
--
-- TODO: Example
muxMany :: forall m a b k. (Serialize k, Ord k, Monad m)
        => (k -> Auto m a b)    -- ^ function to create a new 'Auto' if
                                --   none at that key already exists
        -> Auto m (Map k a) (Map k b)
muxMany f = go mempty
  where
    go :: Map k (Auto m a b) -> Auto m (Map k a) (Map k b)
    go as = mkAutoM l (s as) (t as)
    l     = do
      ks <- get :: Get [k]
      let as = M.fromList (map (id &&& f) ks)
      go <$> mapM loadAuto as
    s as  = put (M.keys as) *> mapM_ saveAuto as
    t     = _muxManyF f go

-- | The non-serializing/non-resuming version of 'muxMany'.
muxMany_ :: forall m a b k. (Ord k, Monad m)
         => (k -> Auto m a b)     -- ^ function to create a new 'Auto' if
                                  --   none at that key already exists
         -> Auto m (Map k a) (Map k b)
muxMany_ f = go mempty
  where
    go :: Map k (Auto m a b) -> Auto m (Map k a) (Map k b)
    go = mkAutoM_ . _muxManyF f go

_muxManyF :: forall k m a b inMap.  (Ord k, Monad m, inMap ~ (Auto m a b))
          => (k -> inMap)                                -- ^ f : make new Autos
          -> (Map k inMap -> Auto m (Map k a) (Map k b)) -- ^ go: make next step
          -> Map k inMap                                 -- ^ as: all current Autos
          -> Map k a                                     -- ^ xs: Inputs
          -> m (Output m (Map k a) (Map k b))            -- ^ Outputs, and next Auto.
_muxManyF f go as xs = do
    -- all the outputs of the autos with the present inputs; autos without
    --   inputs are ignored.
    outs <- sequence steps
    let ys     = fmap outRes outs
        allas' = M.union (fmap outAuto outs) allas
    return (Output ys (go allas'))
  where
    -- new Autos, from the function.  Only on new ones not found in `as`.
    newas :: Map k inMap
    newas = M.mapWithKey (\k _ -> f k) (M.difference xs as)
    -- all Autos, new and old.  Prefer the old ones.
    allas :: Map k inMap
    allas = M.union as newas
    -- Step all the autos with all the inputs.  Lose the Autos that have no
    --   corresponding input.
    steps :: Map k (m (Output m a b))
    steps = M.intersectionWith stepAuto allas xs

-- muxF :: forall m a b k c. (Serialize k, Serialize c, Ord k, Monad m)
--      => (k -> Maybe c -> Auto m a (Maybe b))
--      -> Auto m (k, Either (c, a) a) (Maybe b)
-- muxF f = dimap (uncurry M.singleton) (listToMaybe . M.elems) (muxFMany f)

-- -- | The non-serializing/non-resuming version of 'muxFI'.
-- muxF_ :: forall m a b k c. (Ord k, Monad m)
--       => (k -> Maybe c -> Auto m a (Maybe b))
--       -> Auto m (k, Either (c, a) a) (Maybe b)
-- muxF_ f = dimap (uncurry M.singleton) (listToMaybe . M.elems) (muxFMany_ f)

-- muxFMany :: forall m a b c k. (Serialize k, Serialize c, Ord k, Monad m)
--          => (k -> Maybe c -> Auto m a (Maybe b))
--          -> Auto m (Map k (Either (c, a) a)) (Map k b)
-- muxFMany f = go mempty
--   where
--     go :: Map k (Maybe c, Auto m a (Maybe b))
--        -> Auto m (Map k (Either (c, a) a)) (Map k b)
--     go as = mkAutoM l (s as) (t as)
--     l    = do
--       kszs <- get :: Get [(k, Maybe c)]
--       let as  = M.fromList (map (\(k, mz) -> (k, (mz, f k mz))) kszs)
--       go <$> mapM (mapM loadAuto) as
--     s as = put (zip (M.keys as) (map fst (M.elems as)))
--         *> mapM_ (saveAuto . snd) as
--     t    = _muxFManyF f go

-- -- | The non-serializing/non-resuming version of 'muxFManyI'.
-- muxFMany_ :: forall m a b c k. (Ord k, Monad m)
--           => (k -> Maybe c -> Auto m a (Maybe b))
--           -> Auto m (Map k (Either (c, a) a)) (Map k b)
-- muxFMany_ f = go mempty
--   where
--     go :: Map k (Maybe c, Auto m a (Maybe b))
--        -> Auto m (Map k (Either (c, a) a)) (Map k b)
--     go = mkAutoM_ . _muxFManyF f go

-- _muxFManyF :: forall k m a b c inAuto outAuto outOut.
--                ( Ord k
--                , Monad m
--                , inAuto  ~ (Auto m a (Maybe b))
--                , outAuto ~ (Auto m (Map k (Either (c, a) a)) (Map k b))
--                , outOut  ~ (Output m (Map k (Either (c, a) a)) (Map k b))
--                )
--            => (k -> Maybe c -> inAuto)                 -- f
--            -> (Map k (Maybe c, inAuto) -> outAuto)     -- go
--            -> Map k (Maybe c, inAuto)                  -- as
--            -> Map k (Either (c, a) a)                  -- xs
--            -> m outOut
-- _muxFManyF f go as xs = do
--     outs <- sequence steps
--     let (outs', rems) = M.partition (isJust . outRes . snd) outs
--         as'           = M.difference as rems
--         as''          = M.union as' (fmap (second outAuto) outs')
--         ys            = fmap (fromJust . outRes . snd) outs'
--     return (Output ys (go as''))
--   where
--     mzxs  = fmap eitherToMaybe xs
--     newas = M.mapWithKey (_muxgathermapF f) (M.difference mzxs as)
--     allas = M.union as newas
--     steps = M.intersectionWith interf allas mzxs
--     interf :: (Maybe c, Auto m a (Maybe b))
--            -> (Maybe c, a)
--            -> m (Maybe c, Output m a (Maybe b))
--     interf (mc, a) (_, x) = sequence (mc, stepAuto a x)

eitherToMaybe :: Either (a, b) b -> (Maybe a, b)
eitherToMaybe (Left (x, y)) = (Just x , y)
eitherToMaybe (Right y)     = (Nothing, y)

_muxgathermapF :: (k -> Maybe c -> Interval m a b) -> k -> (Maybe c, a) -> (Maybe c, Interval m a b)
_muxgathermapF f k (mz, _) = (mz, f k mz)

-- | Keeps a whole bunch of 'Auto's in a 'Map', and every step, outputs
-- a 'Map' containing each key and the last ('Just') value outputted by the
-- 'Auto' stored at that key.
--
-- Gets as input a key-value pair @(k, x) :: (k, a)@, and feeds that input
-- to the 'Auto' stored at that key.  If there is no 'Auto' yet stored,
-- then generates a fresh one using the given 'Auto'-making function.
--
-- As soon as a stored 'Auto' outputs a 'Nothing', the 'Auto' (and its key)
-- are removed from the 'Map', and never outputted again in the total
-- output, its most recent value lost forever.  However, you can still
-- "recreate" it by passing in a key-value pair with that key; life starts
-- all over again.
--
-- TODO: keys to use with adding things one after another, like in dynMap.
--
-- TODO: Example
gather :: (Ord k, Monad m, Serialize k, Serialize b)
       => (k -> Interval m a b)     -- ^ function to create a new 'Auto'
                                    --   if none at that key already
                                    --   exists
       -> Auto m (k, a) (Map k b)
gather = lmap (uncurry M.singleton) . gatherMany

-- | The non-serializing/non-resuming version of 'gather':
--
-- __Does__ serialize the actual __'Auto's__ themselves; the 'Auto's are
-- all serialized and re-loaded/resumed when 'gather_ f' is resumed.
--
-- Does __not__ serialize the "last outputs", so resumed 'Auto's that have
-- not yet been re-run/accessed to get a fresh output are not represented
-- in the output map at first.
--
gather_ :: (Ord k, Monad m, Serialize k)
        => (k -> Interval m a b)      -- ^ function to create a new 'Auto'
                                      --   if none at that key already
                                      --   exists
        -> Auto m (k, a) (Map k b)
gather_ = lmap (uncurry M.singleton) . gatherMany_

-- | The non-serializing/non-resuming vervsion of 'gather':
--
-- Serializes neither the 'Auto's themselves nor the "last outputs" ---
-- essentially, serializes/resumes nothing.
gather__ :: (Ord k, Monad m)
         => (k -> Interval m a b)       -- ^ function to create a new
                                        --   'Auto' if none at that key
                                        --   already exists
         -> Auto m (k, a) (Map k b)
gather__ = lmap (uncurry M.singleton) . gatherMany__


-- | Like 'gather', but allows you to input multiple key-value pairs every
-- step, as a 'Map'.  Still outputs a 'Map' with "the last result of every
-- stored 'Auto'".
--
-- TODO: Example
gatherMany :: forall k a m b. (Ord k, Monad m, Serialize k, Serialize b)
           => (k -> Interval m a b)       -- ^ function to create a new
                                          --   'Auto' if none at that key
                                          --   already exists
           -> Auto m (Map k a) (Map k b)
gatherMany f = lmap (fmap Right) (gatherFMany f')
  where
    f' :: k -> Maybe () -> Interval m a b
    f' k _ = f k

-- | The non-serializing/non-resuming version of 'gatherMany':
--
-- __Does__ serialize the actual __'Auto's__ themselves; the 'Auto's are
-- all serialized and re-loaded/resumed when 'gatherMany_ f' is resumed.
--
-- Does __not__ serialize the "last outputs", so resumed 'Auto's that have
-- not yet been re-run/accessed to get a fresh output are not represented
-- in the output map at first.
--
gatherMany_ :: forall k a m b. (Ord k, Monad m, Serialize k)
            => (k -> Interval m a b)      -- ^ function to create a new
                                          --   'Auto' if none at that key
                                          --   already exists
            -> Auto m (Map k a) (Map k b)
gatherMany_ f = lmap (fmap Right) (gatherFMany_ f')
  where
    f' :: k -> Maybe () -> Interval m a b
    f' k _ = f k

-- | The non-serializing/non-resuming vervsion of 'gatherMany':
--
-- Serializes neither the 'Auto's themselves nor the "last outputs" ---
-- essentially, serializes/resumes nothing.
gatherMany__ :: forall k a m b. (Ord k, Monad m)
             => (k -> Interval m a b)       -- ^ function to create a new
                                            --   'Auto' if none at that key
                                            --   already exists
             -> Auto m (Map k a) (Map k b)
gatherMany__ f = lmap (fmap Right) (gatherFMany__ f')
  where
    f' :: k -> Maybe () -> Interval m a b
    f' k _ = f k

-- gatherF :: forall k a m b c. (Ord k, Monad m, Serialize c, Serialize k, Serialize b)
--         => (k -> Maybe c -> Auto m a (Maybe b))
--         -> Auto m (k, Either (c, a) a) (Map k b)
-- gatherF = lmap (uncurry M.singleton) . gatherFMany

-- gatherF_ :: forall k a m b c. (Ord k, Monad m, Serialize c, Serialize k)
--          => (k -> Maybe c -> Auto m a (Maybe b))
--          -> Auto m (k, Either (c, a) a) (Map k b)
-- gatherF_ = lmap (uncurry M.singleton) . gatherFMany_

-- gatherF__ :: forall k a m b c. (Ord k, Monad m)
--           => (k -> Maybe c -> Auto m a (Maybe b))
--           -> Auto m (k, Either (c, a) a) (Map k b)
-- gatherF__ = lmap (uncurry M.singleton) . gatherFMany__


gatherFMany :: forall k m a b c. (Ord k, Monad m, Serialize c, Serialize k, Serialize b)
            => (k -> Maybe c -> Interval m a b)
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

gatherFMany_ :: forall k m a b c. (Ord k, Monad m, Serialize c, Serialize k)
             => (k -> Maybe c -> Interval m a b)
             -> Auto m (Map k (Either (c, a) a)) (Map k b)
gatherFMany_ f = go mempty mempty
  where
    go :: Map k (Maybe c, Interval m a b)
       -> Map k b
       -> Auto m (Map k (Either (c, a) a)) (Map k b)
    go as ys = mkAutoM l (s as) (t as ys)
    l    = go <$> _loadAs f <*> pure mempty
    s as = put (zip (M.keys as) (map fst (M.elems as)))
        *> mapM_ (saveAuto . snd) as
    t    = _gatherFManyF f go

_loadAs :: forall k a m b c. (Serialize k, Serialize c, Ord k)
        => (k -> Maybe c -> Interval m a b)
        -> Get (Map k (Maybe c, Interval m a b))
_loadAs f = do
    kszs <- get :: Get [(k, Maybe c)]
    let as = M.fromList (map (\(k, mz) -> (k, (mz, f k mz))) kszs)
    mapM (mapM loadAuto) as


gatherFMany__ :: forall k a m b c. (Ord k, Monad m)
              => (k -> Maybe c -> Interval m a b)
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
                  , inAuto  ~ (Interval m a b)
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
