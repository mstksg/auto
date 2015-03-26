{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Control.Auto.Collection
-- Description : 'Auto's that represent collections of 'Auto's that can be
--               run in parallel, multiplexed, gathered...
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- The 'Auto's in this module are all dedicated to managing and working
-- with (possibly dynamic) "collections" of 'Auto's: an 'Auto' where the
-- output stream is typically /many/ output streams collected from running
-- many input streams through many internal 'Auto's.
--
-- Particularly useful because a lot of these allow you to add or take away
-- these "channels of inputs" (or "internal 'Auto's") dynamically; so,
-- useful for collections that can be added to or deleted from, like
-- monsters on a map.
--
-- These multiplex, merge, or collect input streams through many 'Auto's
-- and output the multiplexed, merged, or collected output streams.
--
-- A lot of these 'Auto's take advantaage /Interval/ semantics ('Maybe' for
-- continuous on/off periods) to signal when they want to be removed or
-- turned off.
--
-- For these, the best way to learn them is probably by seeing examples.
--
-- If there is a time when you might want collections of things
-- that can be added to or removed from dynamically, this might be what you
-- are looking for.
--
-- These collections are indispensible for coding real applications; many
-- examples of them in use are available in the
-- <https://github.com/mstksg/auto-examples auto-examples> project!  See
-- those projects for "real-world" guides.
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
  , dynZipF
  , dynZipF_
  , dynMap_
  , dynMapF
  , dynMapF_
  -- * Multiplexers
  -- ** Single input, single output
  , mux
  , mux_
  -- ** Multiple input, multiple output
  , muxMany
  , muxMany_
  -- * "Gathering"/accumulating collections
  -- ** Single input, multiple output
  , gather
  , gather_
  , gather__
  --- ** Multiple input, multiple output
  , gatherMany
  , gatherMany_
  , gatherMany__
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
-- summerList :: [Auto' Int Int]
-- summerList = [sumFrom 0, sumFrom 10, sumFrom 20, sumFrom 30]
-- @
--
-- Then, let's throw it into 'zipAuto' with a sensible default value, 0:
--
-- @
-- summings0 :: Auto' [Int] [Int]
-- summings0 = zipAuto 0 summerList
-- @
--
-- Now let's try it out!
--
-- >>> let (r1, summings1) = stepAuto' summings0 [1,2,3,4]
-- >>> r1
-- [ 1, 12, 23, 34]
-- >>> let (r2, summings2) = stepAuto' summings1 [5,5]
-- >>> r2
-- [ 6, 17, 23, 34]
-- >>> let (r3, _        ) = stepAuto' summings2 [10,1,10,1,10000]
-- >>> r3
-- [16, 18, 33, 35]
--
zipAuto :: Monad m
        => a                -- ^ default input value
        -> [Auto m a b]     -- ^ 'Auto's to zip up
        -> Auto m [a] [b]
zipAuto x0 as = mkAutoM (zipAuto x0 <$> mapM resumeAuto as)
                        (mapM_ saveAuto as)
                        $ \xs -> do
                            res <- zipWithM stepAuto as (xs ++ repeat x0)
                            let (ys, as') = unzip res
                            return (ys, zipAuto x0 as')

-- | Like 'zipAuto', but delay the input by one step.  The first input to
-- all of them is the "default" value, and after that, feeds in the input
-- streams delayed by one.
--
-- Let's try the example from 'zipAuto', except with 'dZipAuto' instead:
--
-- @
-- summerList :: [Auto' Int Int]
-- summerList = map sumFrom [0, 10, 20, 30]
--
-- summings0 :: Auto' [Int] [Int]
-- summings0 = dZipAuto 0 summerList
-- @
--
-- Trying it out:
--
-- >>> let (r1, summings1) = stepAuto' summings0 [1,2,3,4]
-- >>> r1
-- [ 0, 10, 20, 30]
-- >>> let (r2, summings2) = stepAuto' summings1 [5,5]
-- >>> r2
-- [ 1, 12, 23, 34]
-- >>> let (r3, summings3) = stepAuto' summings2 [10,1,10,1,10000]
-- >>> r3
-- [ 6, 17, 23, 34]
-- >>> let (r4, _        ) = stepAuto' summings3 [100,100,100,100]
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

-- | Takes a bunch of 'Auto's that take streams streams, and turns them
-- into one 'Auto' that takes a bunch of blip streams and feeds them into
-- each of the original 'Auto's, in order.
--
-- It's basically like 'zipAuto', except instead of taking in normal
-- streams of values, it takes in blip streams of values.
--
-- If the input streams ever number less than the number of 'Auto's zipped,
-- the other 'Auto's are stepped assuming no emitted value.
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

-- | A dynamic box of 'Interval's.  Takes a list of inputs to feed to each
-- one, in the order that they were added.  Also takes a blip stream, which
-- emits with new 'Interval's to add to the box.
--
-- Add new 'Interval's to the box however you want with the blip stream.
--
-- As soon as an 'Interval' turns "off", the 'Interval' is removed from the
-- box, and its output is silenced.
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
-- -- count upwards, then die when you reach 5
-- countThenDie :: 'Interval'' () Int
-- countThenDie = onFor 5 . iterator (+1) 1
--
-- -- emit a new `countThenDie` every two steps
-- throwCounters :: Auto' () ('Blip' ['Interval'' () Int])
-- throwCounters = tagBlips [countThenDie] . every 2
--
-- a :: Auto' () [Int]
-- a = proc _ -> do
--         newCounter <- throwCounters -< ()
--         dynZip_ ()  -< (repeat (), newCounter)
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
-- This 'Auto' is inherently unserializable, but you can use 'dynZipF' for
-- more or less the same functionality, with serialization possible.  It's
-- only slightly less powerful...for all intents and purposes, you should
-- be able to use both in the same situations.  All of the examples here
-- can be also done with 'dynZipF'.
--
dynZip_ :: Monad m
        => a    -- "default" input to feed in
        -> Auto m ([a], Blip [Interval m a b]) [b]
dynZip_ x0 = go []
  where
    go as = mkAutoM_ $ \(xs, news) -> do
                         let newas = as ++ blip [] id news
                         res <- zipWithM stepAuto newas (xs ++ repeat x0)
                         let (ys, as') = unzip [ (y, a) | (Just y, a) <- res ]
                         return (ys, go as')

-- | Like 'dynZip_', but instead of taking in a blip stream of 'Interval's
-- directly, takes in a blip stream of 'k's to trigger adding more
-- 'Interval's to the "box", using the given @k -> 'Interval' m a b@
-- function to make the new 'Interval' to add.
--
-- Pretty much all of the power of 'dynZip_', but with serialization.
--
-- See 'dynZip_' for examples and caveats.
--
-- You could theoretically recover the behavior of 'dynZip_' with
-- @'dynZipF' id@, if there wasn't a 'Serialize' constraint on the @k@.
dynZipF :: (Serialize k, Monad m)
        => (k -> Interval m a b)      -- ^ function to generate a new
                                      --     'Interval' for each coming @k@
                                      --     in the blip stream.
        -> a                          -- ^ "default" input to feed in
        -> Auto m ([a], Blip [k]) [b]
dynZipF f x0 = go []
  where
    go ksas = mkAutoM (do ks <- get
                          as <- mapM (resumeAuto . f) ks
                          return $ go (zip ks as) )
                      (do let (ks,as) = unzip ksas
                          put ks
                          mapM_ saveAuto as)
                      (goFunc ksas)
    goFunc = _dynZipF f x0 go

-- | The non-serializing/non-resuming version of 'dynZipF'.  Well, you
-- really might as well use 'dynZip_', which is more powerful...but maybe
-- using this can inspire more disciplined usage.  Also works as a drop-in
-- replacement for 'dynZipF'.
dynZipF_ :: Monad m
         => (k -> Interval m a b)     -- ^ function to generate a new
                                      --     'Interval' for each coming @k@
                                      --     in the blip stream.
         -> a                         -- ^ "default" input to feed in
         -> Auto m ([a], Blip [k]) [b]
dynZipF_ f x0 = go []
  where
    go ksas = mkAutoM_ (goFunc ksas)
    goFunc = _dynZipF f x0 go

_dynZipF :: Monad m
         => (k -> Interval m a b)
         -> a
         -> ([(k, Interval m a b)] -> Auto m ([a], Blip [k]) [b])
         -> [(k, Interval m a b)]
         -> ([a], Blip [k])
         -> m ([b], Auto m ([a], Blip [k]) [b])
_dynZipF f x0 go ksas (xs, news) = do
    let adds    = blip [] (map (id &&& f)) news
        newksas = ksas ++ adds
        (newks,newas) = unzip newksas
    res <- zipWithM stepAuto newas (xs ++ repeat x0)
    let resks = zip newks res
        (ys, ksas') = unzip [ (y, (k,a)) | (k, (Just y, a)) <- resks ]
    return (ys, go ksas')


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
-- >>> import qualified Data.IntMap as IM
-- >>> let dm0 :: Auto' (IM.IntMap Int) (IM.IntMap Int)
--         dm0 = proc x -> do
--                   initials <- immediately -< [ Just <$> sumFrom 0
--                                              , Just <$> sumFrom 10 ]
--                   newIs    <- every 3     -< [ Just <$> sumFrom 0  ]
--                   dynMap_ (-1) -< (x, initials `mergeL` newIs)
-- >>> let (res1, dm1) = stepAuto' dm0 mempty
-- >>> res1
-- fromList [(0, -1), (1, 9)]
-- >>> let (res2, dm2) = stepAuto' dm1 (IM.fromList [(0,100),(1,50)])
-- >>> res2
-- fromList [(0, 99), (1, 59)]
-- >>> let (res3, dm3) = stepAuto' dm2 (IM.fromList [(0,10),(1,5)])
-- >>> res3
-- fromList [(0, 109), (1, 64), (2, -1)]
-- >>> let (res4, _  ) = stepAuto' dm3 (IM.fromList [(1,5),(2,5)])
-- >>> res4
-- fromList [(0, 108), (1, 69), (2, 4)]
--
-- One quirk is that every internal 'Auto' is "stepped" at every step with
-- the default input; 'gatherMany' is a version of this where 'Auto's that
-- do not have a corresponding "input" are left unstepped, and their last
-- output preserved in the aggregate output.  As such, 'gatherMany' might
-- be seen more often.
--
-- This 'Auto' is inherently unserializable, but you can use 'dynMapF' for
-- more or less the same functionality, with serialization possible.  It's
-- only slightly less powerful...for all intents and purposes, you should
-- be able to use both in the same situations.  All of the examples here
-- can be also done with 'dynMapF'.
--
dynMap_ :: Monad m
        => a    -- ^ "default" input to feed in
        -> Auto m (IntMap a, Blip [Interval m a b]) (IntMap b)
dynMap_ x0 = go 0 IM.empty
  where
    go i as = mkAutoM_ $ \(xs, news) -> do
                           let newas  = zip [i..] (blip [] id news)
                               newas' = as `IM.union` IM.fromList newas
                               newc   = i + length newas
                               resMap = zipIntMapWithDefaults stepAuto Nothing (Just x0) newas' xs
                           res <- sequence resMap
                           let res' = IM.filter (isJust . fst) res
                               ys   = fromJust . fst <$> res'
                               as'  = snd <$> res'
                           return (ys, go newc as')

-- | Like 'dynMap_', but instead of taking in a blip stream of 'Interval's
-- directly, takes in a blip stream of 'k's to trigger adding more
-- 'Interval's to the "box", using the given @k -> 'Interval' m a b@
-- function to make the new 'Interval' to add.
--
-- Pretty much all of the power of 'dynMap_', but with serialization.
--
-- See 'dynMap_' for examples and use cases.
--
-- You could theoretically recover the behavior of 'dynMap_' with
-- @'dynMapF' id@, if there wasn't a 'Serialize' constraint on the @k@.
dynMapF :: (Serialize k, Monad m)
        => (k -> Interval m a b)      -- ^ function to generate a new
                                      --     'Interval' for each coming @k@
                                      --     in the blip stream.
        -> a                          -- ^ "default" input to feed in
        -> Auto m (IntMap a, Blip [k]) (IntMap b)
dynMapF f x0 = go 0 IM.empty IM.empty
  where
    go i ks as = mkAutoM (do i'  <- get
                             ks' <- get
                             as' <- mapM (resumeAuto . f) ks'
                             return (go i' ks' as') )
                         (put i *> put ks *> mapM_ saveAuto as)
                         (goFunc i ks as)
    goFunc = _dynMapF f x0 go

-- | The non-serializing/non-resuming version of 'dynMapF'.  Well, you
-- really might as well use 'dynMap_', which is more powerful...but maybe
-- using this can inspire more disciplined usage.  Also works as a drop-in
-- replacement for 'dynMapF'.
dynMapF_ :: Monad m
         => (k -> Interval m a b)     -- ^ function to generate a new
                                      --     'Interval' for each coming @k@
                                      --     in the blip stream.
         -> a                         -- ^ "default" input to feed in
         -> Auto m (IntMap a, Blip [k]) (IntMap b)
dynMapF_ f x0 = go 0 IM.empty IM.empty
  where
    go i ks as = mkAutoM_ (goFunc i ks as)
    goFunc = _dynMapF f x0 go

-- just splitting out the functionality so that I can write this logic once
-- for both the serializing and non serializing versions
_dynMapF :: Monad m
         => (k -> Interval m a b)
         -> a
         -> (Int -> IntMap k -> IntMap (Interval m a b) -> Auto m (IntMap a, Blip [k]) (IntMap b))
         -> Int
         -> IntMap k
         -> IntMap (Interval m a b)
         -> (IntMap a, Blip [k])
         -> m (IntMap b, Auto m (IntMap a, Blip [k]) (IntMap b))
_dynMapF f x0 go i ks as (xs, news) = do
    let newks  = zip [i..] (blip [] id news)
        newas  = (map . second) f newks
        newks' = ks `IM.union` IM.fromList newks
        newas' = as `IM.union` IM.fromList newas
        newc   = i + length newks
        resMap = zipIntMapWithDefaults stepAuto Nothing (Just x0) newas' xs
    res <- sequence resMap
    let ys' = IM.mapMaybe fst res
        as' = snd <$> IM.intersection res ys'
        ks' = IM.intersection newks' ys'
    return (ys', go newc ks' as')


-- | 'Auto' multiplexer.  Stores a bunch of internal 'Auto's indexed by
-- a key.  At every step, takes a key-input pair, feeds the input to the
-- 'Auto' stored at that key and outputs the output.
--
-- If the key given does not yet have an 'Auto' stored at that key,
-- initializes a new 'Auto' at that key by using the supplied function.
--
-- Once initialized, these 'Auto's are stored there forever.
--
-- You can play around with some combinators from "Control.Auto.Switch";
-- for example, with 'resetOn', you can make 'Auto's that "reset"
-- themselves when given a certain input.  'switchOnF' could be put to use
-- here too in neat ways.
--
-- >>> let mx0 = mux (\_ -> sumFrom 0)
-- >>> let (res1, mx1) = stepAuto' mx0 ("hello", 5)
-- >>> res1
-- 5
-- >>> let (res2, mx2) = stepAuto' mx1 ("world", 3)
-- >>> res2
-- 3
-- >>> let (res3, mx3) = stepAuto' mx2 ("hello", 4)
-- >>> res3
-- 9
-- >>> streamAuto' mx3 [("world", 2), ("foo", 6), ("foo", 1), ("hello", 2)]
-- [5, 6, 7, 11]
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

-- | 'Auto' multiplexer, like 'mux', except allows update/access of many
-- 'Auto's at a time.  Instead of taking in a single key-value pair and
-- outputting a single result, takes in an entire 'Map' of key-value pairs
-- and outputs a 'Map' of key-result pairs.
--
-- >>> import qualified Data.Map as M
-- >>> let mx0 = mux (\_ -> sumFrom 0)
-- >>> let (res1, mx1) = stepAuto' mx0 (M.fromList [ ("hello", 5)
--                                                 , ("world", 3) ])
-- >>> res1
-- fromList [("hello", 5), ("world", 3)]
-- >>> let (res2, mx2) = stepAuto' mx1 (M.fromList [ ("hello", 4)
--                                                 , ("foo"  , 7) ])
-- >>> res2
-- fromList [("foo", 7), ("hello", 9)]
-- >>> let (res3, _  ) = mx2 (M.fromList [("world", 3), ("foo", 1)])
-- >>> res3
-- fromList [("foo", 8), ("world", 6)]
--
-- See 'mux' for more notes.
muxMany :: (Serialize k, Ord k, Monad m)
        => (k -> Auto m a b)    -- ^ function to create a new 'Auto' if
                                --   none at that key already exists
        -> Auto m (Map k a) (Map k b)
muxMany f = go mempty
  where
    -- go :: Map k (Auto m a b) -> Auto m (Map k a) (Map k b)
    go as = mkAutoM l (s as) (t as)
    l     = do
      ks <- get
      let as = M.fromList (map (id &&& f) ks)
      go <$> mapM resumeAuto as
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

_muxManyF :: forall k m a b. (Ord k, Monad m)
          => (k -> Auto m a b)                           -- ^ f : make new Autos
          -> (Map k (Auto m a b) -> Auto m (Map k a) (Map k b)) -- ^ go: make next step
          -> Map k (Auto m a b)                          -- ^ as: all current Autos
          -> Map k a                                     -- ^ xs: Inputs
          -> m (Map k b, Auto m (Map k a) (Map k b))     -- ^ Outputs, and next Auto.
_muxManyF f go as xs = do
    -- all the outputs of the autos with the present inputs; autos without
    --   inputs are ignored.
    outs <- sequence steps
    let ys     = fmap fst outs
        allas' = M.union (fmap snd outs) allas
    return (ys, go allas')
  where
    -- new Autos, from the function.  Only on new ones not found in `as`.
    newas :: Map k (Auto m a b)
    newas = M.mapWithKey (\k _ -> f k) (M.difference xs as)
    -- all Autos, new and old.  Prefer the old ones.
    allas :: Map k (Auto m a b)
    allas = M.union as newas
    -- Step all the autos with all the inputs.  Lose the Autos that have no
    --   corresponding input.
    steps :: Map k (m (b, Auto m a b))
    steps = M.intersectionWith stepAuto allas xs

e2m :: Either (a, b) b -> (Maybe a, b)
e2m (Left (x, y)) = (Just x , y)
e2m (Right y)     = (Nothing, y)

_muxgathermapF :: (k -> Maybe c -> Interval m a b) -> k -> (Maybe c, a) -> (Maybe c, Interval m a b)
_muxgathermapF f k (mz, _) = (mz, f k mz)

-- | Keeps an internal 'Map' of 'Interval's and, at every step, the output is
-- the last seen output of every 'Interval', indexed under the proper key.
--
-- At every step, the input is a key-value pair; 'gather' will feed that
-- input value to the 'Interval' under the proper key and update the output
-- map with that new result.
--
-- If the key offered the input is not yet a part of the collection,
-- initializes it with the given function.
--
-- Any 'Interval' that turns "off" (outputs 'Nothing') from this will be
-- immediately removed from the collection.  If something for that key is
-- received again, it will re-initialize it.
--
-- >>> let sumUntil :: Interval' Int Int
--         sumUntil = proc x -> do
--                        sums <- sumFrom 0     -< x
--                        stop <- became (> 10) -< sums
--                        before -< (sums, stop)
--     -- (a running sum, "on" until the sum is greater than 10)
-- >>> let gt0 = gather (\_ -> sumUntil)
-- >>> let (res1, gt1) = stepAuto' gt0 ("hello", 5)
-- >>> res1
-- fromList [("hello", 5)]
-- >>> let (res2, gt2) = stepAuto' gt1 ("world", 7)
-- >>> res2
-- fromList [("hello", 5), ("world", 7)]
-- >>> let (res3, gt3) = stepAuto' gt2 ("foo", 4)
-- >>> res3
-- fromList [("foo", 4), ("hello", 5), ("world", 7)]
-- >>> let (res4, gt4) = stepAuto' gt3 ("world", 8)
-- >>> res4
-- fromList [("foo", 4), ("hello", 5)]
-- >>> streamAuto' gt4 [("world", 2),("bar", 9),("world", 6),("hello", 11)]
-- [ fromList [("foo", 4), ("hello", 5), ("world", 2)]
-- , fromList [("bar", 9), ("foo", 4), ("hello", 5), ("world", 2)]
-- , fromList [("bar", 9), ("foo", 4), ("hello", 5), ("world", 8)]
-- , fromList [("bar", 9), ("foo", 4), ("world", 8)]
-- ]
--
-- In practice this ends up being a very common collection; see the
-- <https://github.com/mstksg/auto-examples auto-examples> project for many
-- examples!
--
-- Because everything needs a 'key', you don't have the fancy
-- "auto-generate new keys" feature of 'dynMap'...however, you could always
-- pull a new key from @'perBlip' 'enumFromA'@ or something.
--
-- Like with 'mux', combinators from "Control.Auto.Switch" like 'resetOn'
-- and 'switchOnF' are very useful here!
--
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


-- | Much like 'gather', except allows you to pass in multiple key-value
-- pairs every step, to update multiple internal 'Auto's.
--
-- >>> import qualified Data.Map as M
-- >>> let sumUntil :: Interval' Int Int
--         sumUntil = proc x -> do
--                        sums <- sumFrom 0     -< x
--                        stop <- became (> 10) -< sums
--                        before -< (sums, stop)
--     -- (a running sum, "on" until the sum is greater than 10)
-- >>> let gm0 = gatherMany (\_ -> sumUntil)
-- >>> let (res1, gm1) = stepAuto' gm0 (M.fromList [ ("hello", 5)
--                                                 , ("world", 7)
--                                                 ])
-- >>> res1
-- fromList [("hello", 5), ("world", 7)]
-- >>> let (res2, gm2) = stepAuto' gm1 (M.fromList [ ("foo", 4)
--                                                 , ("hello", 3)
--                                                 ])
-- >>> res2
-- fromList [("foo", 4), ("hello", 8), ("world", 7)]
-- >>> let (res3, gm3) = stepAuto' gm2 (M.fromList [ ("world", 8)
--                                                 , ("bar", 9)
--                                                 ])
-- >>> res3
-- fromList [("bar", 9), ("foo", 4), ("hello", 8)]
-- >>> let (res4, _  ) = stepAuto' gm3 (M.fromList [ ("world", 2)
--                                                 , ("bar", 10)
--                                                 ])
-- >>> res4
-- fromList [("foo", 4), ("hello", 8), ("world", 2)]
--
-- See 'gather' for more notes.
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
    mapM (mapM resumeAuto) as


gatherFMany__ :: forall k a m b c. (Ord k, Monad m)
              => (k -> Maybe c -> Interval m a b)
              -> Auto m (Map k (Either (c, a) a)) (Map k b)
gatherFMany__ f = go mempty mempty
  where
    go :: Map k (Maybe c, Auto m a (Maybe b))
       -> Map k b
       -> Auto m (Map k (Either (c, a) a)) (Map k b)
    go as ys = mkAutoM_ (_gatherFManyF f go as ys)

-- you know the type signature looks awful, but this function pretty much
-- wrote itself because of the type signature.  Haskell is awesome, isn't
-- it?  I could have never written this without Haskell's type system.
_gatherFManyF :: forall k m a b c inAuto outAuto outOut.
                  ( Ord k
                  , Monad m
                  , inAuto  ~ (Interval m a b)
                  , outAuto ~ (Auto m (Map k (Either (c, a) a)) (Map k b))
                  , outOut  ~ (Map k b, Auto m (Map k (Either (c, a) a)) (Map k b))
                  )
              => (k -> Maybe c -> inAuto)                 -- f
              -> (Map k (Maybe c, inAuto) -> Map k b -> outAuto)     -- go
              -> Map k (Maybe c, inAuto)                  -- as
              -> Map k b                                  -- ys
              -> Map k (Either (c, a) a)                  -- xs
              -> m outOut
_gatherFManyF f go as ys xs = do
    outs <- sequence steps :: m (Map k (Maybe c, (Maybe b, Auto m a (Maybe b))))
    let outs', rems   :: Map k (Maybe c, (Maybe b, Auto m a (Maybe b)))
        (outs', rems) = M.partition (isJust . fst . snd) outs
        as'           = M.difference allas rems
        ys'           = M.difference ys rems
        as''          = M.union (fmap (second snd) outs') as'
        newys         = fmap (fromJust . fst . snd) outs'
        ys''          = M.union newys ys'
    return (ys'', go as'' ys'')
  where
    _mzxs = fmap e2m xs
    newas = M.mapWithKey (_muxgathermapF f) (M.difference _mzxs as)
    allas = M.union as newas
    steps :: Map k (m (Maybe c, (Maybe b, Auto m a (Maybe b))))
    steps = M.intersectionWith interf allas _mzxs
    interf :: (Maybe c, Auto m a (Maybe b))
           -> (Maybe c, a)
           -> m (Maybe c, (Maybe b, Auto m a (Maybe b)))
    interf (mc, a) (_, x) = sequence (mc, stepAuto a x)

type MapMerge m k a b c = (k -> a -> b -> Maybe c)
                       -> (m a -> m c)
                       -> (m b -> m c)
                       -> m a -> m b -> m c

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
