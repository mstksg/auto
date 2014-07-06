{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}

module Control.Auto.Process.Random (
  -- * Streams of random values
    rands
  , stdRands
  , rands_
  , randsM
  , stdRandsM
  , randsM_
  -- * Bernoulli (on/off) processes
  , bernoulli
  , stdBernoulli
  , bernoulli_
  -- * Follow Markov chains
  , markov
  , stdMarkov
  , markov_
  , stdMarkov_
  ) where

import Control.Applicative
import Control.Auto.Blip
import Control.Auto.Core
import Control.Category
import Data.Serialize
import Data.Foldable
import Data.Map.Strict           (Map)
import Prelude hiding            (id, (.), concat, concatMap, sum)
import System.Random
import qualified Data.Map.Strict as M

stdRands :: Monad m
         => (StdGen -> (b, StdGen)) -- ^ Random function
         -> StdGen                  -- ^ Initial generator
         -> Auto m a b
stdRands r g = mkState f (show g)
  where
    f _ sg = let (res, g') = r (read sg)
             in  (res, show g')

rands :: (Serialize g, RandomGen g, Monad m)
      => (g -> (b, g)) -- ^ Random function
      -> g             -- ^ Initial generator
      -> Auto m a b
rands r = mkState (\_ g -> r g)

rands_ :: (RandomGen g, Monad m)
       => (g -> (b, g))   -- ^ Random function
       -> g               -- ^ Initial generator
       -> Auto m a b
rands_ r = mkState_ (\_ g -> r g)

stdRandsM :: Monad m
          => (StdGen -> m (b, StdGen))
          -> StdGen
          -> Auto m a b
stdRandsM r g = mkStateM f (show g)
  where
    f _ sg = do
      (res, g') <- r (read sg)
      return (res, show g')

randsM :: (Serialize g, RandomGen g, Monad m)
       => (g -> m (b, g))
       -> g
       -> Auto m a b
randsM r = mkStateM (\_ g -> r g)

randsM_ :: (RandomGen g, Monad m)
        => (g -> m (b, g))
        -> g
        -> Auto m a b
randsM_ r = mkStateM_ (\_ g -> r g)

markov :: forall a b m g. (Serialize g, Serialize b, RandomGen g, Monad m, Ord b)
       => Map b (Map b Double)
       -> g
       -> b
       -> Auto m a b
markov tm g0 b0 = mkState (const (_markovF tm _rF)) (b0, g0)

stdMarkov :: forall a b m. (Serialize b, Monad m, Ord b)
          => Map b (Map b Double)
          -> StdGen
          -> b
          -> Auto m a b
stdMarkov tm g0 b0 = mkState f (b0, show g0)
  where
    f _ (b, sg) = let (b', (_, g')) = _markovF tm _rF (b, read sg :: StdGen)
                  in  (b', (b', show g'))

stdMarkov_ :: forall a b m. (Monad m, Ord b)
           => Map b (Map b Double)
           -> StdGen
           -> b
           -> Auto m a b
stdMarkov_ tm g0 b0 = mkAuto (stdMarkov_ tm . read <$> get <*> pure b0)
                             (put (show g0))
                             $ \_ -> let (b', (_, g')) = _markovF tm _rF (b0, g0)
                                     in  Output b' (stdMarkov_ tm g' b')


markov_ :: forall a b m g. (RandomGen g, Monad m, Ord b)
        => Map b (Map b Double)
        -> g
        -> b
        -> Auto m a b
markov_ tm g0 b0 = mkState_ (const (_markovF tm _rF)) (b0, g0)

_rF :: RandomGen g => Double -> g -> (Double, g)
_rF x = randomR (0, x)

_markovF :: Ord b => Map b (Map b Double) -> (Double -> g -> (Double, g)) -> (b, g) -> (b, (b, g))
_markovF tm f (b0, g) | wsum <= 0 = (b0, (b0, g ))
                      | otherwise = (wr, (wr, wg))
  where
    tm' = M.lookup b0 tm
    weights = concatMap M.toList tm'
    wsum = sum . map snd $ weights
    (wr, wg) = weightedRandom weights g f

weightedRandom :: [(a, Double)] -> g -> (Double -> g -> (Double, g)) -> (a, g)
weightedRandom [] _ _ = error "weightedRandom: empty list."
weightedRandom xs g f = er0 `seq` (x, g')
  where
    s       = sum (map snd xs)
    cs      = scanl1 (\(_,q) (y,s') -> (y, s' + q)) xs
    (p, g') = f s g
    x       = fst . head . dropWhile ((< p) . snd) $ cs
    er0 | s <= 0    = error "weightedRandom: weights sum to zero."
        | otherwise = ()

bernoulli :: (Serialize g, RandomGen g, Monad m) => Double -> g -> Auto m a (Blip a)
bernoulli p g = proc x -> do
    q <- rands (randomR (0, 1)) g -< ()
    b <- emitOn (<= p)            -< q
    id -< x <$ b

bernoulli_ :: (RandomGen g, Monad m) => Double -> g -> Auto m a (Blip a)
bernoulli_ p g = proc x -> do
    q <- rands_ (randomR (0, 1)) g -< ()
    b <- emitOn (<= p)            -< q
    id -< x <$ b

stdBernoulli :: Monad m => Double -> StdGen -> Auto m a (Blip a)
stdBernoulli p g = proc x -> do
    q <- stdRands (randomR (0, 1)) g -< ()
    b <- emitOn (<= p)               -< q
    id -< x <$ b

