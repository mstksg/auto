module Control.Auto.Collection where

import Control.Auto.Core
import Control.Monad
import Data.Maybe
import Control.Auto.Event.Internal
import Control.Applicative

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

