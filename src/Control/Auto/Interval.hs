module Control.Auto.Interval where

import Control.Auto.Core
import Control.Applicative
import Data.Binary
import Control.Auto.Event.Internal

after :: Monad m => Auto m (a, Event b) (Maybe a)
after = mkState f False
  where
    f (x, _      ) True  = (Just x , True )
    f (x, Event _) False = (Just x , True )
    f _            False = (Nothing, False)

before :: Monad m => Auto m (a, Event b) (Maybe a)
before = mkState f False
  where
    f _            True  = (Nothing, True )
    f (_, Event _) False = (Nothing, True )
    f (x, _      ) False = (Just x , False)

between :: Monad m => Auto m (a, (Event b, Event c)) (Maybe a)
between = mkState f False
  where
    f (_, (_, Event _)) _     = (Nothing, False)
    f (x, (Event _, _)) _     = (Just x , True )
    f (x, _           ) True  = (Just x , True )
    f _                 False = (Nothing, False)

hold :: (Binary a, Monad m) => Auto m (Event a) (Maybe a)
hold = mkAccum f Nothing
  where
    f x = event x Just

hold_ :: Monad m => Auto m (Event a) (Maybe a)
hold_ = mkAccum_ f Nothing
  where
    f x = event x Just

(<|?>) :: Monad m => Auto m a (Maybe b) -> Auto m a (Maybe b) -> Auto m a (Maybe b)
a1 <|?> a2 = mkAutoM ((<|?>) <$> loadAuto a1 <*> loadAuto a2)
                     (saveAuto a1 *> saveAuto a2)
                     $ \x -> do
                         Output y1 a1' <- stepAuto a1 x
                         Output y2 a2' <- stepAuto a2 x
                         let next = a1' <|?> a2'
                         return $ case (y1, y2) of
                           (y@(Just _), _) -> Output y       next
                           (_, y@(Just _)) -> Output y       next
                           _               -> Output Nothing next

(<|!>) :: Monad m => Auto m a (Maybe b) -> Auto m a b -> Auto m a b
a1 <|!> a2 = mkAutoM ((<|!>) <$> loadAuto a1 <*> loadAuto a2)
                     (saveAuto a1 *> saveAuto a2)
                     $ \x -> do
                         Output y1 a1' <- stepAuto a1 x
                         Output y2 a2' <- stepAuto a2 x
                         let next = a1' <|!> a2'
                         return $ case (y1, y2) of
                           (Just y, _) -> Output y next
                           (_     , y) -> Output y next
