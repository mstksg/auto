module Control.Auto.Interval (
  -- * Static intervals
    inhibit
  , always
  , uninhibit
  , uninhibitWith
  , for
  , inhibitFor
  , window
  -- * Filter intervals
  , when
  , unless
  -- * Choice
  , (<|?>)
  , (<|!>)
  -- * Event-based intervals
  , after
  , before
  , between
  , hold
  , hold_
  , holdFor
  , holdFor_
  -- * Composing with intervals
  , during
  , bindI
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Core
import Control.Auto.Event.Internal
import Control.Category
import Data.Binary
import Data.Maybe
import Prelude hiding              ((.), id)

inhibit :: Monad m => Auto m a (Maybe b)
inhibit = pure Nothing

always :: Monad m => Auto m a (Maybe a)
always = arr Just

uninhibit :: Monad m => a -> Auto m (Maybe a) a
uninhibit d = arr (fromMaybe d)

uninhibitWith :: Monad m => b -> (a -> b) -> Auto m (Maybe a) b
uninhibitWith d f = arr (maybe d f)

for :: Monad m => Int -> Auto m a (Maybe a)
for = mkState f . max 0
  where
    f _ 0 = (Nothing, 0    )
    f x i = (Just x , i - 1)

inhibitFor :: Monad m => Int -> Auto m a (Maybe a)
inhibitFor = mkState f . max 0
  where
    f x 0 = (Just x , 0    )
    f _ i = (Nothing, i - 1)

window :: Monad m => Int -> Int -> Auto m a (Maybe a)
window x y = bindI (inhibitFor x) . for y

when :: Monad m => (a -> Bool) -> Auto m a (Maybe a)
when p = arr f
  where
    f x | p x       = Just x
        | otherwise = Nothing

unless :: Monad m => (a -> Bool) -> Auto m a (Maybe a)
unless p = arr f
  where
    f x | p x       = Nothing
        | otherwise = Just x

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

holdFor :: (Binary a, Monad m) => Int -> Auto m (Event a) (Maybe a)
holdFor n = fst <$> mkAccum (_holdForF n) (Nothing, max 0 n)

holdFor_ :: Monad m => Int -> Auto m (Event a) (Maybe a)
holdFor_ n = fst <$> mkAccum_ (_holdForF n) (Nothing, max 0 n)

_holdForF :: Int -> (Maybe a, Int) -> Event a -> (Maybe a, Int)
_holdForF n = f   -- n should be >= 0
  where
    f _      (Event x) = (Just x , n    )
    f (_, 0) _         = (Nothing, 0    )
    f (x, i) _         = (x      , i - 1)


-- It feels weird that both wires are stepped (even the second one), but
-- that's how netwire does it so I guess it's okay.

(<|?>) :: Monad m => Auto m a (Maybe b) -> Auto m a (Maybe b) -> Auto m a (Maybe b)
a1 <|?> a2 = mkAutoM (liftA2 (<|?>) (loadAuto a1) (loadAuto a2))
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
a1 <|!> a2 = fmap fromJust (a1 <|?> fmap Just a2)



during :: Monad m => Auto m a b -> Auto m (Maybe a) (Maybe b)
during a = a_
  where
    a_ = mkAutoM (during <$> loadAuto a)
                 (saveAuto a)
                 $ \x -> case x of
                           Just x' -> do
                             Output y a' <- stepAuto a x'
                             return (Output (Just y) (during a'))
                           Nothing ->
                             return (Output Nothing  a_         )

bindI :: Monad m => Auto m a (Maybe b) -> Auto m (Maybe a) (Maybe b)
bindI a = a_
  where
    a_ = mkAutoM (bindI <$> loadAuto a)
                 (saveAuto a)
                 $ \x -> case x of
                     Just x' -> do
                       Output y a' <- stepAuto a x'
                       return (Output y       (bindI a'))
                     Nothing ->
                       return (Output Nothing a_        )
