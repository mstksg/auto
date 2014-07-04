module Control.Auto.Event (
  -- * The Event type
    Event
  , event
  , merge
  , mergeL
  , mergeR
  -- * Step/"time" based Event streams
  , never
  , now
  , inE
  , every
  , eachAt
  , eachAt_
  -- * Modifying Event streams
  , (<&)
  , (&>)
  , once
  , notYet
  , filterE
  , takeE
  , takeWhileE
  , dropE
  , dropWhileE
  , fromEvents
  , onJust
  -- * Scanning & Accumulating Event streams
  , accumE
  , accumE_
  , scanE
  , scanE_
  , mscanE
  , mscanE_
  -- * Edge events
  , onChange
  , onChange_
  , became
  , noLonger
  , onFlip
  , became_
  , noLonger_
  , onFlip_
  , became'
  , noLonger'
  , onFlip'
  -- * Composing with Events
  , perEvent
  , perEventI
  , bindE
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Interval
import Data.Monoid
import Control.Auto.Core
import Data.Profunctor
import Control.Auto.Event.Internal
import Control.Auto.Time
import Control.Category
import Data.Binary
import Prelude hiding                  ((.), id, sequence)
import qualified Control.Auto.Generate as A

infixl 5 <&
infixl 5 &>

mergeL :: Event a -> Event a -> Event a
mergeL = merge const

mergeR :: Event a -> Event a -> Event a
mergeR = merge (flip const)



(<&) :: Monad m => Auto m a (Event b) -> Auto m a (Event b) -> Auto m a (Event b)
(<&) = liftA2 (merge const)

(&>) :: Monad m => Auto m a (Event b) -> Auto m a (Event b) -> Auto m a (Event b)
(&>) = liftA2 (merge (flip const))



never :: Monad m => Auto m a (Event b)
never = pure NoEvent

now :: Monad m => Auto m a (Event a)
now = mkState f False
  where
    f _ True  = (NoEvent, True)
    f x False = (Event x, True)

inE :: Monad m => Int -> Auto m a (Event a)
inE n = mkState f (n, False)
  where
    f _ (_, True )             = (NoEvent, (0  , True ))
    f x (i, False) | i <= 0    = (Event x, (0  , True ))
                   | otherwise = (NoEvent, (i-1, False))

every :: Monad m => Int -> Auto m a (Event a)
every n = stretchE n id

eachAt :: (Monad m, Binary b) => Int -> [b] -> Auto m a (Event b)
eachAt n xs = during (every n) . stretch n (A.fromList xs) <|!> never

eachAt_ :: Monad m => Int -> [b] -> Auto m a (Event b)
eachAt_ n xs = during (every n) . stretch_ n (A.fromList_ xs) <|!> never

filterE :: Monad m => (a -> Bool) -> Auto m (Event a) (Event a)
filterE p = arr $ \x -> case x of
                          Event x' | p x' -> x
                          _               -> NoEvent

once :: Monad m => Auto m (Event a) (Event a)
once = mkState f False
  where
    f _           True  = (NoEvent, True )
    f e@(Event _) False = (e,       True )
    f _           False = (NoEvent, False)

notYet :: Monad m => Auto m (Event a) (Event a)
notYet = mkState f False
  where
    f e         True  = (e      , True )
    f (Event _) False = (NoEvent, True )
    f _         False = (NoEvent, False)


takeE :: Monad m => Int -> Auto m (Event a) (Event a)
takeE = mkState f . max 0
  where
    f _ 0           = (NoEvent, 0  )
    f e@(Event _) i = (e      , i-1)
    f _           i = (NoEvent, i  )

takeWhileE :: Monad m => (a -> Bool) -> Auto m (Event a) (Event a)
takeWhileE p = mkState f False
  where
    f _           True        = (NoEvent, True )
    f e@(Event x) False | p x = (e      , False)
    f _           False       = (NoEvent, True )

dropE :: Monad m => Int -> Auto m (Event a) (Event a)
dropE = mkState f . max 0
  where
    f x         0 = (x      , 0  )
    f (Event _) i = (NoEvent, i-1)
    f _         i = (NoEvent, i  )

dropWhileE :: Monad m => (a -> Bool) -> Auto m (Event a) (Event a)
dropWhileE p = mkState f False
  where
    f e           True              = (e      , True )
    f e@(Event x) False | p x       = (NoEvent, False)
                        | otherwise = (e      , True )
    f _           False             = (NoEvent, False)

accumE :: (Monad m, Binary b) => (b -> a -> b) -> b -> Auto m (Event a) (Event b)
accumE f = mkState (_accumEF f)

accumE_ :: Monad m => (b -> a -> b) -> b -> Auto m (Event a) (Event b)
accumE_ f = mkState_ (_accumEF f)

_accumEF :: (b -> a -> b) -> Event a -> b -> (Event b, b)
_accumEF f e y0 = case e of
                    Event x -> let y1 = f y0 x
                               in  (Event y1, y1)
                    NoEvent ->     (NoEvent , y0)

scanE :: (Monad m, Binary b) => (b -> a -> b) -> b -> Auto m (Event a) b
scanE f = mkAccum (_scanEF f)

scanE_ :: Monad m => (b -> a -> b) -> b -> Auto m (Event a) b
scanE_ f = mkAccum_ (_scanEF f)

_scanEF :: (b -> a -> b) -> b -> Event a -> b
_scanEF f y0 = event y0 (f y0)

mscanE :: (Monad m, Monoid a, Binary a) => Auto m (Event a) a
mscanE = scanE (<>) mempty

mscanE_ :: (Monad m, Monoid a) => Auto m (Event a) a
mscanE_ = scanE_ (<>) mempty

became :: (Binary a, Monad m) => (a -> Bool) -> Auto m a (Event a)
became p = mkAccum (_becameF p) NoEvent

noLonger :: (Binary a, Monad m) => (a -> Bool) -> Auto m a (Event a)
noLonger p = became (not . p)

onFlip :: (Binary a, Monad m) => (a -> Bool) -> Auto m a (Event a)
onFlip p = became p &> noLonger p

became_ :: Monad m => (a -> Bool) -> Auto m a (Event a)
became_ p = mkAccum_ (_becameF p) NoEvent

noLonger_ :: Monad m => (a -> Bool) -> Auto m a (Event a)
noLonger_ p = became_ (not . p)

onFlip_ :: Monad m => (a -> Bool) -> Auto m a (Event a)
onFlip_ p = became_ p &> noLonger_ p

_becameF :: (a -> Bool) -> Event a -> a -> Event a
_becameF p e x | p x       = event (Event x) (const NoEvent) e
               | otherwise = NoEvent

became' :: Monad m => (a -> Bool) -> Auto m a (Event ())
became' p = mkAccum f NoEvent
  where
    f e x | p x       = event (Event ()) (const NoEvent) e
          | otherwise = NoEvent

noLonger' :: Monad m => (a -> Bool) -> Auto m a (Event ())
noLonger' p = became' (not . p)

onFlip' :: Monad m => (a -> Bool) -> Auto m a (Event Bool)
onFlip' p = fmap (True <$) (became' p) &> fmap (False <$) (noLonger' p)

onChange :: (Binary a, Eq a, Monad m) => Auto m a (Event a)
onChange = mkState _onChangeF Nothing

onChange_ :: (Eq a, Monad m) => Auto m a (Event a)
onChange_ = mkState_ _onChangeF Nothing

_onChangeF :: Eq a => a -> Maybe a -> (Event a, Maybe a)
_onChangeF x Nothing               = (NoEvent , Just x)
_onChangeF x (Just x') | x == x'   = (NoEvent , Just x')
                       | otherwise = (Event x', Just x')

onJust :: Monad m => Auto m (Maybe a) (Event a)
onJust = arr (maybe NoEvent Event)

fromEvents :: Monad m => a -> Auto m (Event a) a
fromEvents d = arr (event d id)

perEvent :: Monad m => Auto m a b -> Auto m (Event a) (Event b)
perEvent a = a_
  where
    a_ = mkAutoM (perEvent <$> loadAuto a)
                 (saveAuto a)
                 $ \x -> case x of
                           Event x' -> do
                             Output y a' <- stepAuto a x'
                             return (Output (Event y) (perEvent a'))
                           NoEvent  ->
                             return (Output NoEvent   a_           )

perEventI :: Monad m
          => Auto m (Maybe a) (Maybe b)
          -> Auto m (Event a) (Event b)
perEventI = dimap (event Nothing Just) (maybe NoEvent Event)

bindE :: Monad m => Auto m a (Event b) -> Auto m (Event a) (Event b)
bindE a = a_
  where
    a_ = mkAutoM (bindE <$> loadAuto a)
                 (saveAuto a)
                 $ \x -> case x of
                           Event x' -> do
                             Output y a' <- stepAuto a x'
                             return (Output y       (bindE a'))
                           NoEvent  ->
                             return (Output NoEvent a_        )

