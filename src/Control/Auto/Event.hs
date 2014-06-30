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
  -- * Scanning/Accumulating Event streams
  , accumE
  , accumE_
  -- * Edge events
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
  , bindE
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Core
import Control.Auto.Event.Internal
import Data.Binary

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
every n | n <= 0    = error "every: Non-positive interval"
        | otherwise = mkState f 1
  where
    f x 1 = (Event x, n  )
    f _ i = (NoEvent, i-1)

eachAt :: (Monad m, Binary b) => Int -> [b] -> Auto m a (Event b)
eachAt n xs | n <= 0    = error "eachAt: Non-positive interval"
            | otherwise = mkState (const f) (xs, 1)
  where
    f = _eachAtF n

eachAt_ :: Monad m => Int -> [b] -> Auto m a (Event b)
eachAt_ n xs | n <= 0    = error "eachAt: Non-positive interval"
             | otherwise = mkState_ (const f) (xs, 1)
  where
    f = _eachAtF n

_eachAtF :: Int -> ([a], Int) -> (Event a, ([a], Int))
_eachAtF n s = case s of
                 ([]  , _) -> (NoEvent, ([], 0  ))
                 (y:ys, 1) -> (Event y, (ys, n  ))
                 (_   , 0) -> (NoEvent, ([], 0  ))
                 (ys  , i) -> (NoEvent, (ys, i-1))

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
