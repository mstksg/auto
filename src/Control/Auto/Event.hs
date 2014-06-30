module Control.Auto.Event (
    event
  , merge
  , mergeL
  , mergeR
  , (<&)
  , (&>)
  , never
  , now
  , after
  , every
  , eachAt
  , eachAt_
  , filterE
  , once
  , notYet
  , takeE
  , takeWhileE
  , dropE
  , dropWhileE
  , accumE
  , accumE_
  , perEvent
  , bindEvent
  ) where

import Control.Applicative
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

after :: Monad m => Int -> Auto m a (Event a)
after n = mkState f (n, False)
  where
    f _ (_, True)              = (NoEvent, (0  , True ))
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
filterE p = mkFunc $ \x -> case x of
                             Event x' | p x' -> x
                             _               -> NoEvent

once :: Monad m => Auto m (Event a) (Event a)
once = mkState f False
  where
    f _           True = (NoEvent, True )
    f e@(Event _) _    = (e,       True )
    f _           _    = (NoEvent, False)

notYet :: Monad m => Auto m (Event a) (Event a)
notYet = mkState f False
  where
    f e         True = (e      , True )
    f (Event _) _    = (NoEvent, True )
    f _         _    = (NoEvent, False)


takeE :: Monad m => Int -> Auto m (Event a) (Event a)
takeE = mkState f
  where
    f _ 0           = (NoEvent, 0  )
    f e@(Event _) i = (e      , i-1)
    f _           i = (NoEvent, i  )

takeWhileE :: Monad m => (a -> Bool) -> Auto m (Event a) (Event a)
takeWhileE p = mkState f False
  where
    f _           True       = (NoEvent, True )
    f e@(Event x) _    | p x = (e      , False)
    f _           _          = (NoEvent, True )

dropE :: Monad m => Int -> Auto m (Event a) (Event a)
dropE = mkState f
  where
    f x         0 = (x      , 0  )
    f (Event _) i = (NoEvent, i-1)
    f _         i = (NoEvent, i  )

dropWhileE :: Monad m => (a -> Bool) -> Auto m (Event a) (Event a)
dropWhileE p = mkState f False
  where
    f e           True             = (e      , True )
    f e@(Event x) _    | p x       = (NoEvent, False)
                       | otherwise = (e      , True )
    f _           _                = (NoEvent, False)

accumE :: (Monad m, Binary b) => (b -> a -> b) -> b -> Auto m (Event a) (Event b)
accumE f = mkState (_accumEF f)

accumE_ :: Monad m => (b -> a -> b) -> b -> Auto m (Event a) (Event b)
accumE_ f = mkState_ (_accumEF f)

_accumEF :: (b -> a -> b) -> Event a -> b -> (Event b, b)
_accumEF f e y0 = case e of
                    Event x -> let y1 = f y0 x
                               in  (Event y1, y1)
                    NoEvent ->     (NoEvent , y0)

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
                             return (Output NoEvent a_)

bindEvent :: Monad m => Auto m a (Event b) -> Auto m (Event a) (Event b)
bindEvent a = a_
  where
    a_ = mkAutoM (bindEvent <$> loadAuto a)
                 (saveAuto a)
                 $ \x -> case x of
                           Event x' -> do
                             Output y a' <- stepAuto a x'
                             return (Output y (bindEvent a'))
                           NoEvent  ->
                             return (Output NoEvent a_)
