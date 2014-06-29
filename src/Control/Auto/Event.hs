{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Auto.Event where

import Control.Applicative
import Control.Auto.Core
import Data.Binary
import Data.Semigroup
import Data.Typeable

infixl 5 <&
infixl 5 &>

data Event a = NoEvent
             | Event a
             deriving (Functor, Typeable, Show)

instance Semigroup a => Monoid (Event a) where
    mempty  = NoEvent
    mappend = merge (<>)

instance Semigroup a => Semigroup (Event a) where
    (<>) = merge (<>)


event :: b -> (a -> b) -> Event a -> b
event d _ NoEvent   = d
event _ f (Event x) = f x

merge :: (a -> a -> a) -> Event a -> Event a -> Event a
merge _ ex NoEvent          = ex
merge _ NoEvent ey          = ey
merge f (Event x) (Event y) = Event (f x y)

(<&) :: Monad m => Auto m a (Event b) -> Auto m a (Event b) -> Auto m a (Event b)
(<&) = liftA2 (merge const)

(&>) :: Monad m => Auto m a (Event b) -> Auto m a (Event b) -> Auto m a (Event b)
(&>) = liftA2 (merge (flip const))





never :: Monad m => Auto m a (Event b)
never = pure NoEvent

now :: Monad m => Auto m a (Event a)
now = mkState f False
  where
    f x False = (Event x, True)
    f _ True  = (NoEvent, True)

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
    f ([]  , _) = (NoEvent, ([], 0  ))
    f (y:ys, 1) = (Event y, (ys, n  ))
    f (_   , 0) = (NoEvent, ([], 0  ))
    f (ys  , i) = (NoEvent, (ys, i-1))

eachAt_ :: Monad m => Int -> [b] -> Auto m a (Event b)
eachAt_ n xs | n <= 0    = error "eachAt: Non-positive interval"
             | otherwise = mkState_ (const f) (xs, 1)
  where
    f ([]  , _) = (NoEvent, ([], 0  ))
    f (y:ys, 1) = (Event y, (ys, n  ))
    f (_   , 0) = (NoEvent, ([], 0  ))
    f (ys  , i) = (NoEvent, (ys, i-1))
