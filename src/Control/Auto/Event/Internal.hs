{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Control.Auto.Event.Internal (
    Event(..)
  , merge
  , event
  ) where

-- import Data.Foldable
-- import Data.Traversable
import Data.Semigroup
import Data.Typeable
import Data.Binary
import GHC.Generics

data Event a = NoEvent
             | Event a
             deriving ( Functor
                      -- , Foldable
                      -- , Traversable
                      , Show
                      , Typeable
                      , Generic
                      )

instance Semigroup a => Monoid (Event a) where
    mempty  = NoEvent
    mappend = merge (<>)

instance Semigroup a => Semigroup (Event a) where
    (<>) = merge (<>)

instance Binary a => Binary (Event a)

merge :: (a -> a -> a) -> Event a -> Event a -> Event a
merge _ ex NoEvent          = ex
merge _ NoEvent ey          = ey
merge f (Event x) (Event y) = Event (f x y)

event :: b -> (a -> b) -> Event a -> b
event d _ NoEvent   = d
event _ f (Event x) = f x

