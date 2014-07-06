{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Control.Auto.Blip.Internal (
    Blip(..)
  , merge
  , blip
  ) where

-- import Data.Foldable
-- import Data.Traversable
import Data.Semigroup
import Data.Typeable
import Data.Serialize
import GHC.Generics

data Blip a = NoBlip
             | Blip a
             deriving ( Functor
                      -- , Foldable
                      -- , Traversable
                      , Show
                      , Typeable
                      , Generic
                      )

instance Semigroup a => Monoid (Blip a) where
    mempty  = NoBlip
    mappend = merge (<>)

instance Semigroup a => Semigroup (Blip a) where
    (<>) = merge (<>)

instance Serialize a => Serialize (Blip a)

merge :: (a -> a -> a) -> Blip a -> Blip a -> Blip a
merge _ ex NoBlip          = ex
merge _ NoBlip ey          = ey
merge f (Blip x) (Blip y) = Blip (f x y)

blip :: b -> (a -> b) -> Blip a -> b
blip d _ NoBlip   = d
blip _ f (Blip x) = f x

