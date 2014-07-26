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
import Control.DeepSeq
import Data.Semigroup
import Data.Serialize
import Data.Typeable
import GHC.Generics

-- | A type representing a "discrete" sort of event-like thing.  It's
-- represents something that happens alone, and one at a time, as opposed
-- to things that are "on" or "off" for large intervals at a time.
--
-- It's here mainly because it's a pretty useful abstraction in the context
-- of the many combinators found in various modules of this library.  If
-- you think of an @'Auto' m a ('Blip' b)@ as a "'Blip' stream", then there
-- are various combinators and functions that are specifically designed to
-- manipulate "'Blip' streams".
--
-- For the purposes of the semantics of what 'Blip' is supposed to
-- represent, its constructors are hidden.  (Almost) all of the various
-- 'Blip' combinators (and its very useful 'Functor' instance) "preserve
-- 'Blip'ness" --- one-at-a-time occurrences remain one-at-a-time under all
-- of these combinators, and you should have enough so that direct access
-- to the constructor is not needed.
data Blip a =  NoBlip
             | Blip !a
             deriving ( Functor
                      -- , Foldable
                      -- , Traversable
                      , Show
                      , Typeable
                      , Generic
                      )

instance Semigroup a => Semigroup (Blip a) where
    (<>) = merge (<>)

instance Semigroup a => Monoid (Blip a) where
    mempty  = NoBlip
    mappend = merge (<>)

instance Serialize a => Serialize (Blip a)
instance NFData a => NFData (Blip a)

-- TODO: I don't think i can instance NFData like that?

merge :: (a -> a -> a) -> Blip a -> Blip a -> Blip a
merge _ ex NoBlip          = ex
merge _ NoBlip ey          = ey
merge f (Blip x) (Blip y) = Blip (f x y)

blip :: b -> (a -> b) -> Blip a -> b
blip d _ NoBlip   = d
blip _ f (Blip x) = f x

