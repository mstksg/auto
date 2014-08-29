{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Control.Auto.Blip.Internal
-- Description : Exposing internal unsafe functions for working with
--               'Blip'.
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- This module exposes an "unsafe" interface for working with the internal
-- representation of "'Blip' streams".  If you are programming at the logic
-- level or the application level, you should thoroughly be able to avoid
-- importing this, and should be happy with importing the 'Blip' type from
-- "Control.Auto" and 'Blip' stream manipulators from "Control.Auto.Blip".
--
-- If, however, you are programming a framework, library, or backend, you
-- might find it useful to manually create your own 'Blip' streams/sources.
-- In this case, this module will be useful.
--
-- It is important, as with most of this library in general, to always keep
-- in mind when you are programming at the "logic" level, and when you are
-- programming at the "backend" level.  If you can justify that you are at
-- the backend level and not at the logic level of whatever you are
-- programming, then this is useful.  See more on "Control.Auto.Tutorial".
--
-- Be sure, of course, that whatever 'Blip' streams you do manually
-- construct preserve "'Blip' semantics", which is further defined in
-- "Control.Auto.Blip" and "Control.Auto.Tutorial".
--
-- You have been warned!
--

module Control.Auto.Blip.Internal (
    Blip(..)
  , merge
  , blip
  ) where

import Control.DeepSeq
import Data.Semigroup
import Data.Serialize
import Data.Typeable
import GHC.Generics

-- | A type representing a "discrete" sort of event-like thing.  It's
-- supposed to represent something that happens alone, and one at a time,
-- as opposed to things that are "on" or "off" for large intervals at
-- a time.
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
--
-- If you are creating a framework, library, or backend, you might want to
-- manually create 'Blip' stream-producing 'Auto's for your users to
-- access.  In this case, you can import the constructors and useful
-- internal (and, of course, semantically unsafe) functions from
-- "Control.Auto.Blip.Internal".
data Blip a =  NoBlip
             | Blip !a
             deriving ( Functor
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

-- | Merge two 'Blip's with a merging function.  Is only a occuring 'Blip'
-- if *both* 'Blip's are simultaneously occuring.
merge :: (a -> a -> a)      -- ^ merging function
      -> Blip a
      -> Blip a
      -> Blip a
merge _ ex NoBlip          = ex
merge _ NoBlip ey          = ey
merge f (Blip x) (Blip y) = Blip (f x y)

-- | Destruct a 'Blip' by giving a default result if the 'Blip' is
-- non-occuring and a function to apply on the contents, if the 'Blip' is
-- occuring.
--
-- Try not to use if possible, unless you are a framework developer.  If
-- you're just making an application, try to use the other various
-- combinators in this library.  It'll help you preserve the semantics of
-- what it means to be 'Blip'py.
--
-- Analogous to 'maybe' from "Prelude".
blip :: b -> (a -> b) -> Blip a -> b
blip d _ NoBlip   = d
blip _ f (Blip x) = f x

