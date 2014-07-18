module Control.Auto (
  -- * Types
    Output(..)
  , Auto
  , Blip
  -- * Accessors and type manipulators
  , onOutput
  , loadAuto
  , saveAuto
  , stepAuto
  , encodeAuto
  , decodeAuto
  -- * Auto constructors
  -- ** Lifting values and functions
  , mkConst
  , mkConstM
  , mkFunc
  , mkFuncM
  -- ** from State transformers
  , mkState
  , mkStateM
  , mkState_
  , mkStateM_
  -- ** from Accumulators
  , mkAccum
  , mkAccumM
  , mkAccum_
  , mkAccumM_
  -- ** Arbitrary Autos
  , mkAuto
  , mkAutoM
  , mkAuto_
  , mkAutoM_
  -- * Re-exports
  , module Control.Category
  , module Control.Applicative
  , module Control.Arrow
  , module Data.Semigroup
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Blip
import Control.Auto.Core
import Control.Category
import Data.Semigroup
