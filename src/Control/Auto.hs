module Control.Auto (
  -- * Output
    Output(..)
  , Output'
  , onOutput
  -- * Auto
  , Auto
  , Auto'
  -- ** Running
  -- TODO: stepAutoN
  , stepAuto
  , stepAuto'
  -- ** Serializing
  -- | See the header of the "serializing" section of "Control.Auto.Core"
  -- for more detail on how these work.
  , encodeAuto
  , decodeAuto
  -- * Blip
  , Blip
  -- ** Strictness
  , forcer
  , seqer
  -- * Auto constructors
  -- ** from State transformers
  , mkState
  , mkStateM
  , mkState_
  , mkStateM_
  -- ** from Accumulators
  -- *** Result-first
  , mkAccum
  , mkAccum_
  , mkAccumM
  , mkAccumM_
  -- *** Initial accumulator-first
  , mkAccumD
  , mkAccumD_
  , mkAccumMD
  , mkAccumMD_
  -- * Re-exports
  , module Control.Category
  , module Control.Applicative
  , module Control.Arrow
  , module Data.Semigroup
  ) where

import Control.Applicative
import Control.Arrow hiding (loop)
import Control.Auto.Blip
import Control.Auto.Core
import Control.Category
import Data.Semigroup
