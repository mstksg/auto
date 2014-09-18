module Control.Auto (
  -- * Output
    Output(..)
  , Output'
  , onOutput
  -- * Blip
  , Blip
  -- * Auto
  , Auto
  , Auto'
  -- ** Running
  , stepAuto
  , stepAuto'
  , stepAutoN
  , stepAutoN'
  -- ** Serializing
  -- | See the header of the "serializing" section of "Control.Auto.Core"
  -- for more detail on how these work.
  , encodeAuto
  , decodeAuto
  -- ** Strictness
  , forcer
  , seqer
  -- ** Fixed points
  , lastVal
  , lastVal_
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
import Control.Auto.Run
import Control.Auto.Time
import Control.Category
import Data.Semigroup
