module Control.Auto (
  -- * Types
  -- ** Output
    Output(..)
  , Output'
  , onOutput
  -- ** Auto
  , Auto
  , Auto'
  -- ** Misc
  , Blip
  , Interval
  , Interval'
  -- * Running
  , stepAuto
  , stepAuto'
  , evalAuto
  , evalAuto'
  , streamAuto
  , streamAuto'
  , stepAutoN
  , stepAutoN'
  -- ** Serializing
  -- | See the header of the "serializing" section of "Control.Auto.Core"
  -- for more detail on how these work.
  , encodeAuto
  , decodeAuto
  , readAuto
  , writeAuto
  -- ** Strictness
  , forcer
  , seqer
  -- ** Fixed points
  , lastVal
  , lastVal_
  -- * Auto constructors
  , arrM
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
  -- ** from State transformers
  , mkState
  , mkStateM
  , mkState_
  , mkStateM_
  -- ** Generators
  -- *** Effects
  , effect
  , exec
  -- *** Iterators
  , iterator
  , iterator_
  , iteratorM
  , iteratorM_
  -- * Common processes
  , sumFrom
  , productFrom
  , mappender
  , mappendFrom
  -- * Running
  , interactAuto
  , interactRS
  -- * Re-exports
  , module Control.Category
  , module Control.Applicative
  , module Control.Arrow
  , module Data.Semigroup
  ) where

import Control.Applicative
import Control.Arrow hiding   (loop)
import Control.Auto.Blip
import Control.Auto.Core
import Control.Auto.Effects
import Control.Auto.Generate
import Control.Auto.Interval
import Control.Auto.Process
import Control.Auto.Run
import Control.Auto.Serialize
import Control.Auto.Time
import Control.Category
import Data.Semigroup
