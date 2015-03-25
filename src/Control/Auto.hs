-- |
-- Module      : Control.Auto
-- Description : Main entry point to the /auto/ library.
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- This module serves as the main entry point for the library; these are
-- all basically re-exports.  The re-exports are chosen so you can start
-- doing "normal things" off the bat, including all of the types used in
-- this library.
--
-- Conspicuously missing are the most of the tools for working with
-- 'Interval', 'Blip' streams, switches, and the "collection" autos; those
-- are all pretty heavy, and if you do end up working with any of those
-- tools, simply importing the appropriate module should give you all you
-- need.
--
-- See the <https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md tutorial>
-- if you need help getting started!
--

module Control.Auto (
  -- * Types
  -- ** Auto
    Auto
  , Auto'
  -- ** Misc
  , Blip
  , Interval
  , Interval'
  -- * Working with 'Auto'
  -- ** Running
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
  , unserialize
  -- ** Strictness
  , forcer
  , seqer
  -- ** Internal monad
  , hoistA
  , generalizeA
  -- * Auto constructors
  , arrM
  , arrD
  -- ** from Accumulators
  -- *** Result-first
  , accum
  , accum_
  , accumM
  , accumM_
  -- *** Initial accumulator-first
  , accumD
  , accumD_
  , accumMD
  , accumMD_
  -- ** from State transformers
  , mkState
  , mkStateM
  , mkState_
  , mkStateM_
  -- ** Generators
  -- *** Effects
  , effect
  -- , exec
  -- *** Iterators
  , iterator
  , iterator_
  , iteratorM
  , iteratorM_
  -- * Common 'Auto's and combinators
  -- ** Processes
  , sumFrom
  , sumFrom_
  , sumFromD
  , sumFromD_
  , productFrom
  , productFrom_
  , mappender
  , mappender_
  , mappendFrom
  , lastVal
  , lastVal_
  , delay
  , delay_
  , count
  -- ** Switches
  , (-->)
  , (-?>)
  -- ** Blips
  , emitJusts
  , emitOn
  , fromBlips
  , fromBlipsWith
  , holdWith
  , holdWith_
  , perBlip
  , never
  , immediately
  -- ** Intervals
  , onFor
  , during
  , off
  , toOn
  , fromInterval
  -- * Running
  , interactAuto
  , interactRS
  , toEffectStream
  -- * Re-exports
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Category
  , module Data.Functor.Identity
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
import Control.Auto.Switch
import Control.Auto.Time
import Control.Category
import Data.Functor.Identity
import Data.Semigroup
