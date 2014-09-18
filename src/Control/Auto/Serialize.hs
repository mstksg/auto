{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Control.Auto.Serialize
-- Description : Serializing and deserializing 'Auto's to and from disk,
--               and also 'Auto' transformers focused around serialization.
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- This module provides tools for working with the automatically derived
-- serializability and resumability of 'Auto's.  The first half contains
-- boring wrappers around encoding and decoding to and from binary,
-- filepaths on disk, etc.
--
-- The second half contains 'Auto' transformers that "imbue" an 'Auto' with
-- IO serialization abilities.  Note that these all require an underlying
-- 'Monad' that is an instance of 'MonadIO'.
--
-- You have "identity-like" transformers that take an 'Auto' and spit it
-- back out operationally unchanged...but every step, it might do some
-- behind-the-scenes saving or re-load itself from disk when it is first
-- stepped.  Or you have some "trigger enhancers" that take normal 'Auto's
-- and give you the ability to "trigger" saving and loading events on the
-- 'Auto' using the 'Blip' mechanisms from "Control.Auto.Blip".
--

module Control.Auto.Serialize (
  -- * Serializing and deserializing 'Auto's from binary and from disk.
  -- ** To and from "Data.Serialize" types
    saveAuto
  , loadAuto
  -- ** To and from binary
  , encodeAuto
  , decodeAuto
  -- ** To and from disk
  , writeAuto
  , readAuto
  , readAuto'
  -- * 'Auto' transformers
  -- ** Implicit automatic serialization
  , saving
  , loading'
  , loading
  , serializing'
  , serializing
  -- ** Triggered ('Blip'-based) automatic serialization
  -- $onfrom
  -- *** Intrinsic triggering
  , saveFromB
  , loadFromB'
  , loadFromB
  -- *** External triggering
  , saveOnB
  , loadOnB'
  , loadOnB
  ) where

import Control.Auto.Blip.Internal
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import Control.Applicative
import Control.Auto.Core
import qualified Data.ByteString as B

-- | Give a 'FilePath' and an 'Auto', and 'readAuto' will attempt to resume
-- the saved state of the 'Auto' from disk, reading from the given
-- 'FilePath'.  Will return 'Left' upon a decoding error, with the error,
-- and 'Right' if the decoding is succesful.
readAuto :: FilePath        -- ^ filepath to read from
         -> Auto m a b      -- ^ 'Auto' to resume
         -> IO (Either String (Auto m a b))
readAuto fp a = decodeAuto a <$> B.readFile fp

-- | Like 'readAuto', but if any 'IO' or decoding error pops up, it is
-- silently ignored; the /original/ 'Auto' (passed to 'readAuto'') is
-- returned unchanged.
--
-- Useful if you want to "resume an 'Auto' in case there is a save state,
-- and otherwise use it as-is if there isn't".
readAuto' :: FilePath                   -- ^ filepath to read from
          -> Auto m a b                 -- ^ 'Auto' to resume (or return
                                        --   unchanged)
          -> IO (Auto m a b)
readAuto' = go
  where
    go :: forall a m b. FilePath
       -> Auto m a b
       -> IO (Auto m a b)
    go fp a = do
        esa <- try (readAuto fp a) :: IO (Either SomeException (Either String (Auto m a b)))
        case esa of
          Right (Right a') -> return a'
          _                -> return a

-- | Like 'readAuto', but will throw a runtime exception on a failure to
-- decode.
readAutoErr :: FilePath             -- ^ filepath to read from
            -> Auto m a b           -- ^ 'Auto' to resume
            -> IO (Auto m a b)
readAutoErr fp a = do
    esa <- readAuto fp a
    return $ case esa of
      Left e   -> error $ "readAutoErr: Corrupted Auto binary -- " ++ e
      Right a' -> a'

-- | Given a 'FilePath' and an 'Auto', serialize and freeze the state of
-- the 'Auto' as binary to that 'FilePath'.
writeAuto :: FilePath       -- ^ filepath to write to
          -> Auto m a b     -- ^ 'Auto' to serialize
          -> IO ()
writeAuto fp a = B.writeFile fp (encodeAuto a)

-- | "Transforms" the given 'Auto' into an 'Auto' that constantly saves its
-- state to the given 'FilePath' at every "step".  Requires an underlying
-- 'MonadIO'.
--
-- Note that (unless the 'Auto' depends on IO), the resulting 'Auto' is
-- meant to be operationally /identical/ in its inputs/outputs to the
-- original one.
--
saving :: MonadIO m
       => FilePath          -- ^ filepath to write to
       -> Auto m a b        -- ^ 'Auto' to transform
       -> Auto m a b
saving fp a = mkAutoM (saving fp <$> loadAuto a)
                      (saveAuto a)
                      $ \x -> do
                          Output y a' <- stepAuto a x
                          liftIO $ writeAuto fp a'
                          return (Output y (saving fp a'))

-- | "Transforms" the given 'Auto' into an 'Auto' that, when you /first/
-- try to run or step it, "loads" itself from disk at the given 'FilePath'.
--
-- Will throw a runtime exception on either an I/O error or a decoding
-- error.
--
-- Note that (unless the 'Auto' depends on IO), the resulting 'Auto' is
-- meant to be operationally /identical/ in its inputs/outputs to the
-- /fast-forwarded/ original 'Auto'.
--
loading :: MonadIO m
        => FilePath         -- ^ filepath to read from
        -> Auto m a b       -- ^ 'Auto' to transform
        -> Auto m a b
loading fp a0 = mkAutoM (loading fp <$> loadAuto a0)
                         (saveAuto a0)
                         $ \x -> do
                             a <- liftM loaded . liftIO $ readAutoErr fp a0
                             stepAuto a x
  where
    loaded a = mkAutoM (loading' fp <$> loadAuto a)
                       (saveAuto a)
                       $ \x -> do
                           Output y a' <- stepAuto a x
                           return (Output y (loaded a'))



-- | Like 'loading', except suppresses all I/O and decoding errors; if
-- there are errors, it returns back the given 'Auto' as-is.
--
-- Useful for when you aren't sure the save state is on disk or not yet,
-- and want to resume it only in the case that it is.
loading' :: MonadIO m
         => FilePath        -- ^ filepath to read from
         -> Auto m a b      -- ^ 'Auto' to transform (or return unchanged)
         -> Auto m a b
loading' fp a0 = mkAutoM (loading' fp <$> loadAuto a0)
                         (saveAuto a0)
                         $ \x -> do
                             a <- liftM loaded (liftIO (readAuto' fp a0))
                             stepAuto a x
  where
    loaded a = mkAutoM (loading' fp <$> loadAuto a)
                       (saveAuto a)
                       $ \x -> do
                           Output y a' <- stepAuto a x
                           return (Output y (loaded a'))

-- | A combination of 'saving' and 'loading'.  When the 'Auto' is first
-- run, it loads the save state from the given 'FilePath' and fast forwards
-- it.  Then, subsequently, it updates the save state on disk on every
-- step.
serializing :: MonadIO m
            => FilePath     -- ^ filepath to read from and write to
            -> Auto m a b   -- ^ 'Auto' to transform
            -> Auto m a b
serializing fp a = loading fp (saving fp a)

-- | Like 'serializing', except suppresses all I/O and decoding errors.
--
-- Useful in the case that when the 'Auto' is first run and there is no
-- save state yet on disk (or the save state is corrupted), it'll "start
-- a new one"; if there is one, it'll load it automatically.  Then, on
-- every further step in both cases, it'll update the save state.
serializing' :: MonadIO m
             => FilePath        -- ^ filepath to read from and write to
             -> Auto m a b      -- ^ 'Auto' to transform
             -> Auto m a b
serializing' fp a = loading' fp (saving fp a)

-- $onfrom
--
-- Note that these follow the naming conventions from
-- "Control.Auto.Switch": Something "from" a 'Blip' stream is a thing
-- triggered by the 'Auto' itself, and something "on" a 'Blip' stream is
-- a thing triggered externally, from another 'Auto'.

-- | Takes an 'Auto' that produces a 'Blip' stream with a 'FilePath' and
-- a value, and turns it into an 'Auto' that, outwardly, produces just the
-- value.
--
-- Whenever the output 'Blip' stream emits, it automatically serializes and
-- saves the state of the 'Auto' to the emitted 'FilePath'.
--
-- In practice, this allows any 'Auto' to basically control when it wants
-- to "save", by providing a 'Blip' stream.
--
-- The following is an alternative implementation of 'saving', except
-- saving every two steps instead of every step:
--
-- @
--     saving2 fp a = 'saveFromB' (a '&&&' ('every' 2 . 'pure' fp))
-- @
--
-- Or, in proc notation:
--
-- > saving2 fp a = saveFromB $ proc x -> do
-- >     y <- a       -< x
-- >     b <- every 2 -< fp
-- >     id -< (y, b)
--
-- (Recall that @'every' n@ is the "Auto" that emits the received value
-- every @n@ steps)
--
-- In useful real-world cases, you can have the 'Auto' decide whether or
-- not to save itself based on its input.  Like, for example, when it
-- detects a certain user command, or when the user has reached a given
-- location.
--
-- The following takes a 'FilePath' and an 'Auto' (@a@), and turns it into
-- an 'Auto' that "saves" whenever @a@ crosses over from positive to
-- negative.
--
-- @
--     saveOnNegative fp a = saveFromB $ proc x -> do
--         y       <- a            -< x
--         saveNow <- 'became' (< 0) -< y
--         id       -< (y, fp '<$' saveNow)
-- @
--
-- Contrast to 'saveOnB', where the saves are triggered by outside input.
-- In this case, the saves are triggered by the 'Auto' to be saved itself.
--
saveFromB :: MonadIO m
          => Auto m a (b, Blip FilePath)    -- ^ 'Auto' producing a value
                                            --   @b@ and a 'Blip' stream
                                            --   with a 'FilePath' to save
                                            --   to
          -> Auto m a b
saveFromB a = mkAutoM (saveFromB <$> loadAuto a)
                      (saveAuto a)
                      $ \x -> do
                          Output (y, b) a' <- stepAuto a x
                          case b of
                            Blip p -> liftIO $ writeAuto p a'
                            NoBlip -> return ()
                          return (Output y (saveFromB a'))

-- | Takes an 'Auto' that outputs a @b@ and a 'Blip' stream of 'FilePath's
-- and returns an 'Auto' that ouputs only that @b@ stream...but every time
-- the 'Blip' stream emits, it "resets/loads" itself from that 'FilePath'.
--
-- The following is a re-implementation of 'loading'...except delayed by
-- one (the second step that is run is the first "resumed" step).
--
-- @
--     loading2 fp a = 'loadFromB' $ proc x -> do
--         y       <- a           -< x
--         loadNow <- 'immediately' -< fp
--         'id'       -< (y, loadNow)
-- @
--
-- (the 'Blip' stream emits only once, immediately, to re-load).
--
-- In the real world, you could have the 'Auto' decide to reset or resume
-- itself based on a user command:
--
-- @
--     loadFrom = loadFromB $ proc x -> do
--         steps  <- count -< ()
--         toLoad <- case words x of
--                       ("load":fp:_) -> do
--                           immediately -< fp
--                       _             -> do
--                           never       -< ()
--         id      -< (steps, toLoad)
-- @
--
-- This will throw a runtime error on an IO exception or parsing error.
--
loadFromB :: MonadIO m => Auto m a (b, Blip FilePath) -> Auto m a b
loadFromB a = mkAutoM (loadFromB' <$> loadAuto a)
                      (saveAuto a)
                      $ \x -> do
                          Output (y, b) a' <- stepAuto a x
                          a'' <- case b of
                                   Blip p -> liftIO $ readAutoErr p a'
                                   NoBlip -> return a'
                          return (Output y (loadFromB' a''))

-- | Like 'loadFromB', except silently ignores errors.  When a load is
-- requested, but there is an IO or parse error, the loading is skipped.
loadFromB' :: MonadIO m => Auto m a (b, Blip FilePath) -> Auto m a b
loadFromB' a = mkAutoM (loadFromB' <$> loadAuto a)
                       (saveAuto a)
                       $ \x -> do
                           Output (y, b) a' <- stepAuto a x
                           a'' <- case b of
                                    Blip p -> liftIO $ readAuto' p a'
                                    NoBlip -> return a'
                           return (Output y (loadFromB' a''))

-- | Takes an 'Auto' and basically "wraps" it so that you can trigger saves
-- with a 'Blip' stream.
--
-- For example, we can take @'sumFrom' 0@:
--
-- @
--     'saveOnB' ('sumFrom' 0) :: 'Auto' 'IO' ('Int', 'Blip' 'FilePath') 'Int'
-- @
--
-- It'll behave just like @'sumFrom' 0@ (with the input you pass in the
-- first field of the tuple)...and whenever the 'Blip' stream (the second
-- field of the input tuple) emits, it'll save the state of @'sumFrom' 0@
-- to disk at the given 'FilePath'.
--
-- Contrast to 'saveFromB', where the 'Auto' itself can trigger saves; in
-- this one, saves are triggered "externally".
--
-- Might be useful in similar situations as 'saveFromB', except if you want
-- to trigger the save externally.
--
saveOnB :: MonadIO m => Auto m a b -> Auto m (a, Blip FilePath) b
saveOnB a = mkAutoM (saveOnB <$> loadAuto a)
                    (saveAuto a)
                    $ \(x, b) -> do
                      case b of
                        Blip p -> liftIO $ writeAuto p a
                        NoBlip -> return ()
                      Output y a' <- stepAuto a x
                      return (Output y (saveOnB a'))

-- | Takes an 'Auto' and basically "wraps" it so that you can trigger
-- loads/resumes from a file with a 'Blip' stream.
--
-- For example, we can take @'sumFrom' 0@:
--
-- @
--     'loadOnB' ('sumFrom' 0) :: 'Auto' 'IO' ('Int', 'Blip' 'FilePath') 'Int'
-- @
--
-- It'll behave just like @'sumFrom' 0@ (with the input you pass in the
-- first field of the tiple)...and whenever the 'Blip' stream (the second
-- field of the input tuple) emits, it'll "reset" and "reload" the
-- @'sumFrom' 0@ from the 'FilePath' on disk.
--
-- Will throw a runtime exception if there is an IO error or a parse error.
--
-- Contrast to 'loadFromB', where the 'Auto' itself can trigger
-- reloads/resets; in this one, the loads are triggered "externally".
--
-- Might be useful in similar situations as 'loadFromB', except if you want
-- to trigger the loading externally.
--
loadOnB :: MonadIO m => Auto m a b -> Auto m (a, Blip FilePath) b
loadOnB a = mkAutoM (loadOnB' <$> loadAuto a)
                    (saveAuto a)
                    $ \(x, b) -> do
                        a' <- case b of
                                Blip p -> liftIO $ readAutoErr p a
                                NoBlip -> return a
                        Output y a'' <- stepAuto a' x
                        return (Output y (loadOnB' a''))

-- | Like 'loadOnB', except silently ignores errors.  When a load is
-- requested, but there is an IO or parse error, the loading is skipped.
loadOnB' :: MonadIO m => Auto m a b -> Auto m (a, Blip FilePath) b
loadOnB' a = mkAutoM (loadOnB' <$> loadAuto a)
                     (saveAuto a)
                     $ \(x, b) -> do
                         a' <- case b of
                                 Blip p -> liftIO $ readAuto' p a
                                 NoBlip -> return a
                         Output y a'' <- stepAuto a' x
                         return (Output y (loadOnB' a''))

