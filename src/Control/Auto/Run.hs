{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Run (
  -- * "Interactive" running
    interact
  , interactI
  , interactId
  , interactIdI
  , interact'
  , interactI'
  -- * Generic "self-runners"
  -- ** Outside of Monad
  , run
  , runForever
  , runI
  , runForeverI
  -- ** Inside a Monad
  , runM
  , runForeverM
  , runIM
  , runForeverIM
  ) where

import Control.Applicative
import Control.Auto.Core
import Control.Monad hiding  (mapM)
import Data.Functor.Identity
import Data.Maybe
import Data.Traversable
import Prelude hiding        (interact, mapM)

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . mfilter (null . snd) . listToMaybe . reads

runM :: (Monad m, Monad m')      -- ^ The running monad @m@ and the Auto monad @m'@
     => a                        -- ^ Starting input
     -> (b -> m (Maybe a))       -- ^ Handling output and next input in @m@
     -> (forall c. m' c -> m c)  -- ^ Natural transformation from @m'@ to @m@
     -> Auto m' a b              -- ^ Auto in monad @m'@
     -> m (Auto m' a b)          -- ^ Return the resulting/run Auto in @m@
runM x0 f nt a = do
    Output y a' <- nt $ stepAuto a x0
    x1 <- f y
    case x1 of
      Just x  -> runM x f nt a'
      Nothing -> return a'

run :: Monad m
    => a
    -> (b -> Maybe a)
    -> Auto m a b
    -> m (Auto m a b)
run x0 f = runM x0 (return . f) id

runForeverM :: (Monad m, Monad m')
            => a
            -> (b -> m a)
            -> (forall c. m' c -> m c)
            -> Auto m' a b
            -> m r
runForeverM x0 f nt a = runM x0 (liftM Just . f) nt a
                      >> error "runForeverM: reached the end of forever."

runForever :: Monad m
           => a
           -> (b -> a)
           -> Auto m a b
           -> m r
runForever x0 f a = run x0 (Just . f) a
                 >> error "runForever: reached the end of forever."

runIM :: forall m m' a b. (Monad m, Monad m')
      => a
      -> (b -> m (Maybe a))
      -> (forall c. m' c -> m c)
      -> Auto m' a (Maybe b)
      -> m (Auto m' a (Maybe b))
runIM x0 f = runM x0 f'
  where
    f' :: Maybe b -> m (Maybe a)
    f' = liftM join . mapM f

runI :: Monad m
     => a
     -> (b -> Maybe a)
     -> Auto m a (Maybe b)
     -> m (Auto m a (Maybe b))
runI x0 f = runIM x0 (return . f) id

runForeverIM :: (Monad m, Monad m')
             => a
             -> (b -> m a)
             -> (forall c. m' c -> m c)
             -> Auto m' a (Maybe b)
             -> m r
runForeverIM x0 f nt a = runIM x0 (liftM Just . f) nt a
                      >> error "runForeverIM: reached the end of forever."

runForeverI :: Monad m
            => a
            -> (b -> a)
            -> Auto m a (Maybe b)
            -> m r
runForeverI x0 f a = runI x0 (Just . f) a
                  >> error "runForeverI: reached the end of forever."

interact' :: (Monad m, Read a)
          => (b -> IO ())
          -> (forall c. m c -> IO c)
          -> Auto m a b
          -> IO (Auto m a b)
interact' f nt a = do
    x0 <- readMaybe <$> getLine
    case x0 of
      Just x  -> runM x f' nt a
      Nothing -> return a
  where
    f' x = do
      f x
      readMaybe <$> getLine

interactI' :: (Monad m, Read a)
           => (b -> IO ())
           -> (forall c. m c -> IO c)
           -> Auto m a (Maybe b)
           -> IO (Auto m a (Maybe b))
interactI' f nt a = do
    x0 <- readMaybe <$> getLine
    case x0 of
      Just x  -> runIM x f' nt a
      Nothing -> return a
  where
    f' x = do
      f x
      readMaybe <$> getLine

interact :: (Monad m, Read a, Show b)
         => (forall c. m c -> IO c)
         -> Auto m a b
         -> IO (Auto m a b)
interact = interact' print

interactI :: (Monad m, Read a, Show b)
          => (forall c. m c -> IO c)
          -> Auto m a (Maybe b)
          -> IO (Auto m a (Maybe b))
interactI = interactI' print

interactId :: (Read a, Show b)
           => Auto Identity a b
           -> IO (Auto Identity a b)
interactId = interact (return . runIdentity)

interactIdI :: (Read a, Show b)
            => Auto Identity a (Maybe b)
            -> IO (Auto Identity a (Maybe b))
interactIdI = interactI (return . runIdentity)
