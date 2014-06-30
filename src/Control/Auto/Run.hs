module Control.Auto.Run where

import Control.Auto.Core
import Control.Applicative
import Data.Functor.Identity
import Data.Maybe
import Control.Monad

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . mfilter (null . snd) . listToMaybe . reads

interactIO :: Read a => (b -> IO ()) -> Auto IO a b -> IO (Auto IO a b)
interactIO f a = do
    inp <- readMaybe <$> getLine
    case inp of
      Just x -> do
        Output y a' <- stepAuto a x
        f y
        interactIO f a'
      Nothing ->
        return a

interactIO' :: (Read a, Show b) => Auto IO a b -> IO (Auto IO a b)
interactIO' = interactIO print

interactId :: Read a => (b -> IO ()) -> Auto Identity a b -> IO (Auto Identity a b)
interactId f a = do
    inp <- readMaybe <$> getLine
    case inp of
      Just x -> do
        let Output y a' = runIdentity . stepAuto a $ x
        f y
        interactId f a'
      Nothing ->
        return a

interactId' :: (Read a, Show b) => Auto Identity a b -> IO (Auto Identity a b)
interactId' = interactId print

