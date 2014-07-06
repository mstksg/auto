{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Serialize (
    readAuto
  , readAuto'
  , writeAuto
  , saving
  , loading
  , serializing
  ) where

import Control.Monad.IO.Class
import Control.Exception
import Control.Applicative
import Control.Auto.Core
import qualified Data.ByteString as B

readAuto :: FilePath -> Auto m a b -> IO (Either String (Auto m a b))
readAuto fp a = decodeAuto a <$> B.readFile fp

readAuto' :: forall a m b. FilePath -> Auto m a b -> IO (Auto m a b)
readAuto' fp a = do
    esa <- try (readAuto fp a) :: IO (Either SomeException (Either String (Auto m a b)))
    case esa of
      Right (Right a') -> return a'
      _                -> return a

writeAuto :: FilePath -> Auto m a b -> IO ()
writeAuto fp a = B.writeFile fp (encodeAuto a)

saving :: MonadIO m => FilePath -> Auto m a b -> Auto m a b
saving fp a = mkAutoM (saving fp <$> loadAuto a)
                      (saveAuto a)
                      $ \x -> do
                          Output y a' <- stepAuto a x
                          liftIO $ writeAuto fp a'
                          return (Output y (saving fp a'))

loading :: MonadIO m => FilePath -> Auto m a b -> Auto m a b
loading fp a0 = mkAutoM (loading fp <$> loadAuto a0)
                        (saveAuto a0)
                        $ \x -> do
                            a           <- liftIO $ readAuto' fp a0
                            Output y a' <- stepAuto a x
                            return (Output y (loaded a'))
  where
    loaded a = mkAutoM (loading fp <$> loadAuto a)
                       (saveAuto a)
                       $ \x -> do
                           Output y a' <- stepAuto a x
                           return (Output y (loaded a'))

serializing :: MonadIO m => FilePath -> Auto m a b -> Auto m a b
serializing fp a = loading fp (saving fp a)
