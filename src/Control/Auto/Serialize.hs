{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Serialize (
    readAuto
  , readAuto'
  , readAutoEither
  , writeAuto
  , saving
  , loading'
  , serializing
  , saveFromB
  , loadFromB'
  , saveOnB
  , loadOnB'
  ) where

import Control.Auto.Blip.Internal
import Control.Monad.IO.Class
import Control.Monad
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

readAutoEither :: Monad m => FilePath -> Auto m a b -> IO (Auto m a (Either String b))
readAutoEither fp a = do
    esa <- try (readAuto fp a)
    return $ case esa of
               Left (e :: SomeException) -> pure (Left (show e))
               Right (Left e)            -> pure (Left e)
               Right (Right ra)          -> Right <$> ra


writeAuto :: FilePath -> Auto m a b -> IO ()
writeAuto fp a = B.writeFile fp (encodeAuto a)

saving :: MonadIO m => FilePath -> Auto m a b -> Auto m a b
saving fp a = mkAutoM (saving fp <$> loadAuto a)
                      (saveAuto a)
                      $ \x -> do
                          Output y a' <- stepAuto a x
                          liftIO $ writeAuto fp a'
                          return (Output y (saving fp a'))


loading' :: MonadIO m => FilePath -> Auto m a b -> Auto m a b
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

-- loading :: MonadIO m => FilePath -> Auto m a b -> Auto m a (Either String b)
-- loading fp a0 = mkAutoM (loading fp <$> loadAuto a0)
--                         (saveAuto a0)
--                         $ \x -> do
--                             a <- liftIO (readAutoEither fp a0)
--                             stepAuto a x
--   where
--     loaded a = mkAutoM (loading fp <$> loadAuto a)
--                        (saveAuto a)
--                        $ \x -> do
--                            Output y a' <- stepAuto a x
--                            return (Output y (loaded a'))

serializing :: MonadIO m => FilePath -> Auto m a b -> Auto m a b
serializing fp a = loading' fp (saving fp a)

saveFromB :: MonadIO m => Auto m a (b, Blip FilePath) -> Auto m a b
saveFromB a = mkAutoM (saveFromB <$> loadAuto a)
                      (saveAuto a)
                      $ \x -> do
                          Output (y, b) a' <- stepAuto a x
                          case b of
                            Blip p -> liftIO $ writeAuto p a'
                            NoBlip -> return ()
                          return (Output y (saveFromB a'))

loadFromB' :: MonadIO m => Auto m a (b, Blip FilePath) -> Auto m a b
loadFromB' a = mkAutoM (loadFromB' <$> loadAuto a)
                       (saveAuto a)
                       $ \x -> do
                           Output (y, b) a' <- stepAuto a x
                           a'' <- case b of
                                    Blip p -> liftIO $ readAuto' p a'
                                    NoBlip -> return a'
                           return (Output y (loadFromB' a''))


saveOnB :: MonadIO m => Auto m a b -> Auto m (a, Blip FilePath) b
saveOnB a = mkAutoM (saveOnB <$> loadAuto a)
                    (saveAuto a)
                    $ \(x, b) -> do
                      case b of
                        Blip p -> liftIO $ writeAuto p a
                        NoBlip -> return ()
                      Output y a' <- stepAuto a x
                      return (Output y (saveOnB a'))

loadOnB' :: MonadIO m => Auto m a b -> Auto m (a, Blip FilePath) b
loadOnB' a = mkAutoM (loadOnB' <$> loadAuto a)
                     (saveAuto a)
                     $ \(x, b) -> do
                         a' <- case b of
                                 Blip p -> liftIO $ readAuto' p a
                                 NoBlip -> return a
                         Output y a'' <- stepAuto a' x
                         return (Output y (loadOnB' a''))
