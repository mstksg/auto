{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Run (
  -- * Special 'stepAuto' versions.
    overList
  , stepAutoN
  -- * Running "interactively"
  , interact
  , interactId
  , interact'
  -- ** Helpers
  , duringRead
  , bindRead
  -- * Generic "self-runners"
  , run
  , runM
  ) where

import Control.Applicative
import Control.Auto.Core
import Control.Monad hiding  (mapM)
import Data.Functor.Identity
import Data.Maybe
import Control.Arrow
import Control.Auto.Interval
import Prelude hiding        (interact, mapM)

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . mfilter (null . snd) . listToMaybe . reads

duringRead :: (Monad m, Read a)
           => Auto m a b
           -> Auto m String (Maybe b)
duringRead a = during a <<^ readMaybe

bindRead :: (Monad m, Read a)
         => Auto m (Maybe a) (Maybe b)
         -> Auto m String (Maybe b)
bindRead a = bindI a <<^ readMaybe

overList :: Monad m => Auto m a b -> [a] -> m ([b], Auto m a b)
overList a []     = return ([], a)
overList a (x:xs) = do
    Output y a' <- stepAuto a  x
    (ys, a'')   <- overList a' xs
    return (y:ys, a'')

stepAutoN :: Monad m => Int -> Auto m a b -> a -> m ([b], Auto m a b)
stepAutoN n a0 x = go (max n 0) a0
  where
    go 0 a = return ([], a)
    go i a = do
      Output y a' <- stepAuto a x
      (ys, a'')   <- go (i - 1)  a'
      return (y:ys, a'')


runM :: (Monad m, Monad m')
     => a                        -- ^ Starting input
     -> (b -> m (Maybe a))       -- ^ Handling output and next input in @m@
     -> (forall c. m' c -> m c)  -- ^ Natural transformation from @m'@ (the Auto monad) to @m@ (the running monad)
     -> Auto m' a (Maybe b)      -- ^ Auto in monad @m'@
     -> m (Auto m' a (Maybe b))  -- ^ Return the resulting/run Auto in @m@
runM x0 f nt a = do
    Output my a' <- nt $ stepAuto a x0
    case my of
      Just y  -> do
        x1 <- f y
        case x1 of
          Just x  -> runM x f nt a'
          Nothing -> return a'
      Nothing ->
        return a'

run :: Monad m
    => a
    -> (b -> Maybe a)
    -> Auto m a (Maybe b)
    -> m (Auto m a (Maybe b))
run x0 f = runM x0 (return . f) id


interact' :: Monad m
          => (b -> IO ())
          -> (forall c. m c -> IO c)
          -> Auto m String (Maybe b)
          -> IO (Auto m String (Maybe b))
interact' f nt a = do
    x <- getLine
    runM x f' nt a
  where
    f' y = do
      f y
      Just <$> getLine

interact :: Monad m
         => (forall c. m c -> IO c)
         -> Auto m String (Maybe String)
         -> IO (Auto m String (Maybe String))
interact = interact' putStrLn

interactId :: Auto Identity String (Maybe String)
           -> IO (Auto Identity String (Maybe String))
interactId = interact (return . runIdentity)
