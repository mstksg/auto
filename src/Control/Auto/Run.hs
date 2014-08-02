{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Auto.Run (
  -- * Special 'stepAuto' versions.
    overList
  , overList'
  , stepAutoN
  , stepAutoN'
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
import Text.Read
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Arrow
import Control.Auto.Interval
import Prelude hiding        (interact, mapM)

-- | Turn an 'Auto' that takes a "readable" @a@ and outputs a @b@ into an
-- 'Auto' that takes a 'String' and outputs a @'Maybe' b@.  When the
-- 'String' is successfuly readable as the @a@, it steps the 'Auto' and
-- outputs a succesful 'Just' result; when it isn't, it outputs a 'Nothing'
-- on that step.
--
-- >>> let a0 = duringRead (mkAccum (+) (0 :: Int))
-- >>> let Output y1 a1 = stepAuto' a0 "12"
-- >>> y1
-- Just 12
-- >>> let Output y2 a2 = stepAuto' a1 "orange"
-- >>> y2
-- Nothing
-- >>> let Output y3 _  = stepAuto' a2 "4"
-- >>> y3
-- Just 16
duringRead :: (Monad m, Read a)
           => Auto m a b                -- ^ 'Auto' taking in a readable @a@, outputting @b@
           -> Auto m String (Maybe b)   -- ^ 'Auto' taking in 'String', outputting @'Maybe' b@
duringRead a = during a <<^ readMaybe

-- | Like 'duringRead', but the original 'Auto' would output a @'Maybe' b@
-- instead of a @b@.  Returns 'Nothing' if either the 'String' fails to
-- parse or if the original 'Auto' returned 'Nothing'; returns 'Just' if
-- the 'String' parses and the original 'Auto' returned 'Just'.
bindRead :: (Monad m, Read a)
         => Auto m a (Maybe b)        -- ^ 'Auto' taking in a readable @a@, outputting @'Maybe' b@
         -> Auto m String (Maybe b)   -- ^ 'Auto' taking in 'String', outputting @'Maybe' b@
bindRead a = bindI a <<^ readMaybe

-- | Steps the 'Auto' through every element of the given list as input.
--
-- >>> let a          = mkAccum (+) 0
-- >>> let (ys, a')   = runIdentity (overList a [4,8,-3,5])
-- >>> ys
-- [4, 12, 9, 14]
-- >>> let Output y _ = runIdentity (stepAuto a 7)
-- >>> y
-- 21
overList :: Monad m
         => Auto m a b            -- ^ the 'Auto' to run
         -> [a]                   -- ^ list of inputs to step the 'Auto' with
         -> m ([b], Auto m a b)   -- ^ list of outputs and the updated 'Auto'
overList a []     = return ([], a)
overList a (x:xs) = do
    Output y a' <- stepAuto a  x
    (ys, a'')   <- overList a' xs
    return (y:ys, a'')

-- | Like 'overList', but with an 'Auto'' (the underlying 'Monad' is
-- 'Identity')
--
-- >>> let a          = mkAccum (+) 0
-- >>> let (ys, a')   = overList' a [4,8,-3,5]
-- >>> ys
-- [4, 12, 9, 14]
-- >>> let Output y _ = stepAuto' a 7
-- >>> y
-- 21
overList' :: Auto' a b          -- ^ the 'Auto'' to run
          -> [a]                -- ^ list of inputs to step the 'Auto'' with
          -> ([b], Auto' a b)   -- ^ list of outputs and the updated 'Auto''
overList' a xs = runIdentity (overList a xs)

-- | Repeatedly steps an 'Auto' with the same input a given number of
-- times.
--
-- prop> stepAutoN n a0 x = overList a0 (replicate n x)
--
-- >>> let a          = iterator (*2) 1
-- >>> let (ys, a')   = runIdentity (stepAutoN 8 a ())
-- >>> ys
-- [1, 2, 4, 8, 16, 32, 64, 128]
-- >>> let Output y _ = runIdentity (stepAuto a ())
-- >>> y
-- 256
stepAutoN :: Monad m
          => Int                  -- ^ number of times to step the 'Auto'
          -> Auto m a b           -- ^ the 'Auto' to run
          -> a                    -- ^ the repeated input
          -> m ([b], Auto m a b)  -- ^ list of outputs and the updated 'Auto''
stepAutoN n a0 x = go (max n 0) a0
  where
    go 0 a = return ([], a)
    go i a = do
      Output y a' <- stepAuto a x
      (ys, a'')   <- go (i - 1)  a'
      return (y:ys, a'')

-- | Like 'stepAutoN', but with an 'Auto'' (the underlying 'Monad' is
-- 'Identity')
--
-- >>> let a          = iterator (*2) 1
-- >>> let (ys, a')   = stepAutoN 8 a ()
-- >>> ys
-- [1, 2, 4, 8, 16, 32, 64, 128]
-- >>> let Output y _ = stepAuto a ()
-- >>> y
-- 256
stepAutoN' :: Int -> Auto' a b -> a -> ([b], Auto' a b)
stepAutoN' n a0 x = runIdentity (stepAutoN n a0 x)

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

runStateA :: Monad m
          => Auto (StateT s m) a b
          -> Auto m (a, s) (b, s)
runStateA = undefined

runReaderA :: Monad m
           => Auto (ReaderT r m) a b
           -> Auto m (a, r) b
runReaderA = undefined

runListA :: Auto [] a b
         -> Auto m [a] [b]
runListA = undefined
