{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

module Control.Auto.Core (
  -- * Auto output
    Output(..)
  , onOutput
  -- * Auto type & accessors
  , Auto
  , loadAuto
  , saveAuto
  , stepAuto
  , encodeAuto
  , decodeAuto
  -- * Auto constructors
  -- ** Lifting values and functions
  , mkConst
  , mkConstM
  , mkFunc
  , mkFuncM
  -- ** from State transformers
  , mkState
  , mkStateM
  , mkState_
  , mkStateM_
  -- ** from Accumulators
  , mkAccum
  , mkAccumM
  , mkAccum_
  , mkAccumM_
  -- ** Arbitrary Autos
  , mkAuto
  , mkAutoM
  , mkAuto_
  , mkAutoM_
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Data.Serialize
import Data.ByteString
import Data.Monoid
import Data.Profunctor
import Prelude hiding       ((.), id)

data Output m a b = Output { outRes  :: !b
                           , outAuto :: !(Auto m a b)
                           } deriving Functor

instance Monad m => Applicative (Output m a) where
    pure x                      = Output x (pure x)
    Output fx ft <*> Output x t = Output (fx x) (ft <*> t)

onOutput :: (b -> b')
         -> (Auto m a b -> Auto m a' b')
         -> Output m a b -> Output m a' b'
onOutput fx fa (Output x a) = Output (fx x) (fa a)

-- data Auto m a b = Auto { loadAuto :: !(Get (Auto m a b))
--                        , saveAuto :: !Put
--                        , stepAuto :: !(a -> m (Output m a b))
--                        }

data Auto m a b = MkConst  !b
                | MkConstM !(m b)
                | MkFunc   !(a -> b)
                | MkFuncM  !(a -> m b)
                | MkAuto   !(Maybe (AutoS m a b)) !(a -> Output m a b)
                | MkAutoM  !(Maybe (AutoS m a b)) !(a -> m (Output m a b))

data AutoS m a b = AutoS !(Get (Auto m a b)) !Put

loadAuto :: Auto m a b -> Get (Auto m a b)
loadAuto (MkAuto  (Just (AutoS l _)) _) = l
loadAuto (MkAutoM (Just (AutoS l _)) _) = l
loadAuto a                              = pure a

saveAuto :: Auto m a b -> Put
saveAuto (MkAuto  (Just (AutoS _ s)) _) = s
saveAuto (MkAutoM (Just (AutoS _ s)) _) = s
saveAuto _                              = put ()

stepAuto :: Monad m => Auto m a b -> a -> m (Output m a b)
stepAuto a x = case a of
                 MkConst y   ->
                   return (Output y a)
                 MkConstM my -> do
                   y <- my
                   return (Output y a)
                 MkFunc f    ->
                   let y = f x
                   in  return (Output y a)
                 MkFuncM f   -> do
                   y <- f x
                   return (Output y a)
                 MkAuto _ f  ->
                   return (f x)
                 MkAutoM _ f ->
                   f x

encodeAuto :: Auto m a b -> ByteString
encodeAuto = runPut . saveAuto

decodeAuto :: Auto m a b -> ByteString -> Either String (Auto m a b)
decodeAuto a = runGet (loadAuto a)

mkAuto :: Get (Auto m a b)
       -> Put
       -> (a -> Output m a b)
       -> Auto m a b
mkAuto l s = MkAuto (Just (AutoS l s))

mkAutoM :: Get (Auto m a b)
        -> Put
        -> (a -> m (Output m a b))
        -> Auto m a b
mkAutoM l s = MkAutoM (Just (AutoS l s))
{-# INLINE mkAutoM #-}

mkAuto_ :: (a -> Output m a b)
        -> Auto m a b
mkAuto_ = MkAuto Nothing

mkAutoM_ :: (a -> m (Output m a b))
         -> Auto m a b
mkAutoM_ = MkAutoM Nothing

mkConst :: b -> Auto m a b
mkConst = MkConst

mkConstM :: m b -> Auto m a b
mkConstM = MkConstM

mkFunc :: (a -> b)
       -> Auto m a b
mkFunc = MkFunc

mkFuncM :: (a -> m b)
        -> Auto m a b
mkFuncM = MkFuncM

mkState :: Serialize s
        => (a -> s -> (b, s))
        -> s
        -> Auto m a b
mkState f = a_
  where
    a_ s0 = mkAuto (a_ <$> get)
                   (put s0)
                   $ \x -> let (y, s1) = f x s0
                           in  Output y (a_ s1)

mkStateM :: (Serialize s, Monad m)
         => (a -> s -> m (b, s))
         -> s
         -> Auto m a b
mkStateM f = a_
  where
    a_ s0 = mkAutoM (a_ <$> get)
                    (put s0)
                    $ \x -> do
                        (y, s1) <- f x s0
                        return (Output y (mkStateM f s1))

mkState_ :: (a -> s -> (b, s))
         -> s
         -> Auto m a b
mkState_ f = a_
  where
    a_ s0 = mkAuto_ $ \x -> let (y, s1) = f x s0
                        in  Output y (a_ s1)

mkStateM_ :: Monad m
          => (a -> s -> m (b, s))
          -> s
          -> Auto m a b
mkStateM_ f = a_
  where
    a_ s0 = mkAutoM_ $ \x -> do
                         (y, s1) <- f x s0
                         return (Output y (a_ s1))

mkAccum :: Serialize b
        => (b -> a -> b)
        -> b
        -> Auto m a b
mkAccum f = a_
  where
    a_ y0 = mkAuto (a_ <$> get)
                   (put y0)
                   $ \x -> let y1 = f y0 x
                           in  Output y1 (a_ y1)

mkAccumM :: (Serialize b, Monad m)
         => (b -> a -> m b)
         -> b
         -> Auto m a b
mkAccumM f = a_
  where
    a_ y0 = mkAutoM (a_ <$> get)
                    (put y0)
                    $ \x -> do
                        y1 <- f y0 x
                        return (Output y1 (a_ y1))

mkAccum_ :: (b -> a -> b)
         -> b
         -> Auto m a b
mkAccum_ f = a_
  where
    a_ y0 = mkAuto_ $ \x -> let y1 = f y0 x
                            in  Output y1 (a_ y1)

mkAccumM_ :: Monad m
          => (b -> a -> m b)
          -> b
          -> Auto m a b
mkAccumM_ f = a_
  where
    a_ y0 = mkAutoM_ $ \x -> do
                         y1 <- f y0 x
                         return (Output y1 (a_ y1))

instance Monad m => Functor (Auto m a) where
    fmap = rmap

instance Monad m => Applicative (Auto m a) where
    pure      = mkConst
    af <*> ax = mkAutoM ((<*>) <$> loadAuto af <*> loadAuto ax)
                        (saveAuto af *> saveAuto ax)
                        $ \inp -> do
                            outf <- stepAuto af inp
                            outx <- stepAuto ax inp
                            return $ outf <*> outx

instance Monad m => Category (Auto m) where
    id      = mkFunc id
    ag . af = mkAutoM ((.) <$> loadAuto ag <*> loadAuto af)
                      (saveAuto ag *> saveAuto af)
                      $ \x -> do
                          Output y af' <- stepAuto af x
                          Output z ag' <- stepAuto ag y
                          return (Output z (ag' . af'))

instance Monad m => Profunctor (Auto m) where
    lmap f = a_
      where
        a_ a = mkAutoM (a_ <$> loadAuto a)
                       (saveAuto a)
                       $ \x -> do
                           Output y a' <- stepAuto a (f x)
                           return (Output y (a_ a'))
    rmap g = a_
      where
        a_ a = mkAutoM (a_ <$> loadAuto a)
                       (saveAuto a)
                       $ \x -> do
                           Output y a' <- stepAuto a x
                           return (Output (g y) (a_ a'))
    dimap f g = a_
      where
        a_ a = mkAutoM (a_ <$> loadAuto a)
                       (saveAuto a)
                       $ \x -> do
                           Output y a' <- stepAuto a (f x)
                           return (Output (g y) (a_ a'))

instance Monad m => Arrow (Auto m) where
    arr     = mkFunc
    first a = mkAutoM (first <$> loadAuto a)
                      (saveAuto a)
                      $ \(x, y) -> do
                          Output x' a' <- stepAuto a x
                          return (Output (x', y) (first a'))

instance Monad m => ArrowChoice (Auto m) where
    left a = a'
      where
        a' = mkAutoM (left <$> loadAuto a)
                     (saveAuto a)
                     $ \x -> case x of
                         Left y  -> liftM (onOutput Left left) (stepAuto a y)
                         Right y -> return (Output (Right y) a')

instance MonadFix m => ArrowLoop (Auto m) where
    loop a = mkAutoM (loop <$> loadAuto a)
                     (saveAuto a)
                     $ \x -> liftM (onOutput fst loop)
                         . mfix
                         $ \(Output (_, d) _) -> stepAuto a (x, d)

-- Utility instances

instance (Monad m, Monoid b) => Monoid (Auto m a b) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance (Monad m, Num b) => Num (Auto m a b) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    negate      = liftA negate
    abs         = liftA abs
    signum      = liftA signum
    fromInteger = pure . fromInteger

instance (Monad m, Fractional b) => Fractional (Auto m a b) where
    (/)          = liftA2 (/)
    recip        = liftA recip
    fromRational = pure . fromRational

instance (Monad m, Floating b) => Floating (Auto m a b) where
    pi      = pure pi
    exp     = liftA exp
    sqrt    = liftA sqrt
    log     = liftA log
    (**)    = liftA2 (**)
    logBase = liftA2 logBase
    sin     = liftA sin
    tan     = liftA tan
    cos     = liftA cos
    asin    = liftA asin
    atan    = liftA atan
    acos    = liftA acos
    sinh    = liftA sinh
    tanh    = liftA tanh
    cosh    = liftA cosh
    asinh   = liftA asinh
    atanh   = liftA atanh
    acosh   = liftA acosh

-- Monoid, Num, Fractional, and Floating instances for Output because why not.

instance (Monad m, Monoid b) => Monoid (Output m a b) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance (Monad m, Num b) => Num (Output m a b) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    negate      = liftA negate
    abs         = liftA abs
    signum      = liftA signum
    fromInteger = pure . fromInteger

instance (Monad m, Fractional b) => Fractional (Output m a b) where
    (/)          = liftA2 (/)
    recip        = liftA recip
    fromRational = pure . fromRational

instance (Monad m, Floating b) => Floating (Output m a b) where
    pi      = pure pi
    exp     = liftA exp
    sqrt    = liftA sqrt
    log     = liftA log
    (**)    = liftA2 (**)
    logBase = liftA2 logBase
    sin     = liftA sin
    tan     = liftA tan
    cos     = liftA cos
    asin    = liftA asin
    atan    = liftA atan
    acos    = liftA acos
    sinh    = liftA sinh
    tanh    = liftA tanh
    cosh    = liftA cosh
    asinh   = liftA asinh
    atanh   = liftA atanh
    acosh   = liftA acosh

