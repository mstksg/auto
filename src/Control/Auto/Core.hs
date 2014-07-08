{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

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

import GHC.Generics
import Control.Applicative
import Control.Arrow
import Data.Typeable
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Data.Serialize
import Data.ByteString
import Data.Monoid
import Data.Profunctor
import Prelude hiding       ((.), id)

data Output m a b = Output { outRes  :: b
                           , outAuto :: !(Auto m a b)
                           } deriving ( Functor
                                      , Typeable
                                      , Generic
                                      )

instance Monad m => Applicative (Output m a) where
    pure x                      = Output x (pure x)
    Output fx ft <*> Output x t = Output (fx x) (ft <*> t)

onOutput :: (b -> b')
         -> (Auto m a b -> Auto m a' b')
         -> Output m a b -> Output m a' b'
onOutput fx fa (Output x a) = Output (fx x) (fa a)

data Auto m a b = Auto { loadAuto :: !(Get (Auto m a b))
                       , saveAuto :: !Put
                       , stepAuto :: !(a -> m (Output m a b))
                       } deriving ( Typeable
                                  , Generic
                                  )

encodeAuto :: Auto m a b -> ByteString
encodeAuto = runPut . saveAuto

decodeAuto :: Auto m a b -> ByteString -> Either String (Auto m a b)
decodeAuto a = runGet (loadAuto a)

mkAuto :: Monad m
       => Get (Auto m a b)
       -> Put
       -> (a -> Output m a b)
       -> Auto m a b
mkAuto l s f = mkAutoM l s (return . f)

mkAutoM :: Get (Auto m a b)
        -> Put
        -> (a -> m (Output m a b))
        -> Auto m a b
mkAutoM = Auto
{-# INLINE mkAutoM #-}

mkAuto_ :: Monad m
        => (a -> Output m a b)
        -> Auto m a b
mkAuto_ f = mkAutoM_ (return . f)

mkAutoM_ :: Monad m
         => (a -> m (Output m a b))
         -> Auto m a b
mkAutoM_ f = a
  where
    a = mkAutoM (pure a) (put ()) f

mkConst :: Monad m => b -> Auto m a b
mkConst = mkFunc . const

mkConstM :: Monad m => m b -> Auto m a b
mkConstM = mkFuncM . const

mkFunc :: Monad m
       => (a -> b)
       -> Auto m a b
mkFunc f = a
  where
    a = mkAuto_ $ \x -> Output (f x) a

mkFuncM :: Monad m
        => (a -> m b)
        -> Auto m a b
mkFuncM f = a
  where
    a = mkAutoM_ $ \x -> do
                      y <- f x
                      return (Output y a)

mkState :: (Serialize s, Monad m)
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

mkState_ :: Monad m
         => (a -> s -> (b, s))
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

mkAccum :: (Serialize b, Monad m)
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

mkAccum_ :: Monad m
         => (b -> a -> b)
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
    pure                         = mkConst
    Auto fl fs ft <*> Auto l s t = mkAutoM ((<*>) <$> fl <*> l)
                                           (fs *> s)
                                           $ \x -> liftM2 (<*>) (ft x) (t x)

instance Monad m => Category (Auto m) where
    id = mkFunc id
    Auto gl gs gt . Auto fl fs ft = mkAutoM ((.) <$> gl <*> fl)
                                            (gs *> fs)
                                            $ \x -> do
                                                Output y fa' <- ft x
                                                Output z ga' <- gt y
                                                return (Output z (ga' . fa'))

instance Monad m => Profunctor (Auto m) where
    lmap f = a_
      where
        a_ (Auto l s t) = mkAutoM (a_ <$> l)
                                  s
                                  $ \x -> do
                                      Output y a' <- t (f x)
                                      return (Output y (a_ a'))
    rmap g = a_
      where
        a_ (Auto l s t) = mkAutoM (a_ <$> l)
                                  s
                                  $ \x -> do
                                      Output y a' <- t x
                                      return (Output (g y) (a_ a'))
    dimap f g = a_
      where
        a_ (Auto l s t) = mkAutoM (a_ <$> l)
                                  s
                                  $ \x -> do
                                      Output y a' <- t (f x)
                                      return (Output (g y) (a_ a'))

instance Monad m => Arrow (Auto m) where
    arr                = mkFunc
    first (Auto l s t) = mkAutoM (first <$> l)
                                 s
                                 $ \(x, y) -> do
                                     Output x' a' <- t x
                                     return (Output (x', y) (first a'))

instance Monad m => ArrowChoice (Auto m) where
    left (Auto l s t) = a
      where
        a = mkAutoM (left <$> l)
                    s
                    $ \x -> case x of
                        Left y  -> liftM (onOutput Left left) (t y)
                        Right y -> return (Output (Right y) a)

instance MonadFix m => ArrowLoop (Auto m) where
    loop (Auto l s t) = mkAutoM (loop <$> l)
                                s
                                $ \x ->
                                    -- Output (y, _) a' <- mfix (\(Output (_, d) _) -> t (x, d))
                                    -- return (Output y (loop a'))
                                  liftM (onOutput fst loop)
                                      . mfix
                                      $ \ ~(Output (_, d) _) -> t (x, d)
    -- loop (Auto l s t) = mkAutoM (loop <$> l)
    --                             s
    --                             $ \x -> liftM (onOutput fst loop)

-- instance (MonadFix m) => ArrowLoop (Wire s e m) where
--     loop w' =
--         WGen $ \ds mx' ->
--             liftM (fmap fst ***! loop) .
--             mfix $ \ ~(mx, _) ->
--                 let d | Right (_, d) <- mx = d
--                       | otherwise = error "Feedback broken by inhibition"
--                 in stepWire w' ds (fmap (, d) mx')

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

