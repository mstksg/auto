{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Compat where

import Control.Applicative
import Data.Foldable
import Control.Monad
import Data.Monoid
import Data.Bits
import Data.Traversable

instance Foldable (Either e) where
    foldMap f e = case e of
                    Left _  -> mempty
                    Right x -> f x

    foldr f z e = case e of
                    Left _  -> z
                    Right x -> f x z

    fold e = case e of
               Left _  -> mempty
               Right x -> x

instance Foldable ((,) a) where
    foldMap f (_, x) = f x

    foldr f z (_, x) = f x z

    fold (_, x)      = x

instance Traversable (Either e) where
    traverse f e = case e of
                     Left x  -> pure (Left x)
                     Right x -> Right <$> f x
    sequenceA e = case e of
                    Left x  -> pure (Left x)
                    Right x -> Right <$> x
    mapM f e = case e of
                 Left x  -> return (Left x)
                 Right x -> liftM Right (f x)
    sequence e = case e of
                   Left x  -> return (Left x)
                   Right x -> liftM Right x

instance Traversable ((,) a) where
    traverse f (x, y) = (x,) <$> f y
    sequenceA (x, y)  = (x,) <$> y
    mapM f (x, y)     = liftM (x,) (f y)
    sequence (x, y)   = liftM (x,) y

instance Bits Bool where
    (.&.) = (&&)

    (.|.) = (||)

    xor = (/=)

    complement = not

    shift x 0 = x
    shift _ _ = False

    rotate x _ = x

    bit 0 = True
    bit _ = False

    testBit x 0 = x
    testBit _ _ = False

#if MIN_VERSION_base(4,7,0)
    bitSizeMaybe _ = Just 1
#endif

    bitSize _ = 1

    isSigned _ = False

    popCount False = 0
    popCount True  = 1
