-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Control.Auto.State where

import Control.Applicative
import Data.Function ((&))
import Data.Functor.Identity
import Control.Auto.Effects
import Control.Arrow
import Control.Auto.Core
import Control.Category
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Profunctor
import Data.Semigroup
import Data.String
import Data.Typeable
import Prelude hiding            ((.), id)

-- newtype AutoS s m a b = AutoS { runAutoS :: Auto (StateT s m) a b
--                               } deriving ( Functor
--                                          , Applicative
--                                          , Category
--                                          , Arrow
--                                          , ArrowChoice
--                                          , ArrowLoop
--                                          , Profunctor
--                                          , Strong
--                                          , Choice
--                                          , Costrong
--                                          , Alternative
--                                          , IsString
--                                          , Monoid
--                                          , Semigroup
--                                          , Num
--                                          , Fractional
--                                          , Floating
--                                          , Typeable
--                                          )

type AutoS s m a b = Auto (StateT s m) a b

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

set :: Lens s t a b -> b -> s -> t
set l x = runIdentity . l (\_ -> Identity x)

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

view :: Lens s t a b -> s -> a
view l = getConst . l Const

sink :: Monad m => Lens' s b -> Auto m a b -> AutoS s m a b
sink l a = mkAutoM (sink l <$> resumeAuto a)
                   (saveAuto a)
                 $ \x -> do
                     (y, a') <- lift $ stepAuto a x
                     modify $ set l y
                     return (y, sink l a')

pull :: Monad m => Lens' s a -> Auto m a b -> AutoS s m () b
pull l a = mkAutoM (pull l <$> resumeAuto a)
                   (saveAuto a)
                 $ \() -> do
                     x <- gets $ view l
                     (y, a') <- lift $ stepAuto a x
                     return (y, pull l a')

onLens :: Monad m => Lens s s a b -> Auto m a b -> AutoS s m () b
onLens l a = mkAutoM (onLens l <$> resumeAuto a)
                     (saveAuto a)
                   $ \() -> StateT $ \s -> do
                       (y, a') <- stepAuto a (view l s)
                       return ((y, onLens l a'), set l y s)

connect :: Monad m => Lens' s a -> Lens' s b -> Auto m a b -> AutoS s m () b
connect li lo a = mkAutoM (connect li lo <$> resumeAuto a)
                          (saveAuto a)
                       $ \() -> StateT $ \s -> do
                           (y, a') <- stepAuto a (view li s)
                           return ((y, connect li lo a'), set lo y s)

zoomA :: Monad m => Lens' s s' -> AutoS s' m a b -> AutoS s m a b
zoomA l a = mkAutoM (zoomA l <$> resumeAuto a)
                    (saveAuto a)
                  $ \x -> zoom l $ do
                      (y, a') <- stepAuto a x
                      return (y, zoomA l a')

zoom :: Functor m => Lens' s s' -> StateT s' m a -> StateT s m a
zoom l st1 = StateT $ \s -> second (flip (set l) s) <$> runStateT st1 (view l s)

mkStateSM :: (a -> s -> m (b, s)) -> AutoS s m a b
mkStateSM f = arrM $ StateT . f

mkStateS :: Monad m => (a -> s -> (b, s)) -> AutoS s m a b
mkStateS f = arrM $ state . f

hookAuto :: Monad m => Lens s t a b -> Auto m a b -> Auto m q s -> Auto m q t
hookAuto l a0 a1 = mkAutoM (hookAuto l <$> resumeAuto a0 <*> resumeAuto a1)
                           (saveAuto a0 *> saveAuto a1)
                         $ \x -> do
                             (y, a1') <- stepAuto a1 x
                             (z, a0') <- stepAuto a0 (view l y)
                             return (set l z y, hookAuto l a0' a1')

_1 :: Lens (a, c) (b, c) a b
_1 f (x, y) = (,y) <$> f x

_2 :: Lens (c, a) (c, b) a b
_2 f (x, y) = (x,) <$> f y


first' :: Monad m => Auto m a b -> Auto m (a, c) (b, c)
first' a = id
         & hookAuto _1 a

bimap' :: Monad m => Auto m a b -> Auto m c d -> Auto m (a, c) (b, d)
bimap' a1 a2 = id
             & hookAuto _1 a1
             & hookAuto _2 a2

pipeAuto :: Monad m => Lens' s a -> Lens s t q b -> Auto m a b -> Auto m q s -> Auto m q t
pipeAuto l1 l2 a0 a1 = mkAutoM (pipeAuto l1 l2 <$> resumeAuto a0 <*> resumeAuto a1)
                               (saveAuto a0 *> saveAuto a1)
                             $ \x -> do
                                 (y, a1') <- stepAuto a1 x
                                 (z, a0') <- stepAuto a0 (view l1 y)
                                 return (set l2 z y, pipeAuto l1 l2 a0' a1')
