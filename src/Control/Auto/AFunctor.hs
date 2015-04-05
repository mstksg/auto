module Control.Auto.AFunctor where

import Control.Applicative
import Control.Arrow
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Auto.Run
import Control.Monad
import Data.Coerce
import Data.Functor.Identity
import Data.Profunctor
import Data.Traversable

-- | Laws:
--
-- 1.  @'afmap' 'id' == 'id'@
-- 2.  @'afmap' g . 'afmap' f == 'afmap' (g . f)@
class AFunctor f where
    afmap :: Monad m => Auto m a b -> Auto m (f a) (f b)

instance AFunctor (Either e) where
    afmap = right

instance AFunctor ((,) w) where
    afmap = second

instance AFunctor Maybe where
    afmap = dimap to from . right
      where
        from = either (const Nothing) Just
        to   = maybe (Left ()) Right

instance AFunctor Blip where
    afmap = dimap to from . right
      where
        to   = blip (Left ()) Right
        from = either (const NoBlip) Blip

instance AFunctor Identity where
    afmap = dimap runIdentity Identity

-- is this lawful?????
-- does this break the second law?
instance AFunctor [] where
    afmap a = mkAutoM (afmap <$> resumeAuto a)
                      (saveAuto a)
                      (liftM (second afmap) . overList a)

instance AFunctor (Const m) where
    afmap _ = arr coerce

instance AFunctor ZipList where
    afmap = dimap getZipList ZipList . afmap

afmapTraversable :: (Traversable t, Monad m)
                 => Auto m a b
                 -> Auto m (t a) (t b)
afmapTraversable a = mkAutoM (afmapTraversable <$> resumeAuto a)
                             (saveAuto a)
                             (liftM (second afmapTraversable) . overTraversable a)
