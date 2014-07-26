module Control.Auto.Blip (
  -- * The Blip type
    Blip
  , blip
  , merge
  , mergeL
  , mergeR
  , emitOn
  , emitJusts
  , onJusts
  -- * Step/"time" based Blip streams
  , never
  , immediately
  , inB
  , every
  , eachAt
  , eachAt_
  -- * Modifying Blip streams
  , (<&)
  , (&>)
  , once
  , notYet
  , filterB
  , takeB
  , takeWhileB
  , dropB
  , dropWhileB
  , fromBlips
  , tagBlips
  -- * Scanning & Accumulating Blip streams
  , accumB
  , accumB_
  , scanB
  , scanB_
  , mscanB
  , mscanB_
  , countB
  -- * Edge blips
  , onChange
  , onChange_
  , became
  , noLonger
  , onFlip
  , became_
  , noLonger_
  , onFlip_
  , became'
  , noLonger'
  , onFlip'
  -- * Composing with Blip streams
  , perBlip
  , perBlipI
  , bindB
  ) where

import Control.Applicative
import Control.Arrow
import Control.Auto.Blip.Internal
import Control.Auto.Core
import Control.Auto.Interval
import Control.Auto.Time
import Control.Category
import Data.Monoid
import Data.Profunctor
import Data.Serialize
import Prelude hiding                  ((.), id, sequence)
import qualified Control.Auto.Generate as A

infixl 5 <&
infixl 5 &>

mergeL :: Blip a -> Blip a -> Blip a
mergeL = merge const

mergeR :: Blip a -> Blip a -> Blip a
mergeR = merge (flip const)



(<&) :: Monad m => Auto m a (Blip b) -> Auto m a (Blip b) -> Auto m a (Blip b)
(<&) = liftA2 (merge const)

(&>) :: Monad m => Auto m a (Blip b) -> Auto m a (Blip b) -> Auto m a (Blip b)
(&>) = liftA2 (merge (flip const))



never :: Monad m => Auto m a (Blip b)
never = pure NoBlip

immediately :: Monad m => Auto m a (Blip a)
immediately = mkState f False
  where
    f _ True  = (NoBlip, True)
    f x False = (Blip x, True)

inB :: Monad m => Int -> Auto m a (Blip a)
inB n = mkState f (n, False)
  where
    f _ (_, True )             = (NoBlip, (0  , True ))
    f x (i, False) | i <= 0    = (Blip x, (0  , True ))
                   | otherwise = (NoBlip, (i-1, False))

emitOn :: Monad m => (a -> Bool) -> Auto m a (Blip a)
emitOn p = filterB p . every 1

emitJusts :: Monad m => (a -> Maybe b) -> Auto m a (Blip b)
emitJusts p = onJusts <<^ p

every :: Monad m => Int -> Auto m a (Blip a)
every n = stretchB n id

eachAt :: (Monad m, Serialize b) => Int -> [b] -> Auto m a (Blip b)
eachAt n xs = during (every n) . stretch n (A.fromList xs) <|!> never

eachAt_ :: Monad m => Int -> [b] -> Auto m a (Blip b)
eachAt_ n xs = during (every n) . stretch_ n (A.fromList_ xs) <|!> never

filterB :: Monad m => (a -> Bool) -> Auto m (Blip a) (Blip a)
filterB p = arr $ \x -> case x of
                          Blip x' | p x' -> x
                          _               -> NoBlip

once :: Monad m => Auto m (Blip a) (Blip a)
once = mkState f False
  where
    f _          True  = (NoBlip, True )
    f e@(Blip _) False = (e,       True )
    f _          False = (NoBlip, False)

notYet :: Monad m => Auto m (Blip a) (Blip a)
notYet = mkState f False
  where
    f e        True  = (e      , True )
    f (Blip _) False = (NoBlip, True )
    f _        False = (NoBlip, False)


takeB :: Monad m => Int -> Auto m (Blip a) (Blip a)
takeB = mkState f . max 0
  where
    f _ 0          = (NoBlip, 0  )
    f e@(Blip _) i = (e      , i-1)
    f _          i = (NoBlip, i  )

takeWhileB :: Monad m => (a -> Bool) -> Auto m (Blip a) (Blip a)
takeWhileB p = mkState f False
  where
    f _          True        = (NoBlip, True )
    f e@(Blip x) False | p x = (e      , False)
    f _          False       = (NoBlip, True )

dropB :: Monad m => Int -> Auto m (Blip a) (Blip a)
dropB = mkState f . max 0
  where
    f x        0 = (x      , 0  )
    f (Blip _) i = (NoBlip, i-1)
    f _        i = (NoBlip, i  )

dropWhileB :: Monad m => (a -> Bool) -> Auto m (Blip a) (Blip a)
dropWhileB p = mkState f False
  where
    f e          True              = (e      , True )
    f e@(Blip x) False | p x       = (NoBlip, False)
                       | otherwise = (e      , True )
    f _          False             = (NoBlip, False)

accumB :: (Monad m, Serialize b) => (b -> a -> b) -> b -> Auto m (Blip a) (Blip b)
accumB f = mkState (_accumBF f)

accumB_ :: Monad m => (b -> a -> b) -> b -> Auto m (Blip a) (Blip b)
accumB_ f = mkState_ (_accumBF f)

_accumBF :: (b -> a -> b) -> Blip a -> b -> (Blip b, b)
_accumBF f e y0 = case e of
                    Blip x -> let y1 = f y0 x
                              in  (Blip y1, y1)
                    NoBlip ->     (NoBlip , y0)

scanB :: (Monad m, Serialize b) => (b -> a -> b) -> b -> Auto m (Blip a) b
scanB f = mkAccum (_scanBF f)

scanB_ :: Monad m => (b -> a -> b) -> b -> Auto m (Blip a) b
scanB_ f = mkAccum_ (_scanBF f)

_scanBF :: (b -> a -> b) -> b -> Blip a -> b
_scanBF f y0 = blip y0 (f y0)

mscanB :: (Monad m, Monoid a, Serialize a) => Auto m (Blip a) a
mscanB = scanB (<>) mempty

mscanB_ :: (Monad m, Monoid a) => Auto m (Blip a) a
mscanB_ = scanB_ (<>) mempty

countB :: Monad m => Auto m (Blip a) Int
countB = scanB (+) 0 <<^ (1 <$)

became :: (Serialize a, Monad m) => (a -> Bool) -> Auto m a (Blip a)
became p = mkAccum (_becameF p) NoBlip

noLonger :: (Serialize a, Monad m) => (a -> Bool) -> Auto m a (Blip a)
noLonger p = became (not . p)

onFlip :: (Serialize a, Monad m) => (a -> Bool) -> Auto m a (Blip a)
onFlip p = became p &> noLonger p

became_ :: Monad m => (a -> Bool) -> Auto m a (Blip a)
became_ p = mkAccum_ (_becameF p) NoBlip

noLonger_ :: Monad m => (a -> Bool) -> Auto m a (Blip a)
noLonger_ p = became_ (not . p)

onFlip_ :: Monad m => (a -> Bool) -> Auto m a (Blip a)
onFlip_ p = became_ p &> noLonger_ p

_becameF :: (a -> Bool) -> Blip a -> a -> Blip a
_becameF p e x | p x       = blip (Blip x) (const NoBlip) e
               | otherwise = NoBlip

became' :: Monad m => (a -> Bool) -> Auto m a (Blip ())
became' p = mkAccum f NoBlip
  where
    f e x | p x       = blip (Blip ()) (const NoBlip) e
          | otherwise = NoBlip

noLonger' :: Monad m => (a -> Bool) -> Auto m a (Blip ())
noLonger' p = became' (not . p)

onFlip' :: Monad m => (a -> Bool) -> Auto m a (Blip Bool)
onFlip' p = fmap (True <$) (became' p) &> fmap (False <$) (noLonger' p)

onChange :: (Serialize a, Eq a, Monad m) => Auto m a (Blip a)
onChange = mkState _onChangeF Nothing

onChange_ :: (Eq a, Monad m) => Auto m a (Blip a)
onChange_ = mkState_ _onChangeF Nothing

_onChangeF :: Eq a => a -> Maybe a -> (Blip a, Maybe a)
_onChangeF x Nothing               = (NoBlip , Just x)
_onChangeF x (Just x') | x == x'   = (NoBlip , Just x')
                       | otherwise = (Blip x', Just x')

onJusts :: Monad m => Auto m (Maybe a) (Blip a)
onJusts = arr (maybe NoBlip Blip)

fromBlips :: Monad m => a -> Auto m (Blip a) a
fromBlips d = arr (blip d id)

tagBlips :: Monad m => b -> Auto m (Blip a) (Blip b)
tagBlips y = arr (y <$)

perBlip :: Monad m => Auto m a b -> Auto m (Blip a) (Blip b)
perBlip a = a_
  where
    a_ = mkAutoM (perBlip <$> loadAuto a)
                 (saveAuto a)
                 $ \x -> case x of
                           Blip x' -> do
                             Output y a' <- stepAuto a x'
                             return (Output (Blip y) (perBlip a'))
                           NoBlip  ->
                             return (Output NoBlip   a_           )

perBlipI :: Monad m
          => Auto m (Maybe a) (Maybe b)
          -> Auto m (Blip a) (Blip b)
perBlipI = dimap (blip Nothing Just) (maybe NoBlip Blip)

bindB :: Monad m => Auto m a (Blip b) -> Auto m (Blip a) (Blip b)
bindB a = a_
  where
    a_ = mkAutoM (bindB <$> loadAuto a)
                 (saveAuto a)
                 $ \x -> case x of
                           Blip x' -> do
                             Output y a' <- stepAuto a x'
                             return (Output y       (bindB a'))
                           NoBlip  ->
                             return (Output NoBlip a_        )

