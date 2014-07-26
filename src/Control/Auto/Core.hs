{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Control.Auto.Core (
  -- * Auto
  -- ** Type
    Auto
  , Auto'
  -- ** Running
  , stepAuto
  , stepAuto'
  -- ** Serializing
  -- $serializing
  , encodeAuto
  , decodeAuto
  , saveAuto
  , loadAuto
  -- * Auto output
  , Output(..)
  , Output'
  , onOutput
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
  -- * Special Autos
  , forcer
  , seqer
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.DeepSeq
import Control.Monad
import Control.Monad.Fix
import Data.ByteString
import Data.Functor.Identity
import Data.Profunctor
import Data.Semigroup
import Data.Serialize
import Data.Typeable
import GHC.Generics
import Prelude hiding        ((.), id)

-- | The output of a 'stepAuto'.  Contains the "result" value of the
-- stepping ('outRes'), and the "next 'Auto'", 'outAuto'.
--
-- An @'Auto' m a b@ will produce an @'Output' m a b@; when "stepped" with
-- an @a@, the "result" ('outRes') is a @b@.
--
-- Really, you can just think of this as a fancy tuple.
data Output m a b = Output { outRes  :: b             -- ^ Result value of a step
                           , outAuto :: Auto m a b    -- ^ The next 'Auto'
                           } deriving ( Functor
                                      , Typeable
                                      , Generic
                                      )

instance Monad m => Applicative (Output m a) where
    pure x                      = Output x (pure x)
    Output fx ft <*> Output x t = Output (fx x) (ft <*> t)

-- | Map two functions onto both fields of an 'Output'.
--
-- If you want to map an @a -> b@ onto both fields (the result and the
-- result of the next Auto), you can use the 'Functor' instance instead.
onOutput :: (b -> b')
         -> (Auto m a b -> Auto m a' b')
         -> Output m a b -> Output m a' b'
onOutput fx fa (Output x a) = Output (fx x) (fa a)

-- | The Auto type.  Basically represents a function containing its own
-- localized internal state.  If you have an @'Auto' a b@, you can "step"
-- it with 'stepAuto' and an @a@, to get a @b@ and a "next 'Auto'".  The
-- @a@ is the input, and the @b@ is the output, and the next 'Auto' is the
-- 'Auto' with updated internal state.
--
-- The "stepping" process can be monadic:
--
-- > stepAuto :: Auto m a b -> a -> m (Output m a b)
--
-- So you can have access to, say, a shared environment using 'Reader' or
-- something like that.
--
-- 'Auto' is mostly useful because of its 'Functor', 'Applicative',
-- 'Category', and 'Arrow' (and Arrow-related) instances.  These allow you
-- to modify, combine, chain, and side-chain Autos in expressive way,
-- allowing you to build up complex ones from combinations of simple,
-- primitive ones.
--
-- TODO: see tutorial
--
-- The 'Auto' also contains information on its own serialization, so you
-- can serialize and re-load the internal state without actually accessing
-- it.
data Auto m a b = Auto { _loadAuto :: !(Get (Auto m a b))
                       , _saveAuto :: !Put
                       , _stepAuto :: !(a -> m (Output m a b))
                       } deriving ( Typeable
                                  , Generic
                                  )

-- | Special case of 'Auto' where the underlying 'Monad' is 'Identity'.
type Auto'   = Auto Identity

-- | Special case of 'Output' where the underlying 'Monad' of 'outAuto' is
-- 'Identity'.
type Output' = Output Identity

-- $serializing
-- The 'Auto' type offers an interface in which you can serialize
-- ("freeze") and "resume" an Auto, in 'ByteString' (binary) form.
--
-- You can "freeze" any 'Auto' into a 'ByteString' using 'encodeAuto' (or,
-- if you want the raw 'Put' (from "Data.Serialize") for some reason,
-- there's 'saveAuto'.
--
-- You can "resume" any 'Auto' from a 'ByteString' using 'decodeAuto' (or,
-- if you want the raw 'Get' for some reason, there's 'loadAuto').
--
-- Note 'decodeAuto' and 'loadAuto' "resume" a /given 'Auto'/.  That is, if
-- you call 'decodeAuto' on a "fresh 'Auto'", it'll decode a 'ByteString'
-- into /that 'Auto', but "resumed"/.  That is, it'll "fast forward" that
-- 'Auto' into the state it was when it was saved.
--
-- For example, let's say I have @a = 'mkAccum' (+) 0@, the 'Auto' that
-- returns the sum of everything it has received so far.  If I feed it
-- 3 and 10, it'll have its internal accumulator as 13, keeping track of
-- all the numbers it has seen so far.
--
-- >>> let a             = mkAccum (+) 0
-- >>> let Output _ a'   = stepAuto' a  3
-- >>> let Output _ a''  = stepAuto' a' 10
--
-- I can then use 'encodeAuto' to "freeze"/"save" the 'Auto' into the
-- 'ByteString' @bs@:
--
-- >>> let bs            = encodeAuto a''
--
-- To /resume//"load" it, I can use 'decodeAuto' to /resume/ the original
-- __@a@__.  Remember, 'a' was our original 'Auto', the summer 'Auto' with
-- a starting accumulator of 0.  We use 'decodeAuto' to "resume" it, with
-- and resume it with its internal accumulator at 13.
--
-- >>> let Right resumed = decodeAuto a bs
-- >>> let Output y _    = stepAuto' resumed 0
-- 13
--
-- Note that all of these would have had the same result:
--
-- >>> let Right resumed = decodeAuto a'  bs
-- >>> let Right resumed = decodeAuto a'' bs
-- >>> let Right resumed = decodeAuto (mkAccum (+) 0) bs
--
-- One way you can think about it is that 'loadAuto'/'decodeAuto' takes an
-- 'Auto' and creates a "blueprint" from that 'Auto', on how to "load it";
-- the blueprint contains what the form of the internal state is, and their
-- offets in the 'ByteString'.  So in the above, 'a', 'a'', 'a''', and
-- 'mkAccum (+) 0' all have the same "blueprint" --- their internal states
-- are of the same structure.
--
-- Some specific 'Auto's (indicated by a naming convention) might choose to
-- have internal state, yet ignore it when saving/loading.  So, saving it
-- actaully saves no state, and "resuming" it really doesn't do anything.
-- That is, @'decodeAuto' a_ bs = Right a_@.  There isn't a real way to
-- identify from the type of the 'Auto' if it will properly save/resume or
-- not, so you have to keep track of this yourself.  In all of the 'Auto'
-- "included" in this library, any 'Auto' whose name /does not/ end in @_@
-- /will serialize and resume/.  An 'Auto' whose name ends in @_@ is taken
-- by naming convention to be a non-resuming 'Auto'.
--
-- In your own compositions, if you are sure to always use resuming
-- 'Auto's, your composition will also be properly resuming...so you don't
-- have to worry about this!  You shouldn't really ever be "surprised",
-- because you'll always explicitly chose the resuming version for 'Auto's
-- you want to resume, and the non-resuming version for those you don't.
--
-- TODO: replace "decode" with "resume".

-- | Encode an 'Auto' and its internal state into a 'ByteString'.
encodeAuto :: Auto m a b -> ByteString
encodeAuto = runPut . saveAuto

-- | "Resume" an 'Auto' from its 'ByteString' serialization, giving
-- a 'Left' if the deserialization is not possible.
decodeAuto :: Auto m a b -> ByteString -> Either String (Auto m a b)
decodeAuto = runGet . loadAuto

-- | Returns a 'Get' from an 'Auto' ---  instructions (from
-- "Data.Serialize") on taking a ByteString and "restoring" the originally
-- saved 'Auto', in the originally saved state.
loadAuto :: Auto m a b -> Get (Auto m a b)
loadAuto = _loadAuto

-- | Returns a 'Put' --- instructions (from "Data.Serialize") on how to
-- "freeze" the 'Auto', with its internal state, and save it to a binary
-- encoding.  It can later be reloaded and "resumed" by
-- 'loadAuto'/'decodeAuto'.
saveAuto :: Auto m a b -> Put
saveAuto = _saveAuto


-- | "Runs" the 'Auto' through one step.
--
-- Remember that at every step for an @'Auto' m a b@, you provide an @a@
-- input and receive a @b@ output with an "updated"/"next" 'Auto'.
--
-- >>> let a = mkAccum (+) 0 :: Auto Identity Int Int
--             -- an Auto that sums all of its input.
-- >>> let Identity (Output y a') = stepAuto a 3
-- >>> y      -- the result
-- 3
-- :: Int
-- >> :t a'   -- the updated 'Auto'
-- a' :: Auto Identity Int Int
--
-- ('Identity', from "Data.Functor.Identity", is the "dumb Functor": @data
-- 'Identity' a = 'Identity' a@)
--
-- If you think of an @'Auto' m a b@ as a "stateful function" of type @a ->
-- m b@, then 'stepAuto' lets you "run" it.
stepAuto :: Auto m a b        -- ^ the 'Auto' to step
         -> a                 -- ^ the input
         -> m (Output m a b)  -- ^ the output, and the updated 'Auto''.
stepAuto = _stepAuto

-- | 'stepAuto', but for an 'Auto'' --- the underlying 'Monad' is
-- 'Identity'.
stepAuto' :: Auto' a b        -- ^ the 'Auto'' to step
          -> a                -- ^ the input
          -> Output' a b      -- ^ the output, and the updated 'Auto''
stepAuto' a = runIdentity . stepAuto a

-- | A special 'Auto' that acts like the 'id' 'Auto', but forces results as
-- they come through to be fully evaluated, when composed with other
-- 'Auto's.
forcer :: (NFData a, Monad m) => Auto m a a
forcer = mkAuto_ $ \x -> x `deepseq` Output x forcer

-- | A special 'Auto' that acts like the 'id' 'Auto', but forces results as
-- they come through to be evaluated to Weak Head Normal Form, with 'seq',
-- when composed with other 'Auto's.
seqer :: Monad m => Auto m a a
seqer = mkAuto_ $ \x -> x `seq` Output x seqer

-- | Construct an 'Auto' by explicity giving its serialization,
-- deserialization, and the (pure) function from @a@ to @b@ and the "next
-- 'Auto'".
--
-- Ideally, you wouldn't have to use this unless you are making your own
-- framework.  Try your best to make what you want by assembling
-- primtives together.
mkAuto :: Monad m
       => Get (Auto m a b)      -- ^ resuming/loading 'Get'
       -> Put                   -- ^ saving 'Put'
       -> (a -> Output m a b)   -- ^ step function
       -> Auto m a b
mkAuto l s f = mkAutoM l s (return . f)

-- | Construct an 'Auto' by explicitly giving its serializiation,
-- deserialization, and the (monadic) function from @a@ to @b@ and the
-- "next 'Auto'".
--
-- Ideally, you wouldn't have to use this unless you are making your own
-- framework.  Try your best to make what you want by assembling
-- primtives together.
mkAutoM :: Get (Auto m a b)         -- ^ resuming/loading 'Get'
        -> Put                      -- ^ saving 'Put'
        -> (a -> m (Output m a b))  -- ^ (monadic) step function
        -> Auto m a b
mkAutoM = Auto
{-# INLINE mkAutoM #-}

-- | Like 'mkAuto', but without any way of meaningful serializing or
-- deserializing.
--
-- Be careful!  This 'Auto' can still carry arbitrary internal state, but
-- it cannot be meaningfully serialized or re-loaded/resumed.  You can
-- still pretend to do so using
-- 'loadAuto'/'saveAuto'/'encodeAuto'/'decodeAuto' (and the type system
-- won't stop you), but when you try to "resume"/decode it, its state will
-- be lost.
mkAuto_ :: Monad m
        => (a -> Output m a b)      -- ^ step function
        -> Auto m a b
mkAuto_ f = mkAutoM_ (return . f)

-- | Like 'mkAutoM', but without any way of meaningful serializing or
-- deserializing.
--
-- Be careful!  This 'Auto' can still carry arbitrary internal state, but
-- it cannot be meaningfully serialized or re-loaded/resumed.  You can
-- still pretend to do so using
-- 'loadAuto'/'saveAuto'/'encodeAuto'/'decodeAuto' (and the type system
-- won't stop you), but when you try to "resume"/decode it, its state will
-- be reset.
mkAutoM_ :: Monad m
         => (a -> m (Output m a b))   -- ^ (monadic) step function
         -> Auto m a b
mkAutoM_ f = a
  where
    a = mkAutoM (pure a) (put ()) f

-- | Construct the 'Auto' that always yields the given value, ignoring its
-- input.
--
-- You really shouldn't ever need this; you should be using 'pure' from the
-- 'Applicative' instance, from the "Control.Applicative" module.
mkConst :: Monad m
        => b            -- ^ constant value to be outputted
        -> Auto m a b
mkConst = mkFunc . const

-- | Construct the 'Auto' that alyways "executs" the given monadic value at
-- every step, yielding the result and ignoring its input.
--
-- Only really provided here for consistency with the rest of this module's
-- API.  You should really be using 'effect' from the
-- "Control.Auto.Effects" module.
mkConstM :: Monad m
         => m b           -- ^ monadic action to be executed at every step
         -> Auto m a b
mkConstM = mkFuncM . const

-- | Construct a stateless 'Auto' that simply applies the given (pure)
-- function to every input, yielding the output.
--
-- This is rarely needed; you should be using 'arr' from the 'Arrow'
-- instance, from "Control.Arrow".
mkFunc :: Monad m
       => (a -> b)        -- ^ pure function
       -> Auto m a b
mkFunc f = a
  where
    a = mkAuto_ $ \x -> Output (f x) a

-- | Construct a statelss 'Auto' that simply applies and executes the givne
-- (monadic) function to every input, yielding the output.
--
-- It's recommended that you use 'arrM' from "Control.Auto.Effects".  This
-- is only really provided for consistency.
mkFuncM :: Monad m
        => (a -> m b)     -- ^ "monadic" function
        -> Auto m a b
mkFuncM f = a
  where
    a = mkAutoM_ $ \x -> do
                      y <- f x
                      return (Output y a)

-- | Construct an 'Auto' from a state transformer: an @a -> s -> (b, s)@
-- gives you an @'Auto' m a b@, for any 'Monad' @m@.  At every step, it
-- takes in the @a@ input, runs the function with the stored internal
-- state, returns the @b@ result, and now contains the new resulting state.
-- You have to intialize it with an initial state, of course.
--
-- Try not to use this if it's ever avoidable, unless you're a framework
-- developer or something.  Try make something by combining/composing the
-- various 'Auto' combinators.
--
-- This version is a wrapper around 'mkAuto', that keeps track of the
-- serialization and re-loading of the internal state for you, so you don't
-- have to deal with it explicitly.
--
-- If your state @s@ does not have a 'Serialize' instance, then you should
-- either write a meaningful one, or throw away serializability and use
-- 'mkState_'.
mkState :: (Serialize s, Monad m)
        => (a -> s -> (b, s))       -- ^ state transformer
        -> s                        -- ^ intial state
        -> Auto m a b
mkState f = a_
  where
    a_ s0 = mkAuto (a_ <$> get)
                   (put s0)
                   $ \x -> let (y, s1) = f x s0
                           in  Output y (a_ s1)

-- | Construct an 'Auto' from a "monadic" state transformer: @a -> s ->
-- m (b, s)@ gives you an @'Auto' m a b@.  At every step, it takes in the
-- @a@ input, runs the function with the stored internal state and
-- "executes" the @m (b, s)@ to get the @b@ output, and stores the @s@ as
-- the new, updated state.  Must be initialized with an initial state.
--
-- Try not to use this if it's ever avoidable, unless you're a framework
-- developer or something.  Try make something by combining/composing the
-- various 'Auto' combinators.
--
-- This version is a wrapper around 'mkAuto', that keeps track of the
-- serialization and re-loading of the internal state for you, so you don't
-- have to deal with it explicitly.
--
-- If your state @s@ does not have a 'Serialize' instance, then you should
-- either write a meaningful one, or throw away serializability and use
-- 'mkStateM_'.
mkStateM :: (Serialize s, Monad m)
         => (a -> s -> m (b, s))      -- ^ (monadic) state transformer
         -> s                         -- ^ initial state
         -> Auto m a b
mkStateM f = a_
  where
    a_ s0 = mkAutoM (a_ <$> get)
                    (put s0)
                    $ \x -> do
                        (y, s1) <- f x s0
                        return (Output y (mkStateM f s1))

-- | A version of 'mkState', where the internal state isn't serialized.  It
-- can be "saved" and "loaded", but the state is lost in the process.
--
-- Useful if your state @s@ cannot have a meaningful 'Serialize' instance.
mkState_ :: Monad m
         => (a -> s -> (b, s))    -- ^ state transformer
         -> s                     -- ^ initial state
         -> Auto m a b
mkState_ f = a_
  where
    a_ s0 = mkAuto_ $ \x -> let (y, s1) = f x s0
                        in  Output y (a_ s1)

-- | A version of 'mkStateM', where the internal state isn't serialized.
-- It can be "saved" and "loaded", but the state is lost in the process.
--
-- Useful if your state @s@ cannot have a meaningful 'Serialize' instance.
mkStateM_ :: Monad m
          => (a -> s -> m (b, s))   -- ^ (monadic) state transformer
          -> s                      -- ^ initial state
          -> Auto m a b
mkStateM_ f = a_
  where
    a_ s0 = mkAutoM_ $ \x -> do
                         (y, s1) <- f x s0
                         return (Output y (a_ s1))

-- | Construct an 'Auto' from a "folding" function: @b -> a -> b@ yields an
-- @'Auto' m a b@.  Basically acts like a 'foldl' or a 'scanl'.  There is
-- an internal accumulator that is "updated" with an @a@ at every step.
-- Must be given an initial accumulator.
--
-- Example: an 'Auto' that sums up all of its input.
--
-- >>> let summer = mkAccum (+) 0
-- >>> let Output sum1 summer' = stepAuto summer 3
-- >>> sum1
-- 3
-- >>> let Output sum2 _       = stepAuto summer'' 10
-- >>> sum2
-- 13
--
-- If your accumulator @b@ does not have a 'Serialize' instance, then you
-- should either write a meaningful one, or throw away serializability and
-- use 'mkAccum_'.
mkAccum :: (Serialize b, Monad m)
        => (b -> a -> b)      -- ^ accumulating function
        -> b                  -- ^ initial accumulator
        -> Auto m a b
mkAccum f = a_
  where
    a_ y0 = mkAuto (a_ <$> get)
                   (put y0)
                   $ \x -> let y1 = f y0 x
                           in  Output y1 (a_ y1)

-- | Construct an 'Auto' from a "monadic" "folding" function: @b -> a ->
-- m b@ yields an @'Auto' m a b@.  Basically acts like a 'foldM' or 'scanM'
-- (if it existed).  here is an internal accumulator that is "updated" with
-- an input @a@ with the result of the executed @m b@ at every step.  Must
-- be given an initial accumulator.
--
-- If your accumulator @b@ does not have a 'Serialize' instance, then you
-- should either write a meaningful one, or throw away serializability and
-- use 'mkAccumM_'.
mkAccumM :: (Serialize b, Monad m)
         => (b -> a -> m b)       -- ^ (monadic) accumulating function
         -> b                     -- ^ initial accumulator
         -> Auto m a b
mkAccumM f = a_
  where
    a_ y0 = mkAutoM (a_ <$> get)
                    (put y0)
                    $ \x -> do
                        y1 <- f y0 x
                        return (Output y1 (a_ y1))

-- | A version of 'mkAccum_, where the internal accumulator isn't
-- serialized. It can be "saved" and "loaded", but the state is lost in the
-- process.
--
-- Useful if your accumulator @b@ cannot have a meaningful 'Serialize'
-- instance.
mkAccum_ :: Monad m
         => (b -> a -> b)   -- ^ accumulating function
         -> b               -- ^ intial accumulator
         -> Auto m a b
mkAccum_ f = a_
  where
    a_ y0 = mkAuto_ $ \x -> let y1 = f y0 x
                            in  Output y1 (a_ y1)

-- | A version of 'mkAccumM_, where the internal accumulator isn't
-- serialized. It can be "saved" and "loaded", but the state is lost in the
-- process.
--
-- Useful if your accumulator @b@ cannot have a meaningful 'Serialize'
-- instance.
mkAccumM_ :: Monad m
          => (b -> a -> m b)    -- ^ (monadic) accumulating function
          -> b                  -- ^ initial accumulator
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
                        $ \x -> liftM2 (<*>) (stepAuto af x) (stepAuto ax x)

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
    left a0 = a
      where
        a = mkAutoM (left <$> loadAuto a0)
                    (saveAuto a0)
                    $ \x -> case x of
                        Left y  -> liftM (onOutput Left left) (stepAuto a0 y)
                        Right y -> return (Output (Right y) a)

instance MonadFix m => ArrowLoop (Auto m) where
    loop a = mkAutoM (loop <$> loadAuto a)
                     (saveAuto a)
                     $ \x -> liftM (onOutput fst loop)
                             . mfix
                             $ \ ~(Output (_, d) _) -> stepAuto a (x, d)

-- Utility instances

instance (Monad m, Semigroup b) => Semigroup (Auto m a b) where
    (<>) = liftA2 (<>)

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

-- Semigroup, Monoid, Num, Fractional, and Floating instances for Output
-- because why not.

instance (Monad m, Semigroup b) => Semigroup (Output m a b) where
    (<>) = liftA2 (<>)

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

