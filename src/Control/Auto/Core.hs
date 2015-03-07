{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Control.Auto.Core
-- Description : Core types, constructors, and utilities.
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- This module defines and provides the core types, (smart) constructors,
-- and general high and low-level utilities used by the /auto/ library.
--
-- A lot of low-level functionality is provided here which is most likely
-- unnecessary for most applications; many are mostly for internal usage or
-- advanced/fine-grained usage.  It also isn't really enough to do too many
-- useful things, either.  It's recommended that you import "Control.Auto"
-- instead, which re-organizes the more useful parts of this module in
-- addition with useful parts of others to provide a nice packaged entry
-- point.  If something in here becomes useful for more than just
-- fine-tuning or low-level tweaking, it is probably supposed to be in
-- "Control.Auto" anyway.
--
-- Information on how to use these types is available in the
-- <https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md tutorial>!
--

module Control.Auto.Core (
  -- * Auto
  -- ** Type
    Auto
  , Auto'
  , autoConstr
  , toArb
  , purifyAuto
  -- ** Running
  , stepAuto
  , stepAuto'
  , evalAuto
  , evalAuto'
  , execAuto
  , execAuto'
  -- ** Serializing
  -- $serializing
  , encodeAuto
  , decodeAuto
  , saveAuto
  , loadAuto
  , unserialize
  -- ** Underlying monad
  , hoistA
  , generalizeA
  -- ** Special modifiers
  , interceptO
  -- * Auto constructors
  -- ** Lifting values and functions
  , mkConst
  , mkConstM
  , mkFunc
  , mkFuncM
  -- ** from State transformers
  , mkState
  , mkState_
  , mkStateM
  , mkStateM_
  , mkState'
  , mkStateM'
  -- ** from Accumulators
  -- *** Result-first
  , accum
  , accum_
  , accumM
  , accumM_
  -- *** Initial accumulator-first
  , accumD
  , accumD_
  , accumMD
  , accumMD_
  -- ** Arbitrary Autos
  , mkAuto
  , mkAuto_
  , mkAutoM
  , mkAutoM_
  -- * Strictness
  , forceSerial
  , forcer
  , seqer
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.DeepSeq
import Control.Monad hiding   (sequence)
import Control.Monad.Fix
import Data.ByteString hiding (empty)
import Data.Functor.Identity
import Data.Profunctor
import Data.Semigroup
import Data.Serialize
import Data.Traversable
import Data.Typeable
import Prelude hiding         ((.), id, sequence)


-- | The 'Auto' type.  For this library, an 'Auto' semantically
-- represents/denotes a /a relationship/ between an input and an
-- output that is preserved over multiple steps, where that relationship is
-- (optionally) maintained within the context of a monad.
--
-- A lot of fancy words, I know...but you can think of an 'Auto' as nothing
-- more than a "stream transformer".  A stream of sequential inputs come in
-- one at a time, and a stream of outputs pop out one at a time, as well.
--
-- Using the 'streamAuto' function, you can "unwrap" the inner stream
-- transformer from any 'Auto': if @a :: 'Auto' m a b@, 'streamAuto' lets
-- you turn it into an @[a] -> m [b]@.  "Give me a stream of @a@s, one at
-- a time, and I'll give you a list of @b@s, matching a relationship to
-- your stream of @a@s."
--
-- @
-- -- unwrap your inner [a] -> m [b]!
-- 'streamAuto' :: Monad m => 'Auto' m a b -> ([a] -> m [b])
-- @
--
-- There's a handy type synonym 'Auto'' for relationships that don't really
-- need a monadic context; the @m@ is just 'Identity':
--
-- @
-- type Auto' = Auto Identity
-- @
--
-- So if you had an @a :: 'Auto'' a b@, you can use 'streamAuto'' to
-- "unwrap" the inner stream transformer, @[a] -> [b]@.
--
-- @
-- -- unwrap your inner [a] -> [b]!
-- 'streamAuto'' :: 'Auto'' a b -> ([a] -> [b])
-- @
--
-- All of the 'Auto's given in this library maintain some sort of semantic
-- relationship between streams --- for some, the outputs might be the
-- inputs with a function applied; for others, the outputs might be the
-- cumulative sum of the inputs.
--
-- See the
-- <https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md tutorial>
-- for more information!
--
-- Operationally, an  @'Auto' m a b@ is implemented as a "stateful
-- function".  A function from an @a@ where, every time you "apply" it, you
-- get a @b@ and an "updated 'Auto'"/function with updated state.
--
-- You can get this function using 'stepAuto':
--
-- @
-- 'stepAuto' :: Auto m a b -> (a -> (b, Auto m a b))
-- @
--
-- --
-- --
-- --
-- --
-- -- 
-- -- In a way, you can think about 'Auto's as /stream transformers/.
-- -- A stream of sequential inputs come in one at a time, and a stream of
-- -- outputs pop out one at a time as well.  You can think of 'streamAuto''
-- -- as taking an `Auto' a b` and "unwrapping" its internal `[a] -> [b]`.
-- -- 
-- -- The Auto type.  Basically represents a function containing its own
-- -- localized internal state.  If you have an @'Auto' a b@, you can "step"
-- -- it with 'stepAuto' and an @a@, to get a @b@ and a "next 'Auto'".  The
-- -- @a@ is the input, and the @b@ is the output, and the next 'Auto' is the
-- -- 'Auto' with updated internal state.
-- --
-- -- The "stepping" process can be monadic:
-- --
-- -- > stepAuto :: Auto m a b -> a -> m (Output m a b)
-- --
-- -- So you can have access to, say, a shared environment using 'Reader' or
-- -- something like that.
-- --
-- -- 'Auto' is mostly useful because of its 'Functor', 'Applicative',
-- -- 'Category', and 'Arrow' (and Arrow-related) instances.  These allow you
-- -- to modify, combine, chain, and side-chain Autos in expressive way,
-- -- allowing you to build up complex ones from combinations of simple,
-- -- primitive ones.
-- --
-- -- TODO: see tutorial
-- --
-- -- The 'Auto' also contains information on its own serialization, so you
-- -- can serialize and re-load the internal state without actually accessing
-- -- it.
data Auto m a b =           AutoFunc    !(a -> b)
                |           AutoFuncM   !(a -> m b)
                | forall s. AutoState   (Get s, s -> Put) !(a -> s -> (b, s))   !s
                | forall s. AutoStateM  (Get s, s -> Put) !(a -> s -> m (b, s)) !s
                |           AutoArb     (Get (Auto m a b)) Put !(a -> (b, Auto m a b))
                |           AutoArbM    (Get (Auto m a b)) Put !(a -> m (b, Auto m a b))
                deriving ( Typeable )

-- | Special case of 'Auto' where the underlying 'Monad' is 'Identity'.
type Auto'   = Auto Identity


-- | Re-structure 'Auto' internals to use the 'Arb' ("arbitrary")
-- constructors, as recursion-based mealy machines.  Almost always a bad
-- idea in every conceivable situation.  Why is it even here?
toArb :: Monad m => Auto m a b -> Auto m a b
toArb a = a_
  where
    a_ = case a of
           AutoFunc f  -> AutoArb  (pure a_)
                                   (return ())
                                 $ \x -> (f x, a_)
           AutoFuncM f -> AutoArbM (pure a_)
                                   (return ())
                                 $ \x -> liftM (, a_) (f x)
           AutoState gp@(g,p) f s  ->
                          let a__ s' = AutoArb (toArb . AutoState gp f <$> g)
                                               (p s')
                                             $ \x -> let (y, s'') = f x s'
                                                     in  (y, a__ s'')
                          in  a__ s
           AutoStateM gp@(g,p) f s ->
                          let a__ s' = AutoArbM (toArb . AutoStateM gp f <$> g)
                                                (p s)
                                              $ \x -> do
                                                  (y, s'') <- f x s'
                                                  return (y, a__ s'')
                          in  a__ s
           _                       -> a


-- | Returns a string representation of the internal constructor of the
-- 'Auto'.  Useful for debugging the result of compositions and functions
-- and seeing how they affect the internal structure of the 'Auto'.
autoConstr :: Auto m a b -> String
autoConstr (AutoFunc {})   = "AutoFunc"
autoConstr (AutoFuncM {})  = "AutoFuncM"
autoConstr (AutoState {})  = "AutoState"
autoConstr (AutoStateM {}) = "AutoStateM"
autoConstr (AutoArb {})    = "AutoArb"
autoConstr (AutoArbM {})   = "AutoArbM"

-- | Swaps out the underlying 'Monad' of an 'Auto' using the given monad
-- morphism "transforming function".
--
-- Should be free for non-monadic functions.
hoistA :: (Monad m, Monad m')
       => (forall c. m c -> m' c)
       -> Auto m a b -> Auto m' a b
hoistA _ (AutoFunc f)        = AutoFunc f
hoistA g (AutoFuncM f)       = AutoFuncM (g . f)
hoistA _ (AutoState gp f s)  = AutoState gp f s
hoistA g (AutoStateM gp f s) = AutoStateM gp (\x s' -> g (f x s')) s
hoistA g (AutoArb gt pt f)   = AutoArb (fmap (hoistA g) gt)
                                       pt
                                       $ \x -> let (y, a') = f x
                                               in  (y, hoistA g a')
hoistA g (AutoArbM gt pt f)  = AutoArbM (fmap (hoistA g) gt)
                                        pt
                                        $ \x -> g $ do
                                            (y, a') <- f x
                                            return (y, hoistA g a')

-- | Generalizes an 'Auto'' to any 'Auto' m a b', using 'hoist'.
--
-- Should be free for non-monadic functions.
generalizeA :: Monad m => Auto' a b -> Auto m a b
generalizeA = hoistA (return . runIdentity)

-- | Force the serializing components of an 'Auto'.
forceSerial :: Auto m a b -> Auto m a b
forceSerial a = case a of
                  AutoArb _ l s  -> l `seq` s `seq` a
                  AutoArbM _ l s -> l `seq` s `seq` a
                  _              -> a

-- $serializing
--
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
-- For example, let's say I have @a = 'accum' (+) 0@, the 'Auto' that
-- returns the sum of everything it has received so far.  If I feed it
-- 3 and 10, it'll have its internal accumulator as 13, keeping track of
-- all the numbers it has seen so far.
--
-- >>> let a             = accum (+) 0
-- >>> let Output _ a'   = stepAuto' a  3
-- >>> let Output _ a''  = stepAuto' a' 10
--
-- I can then use 'encodeAuto' to "freeze"/"save" the 'Auto' into the
-- 'ByteString' @bs@:
--
-- >>> let bs            = encodeAuto a''
--
-- To "resume" / "load" it, I can use 'decodeAuto' to "resume" the
-- /original/ @a@.  Remember, 'a' was our original 'Auto', the summer
-- 'Auto' with a starting accumulator of 0.  We use 'decodeAuto' to
-- "resume" it, with and resume it with its internal accumulator at 13.
--
-- >>> let Right resumed = decodeAuto a bs
-- >>> let Output y _    = stepAuto' resumed 0
-- 13
--
-- Note that all of these would have had the same result:
--
-- >>> let Right resumed = decodeAuto a'  bs
-- >>> let Right resumed = decodeAuto a'' bs
-- >>> let Right resumed = decodeAuto (accum (+) 0) bs
--
-- I mean, after all, if 'decodeAuto' "fast forwards" an 'Auto' to the
-- state it was at when it was frozen...then all of these should really be
-- resumed to the same point, right?
--
-- One way you can think about it is that 'loadAuto' / 'decodeAuto' takes
-- an 'Auto' and creates a "blueprint" from that 'Auto', on how to "load
-- it"; the blueprint contains what the form of the internal state is, and
-- their offets in the 'ByteString'.  So in the above, 'a', 'a'', 'a''',
-- and 'accum (+) 0' all have the same "blueprint" --- their internal
-- states are of the same structure.
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
{-# INLINE encodeAuto #-}

-- | "Resume" an 'Auto' from its 'ByteString' serialization, giving
-- a 'Left' if the deserialization is not possible.
decodeAuto :: Auto m a b -> ByteString -> Either String (Auto m a b)
decodeAuto = runGet . loadAuto
{-# INLINE decodeAuto #-}

-- | Returns a 'Get' from an 'Auto' ---  instructions (from
-- "Data.Serialize") on taking a ByteString and "restoring" the originally
-- saved 'Auto', in the originally saved state.
loadAuto :: Auto m a b -> Get (Auto m a b)
loadAuto a = case a of
               AutoState gp f _  -> AutoState  gp f <$> fst gp
               AutoStateM gp f _ -> AutoStateM gp f <$> fst gp
               AutoArb g _ _     -> g
               AutoArbM g _ _    -> g
               _                 -> return a
-- loadAuto = return
{-# INLINE loadAuto #-}

-- | Returns a 'Put' --- instructions (from "Data.Serialize") on how to
-- "freeze" the 'Auto', with its internal state, and save it to a binary
-- encoding.  It can later be reloaded and "resumed" by
-- 'loadAuto'/'decodeAuto'.
saveAuto :: Auto m a b -> Put
saveAuto a = case a of
               AutoState (_, p) _ s  -> p s
               AutoStateM (_, p) _ s -> p s
               AutoArb _ p _         -> p
               AutoArbM _ p _        -> p
               _                     -> return ()
-- saveAuto _ = return ()
{-# INLINE saveAuto #-}

unserialize :: Monad m => Auto m a b -> Auto m a b
unserialize a =
    case a of
        AutoFunc _       -> a
        AutoFuncM _      -> a
        AutoState _ f s  -> AutoState (pure s, const (put ())) f s
        AutoStateM _ f s -> AutoStateM (pure s, const (put ())) f s
        AutoArb _ _ f    -> AutoArb (pure a) (put ()) (second unserialize . f)
        AutoArbM _ _ f   -> AutoArbM (pure a) (put ()) (liftM (second unserialize) . f)

-- | "Runs" the 'Auto' through one step.
--
-- Remember that at every step for an @'Auto' m a b@, you provide an @a@
-- input and receive a @b@ output with an "updated"/"next" 'Auto'.
--
-- >>> let a = accum (+) 0 :: Auto Identity Int Int
--             -- an Auto that sums all of its input.
-- >>> let Identity (Output y a') = stepAuto a 3
-- >>> y      -- the result
-- 3 :: Int
-- >>> :t a'   -- the updated 'Auto'
-- a' :: Auto Identity Int Int
--
-- ('Identity', from "Data.Functor.Identity", is the "dumb Functor": @data
-- 'Identity' a = 'Identity' a@)
--
-- If you think of an @'Auto' m a b@ as a "stateful function" of type @a ->
-- m b@, then 'stepAuto' lets you "run" it.
stepAuto :: Monad m
         => Auto m a b        -- ^ the 'Auto' to step
         -> a                 -- ^ the input
         -> m (b, Auto m a b)  -- ^ the output, and the updated 'Auto''.
stepAuto a x = case a of
                 AutoFunc f        ->
                     return (f x, a)
                 AutoFuncM f       -> do
                     y <- f x
                     return (y, a)
                 AutoState gp f s  ->
                     let (y, s') = f x s
                         a'      = AutoState gp f s'
                     in  return (y, a')
                 AutoStateM gp f s -> do
                     (y, s') <- f x s
                     let a' = AutoStateM gp f s'
                     return (y, a')
                 AutoArb _ _ f     -> return (f x)
                 AutoArbM _ _ f    -> f x
{-# INLINE stepAuto #-}

-- | 'stepAuto', but for an 'Auto'' --- the underlying 'Monad' is
-- 'Identity'.  Returns the output stripped of 'Identity'.
--
-- If you think of an @'Auto'' a b@ as a "stateful function" of type
-- @a -> b@, then 'stepAuto'' lets you "run" it.
stepAuto' :: Auto' a b        -- ^ the 'Auto'' to step
          -> a                -- ^ the input
          -> (b, Auto' a b)   -- ^ the output, and the updated 'Auto''
-- stepAuto' a = runIdentity . stepAuto a
stepAuto' a x = case a of
                  AutoFunc f        -> (f x, a)
                  AutoFuncM f       -> (runIdentity (f x), a)
                  AutoState gp f s  -> let (y, s') = f x s
                                           a'      = AutoState gp f s'
                                       in  (y, a')
                  AutoStateM gp f s -> let (y, s') = runIdentity (f x s)
                                           a'      = AutoStateM gp f s'
                                       in  (y, a')
                  AutoArb _ _ f     -> f x
                  AutoArbM _ _ f    -> runIdentity (f x)
{-# INLINE stepAuto' #-}

-- | Playing around with an optimization hack.  If you have an 'Auto'', run
-- 'purifyAuto' on it before you "step" or "stream" it...you might get
-- performance benefits.  Benchmarks to be written!
purifyAuto :: Auto' a b -> Auto' a b
purifyAuto a@(AutoFunc {})     = a
purifyAuto (AutoFuncM f)       = AutoFunc (runIdentity . f)
purifyAuto a@(AutoState {})    = a
purifyAuto (AutoStateM gp f s) = AutoState gp (\x s' -> runIdentity (f x s')) s
purifyAuto (AutoArb g p f)     = AutoArb (purifyAuto <$> g)
                                         p
                                       $ \x -> let (y, a') = f x
                                               in  (y, purifyAuto a')
purifyAuto (AutoArbM g p f)    = AutoArb (purifyAuto <$> g)
                                         p
                                       $ \x -> let (y, a') = runIdentity (f x)
                                               in  (y, purifyAuto a')

-- | Like 'stepAuto', but drops the "next 'Auto'" and just gives the
-- result.
evalAuto :: Monad m
         => Auto m a b
         -> a
         -> m b
evalAuto a = liftM fst . stepAuto a

-- | Like 'stepAuto'', but drops the "next 'Auto''" and just gives the
-- result.  'evalAuto' for 'Auto''.
evalAuto' :: Auto' a b
          -> a
          -> b
evalAuto' a = fst . stepAuto' a

-- | Like 'stepAuto', but drops the result and just gives the "next
-- 'Auto'".
execAuto :: Monad m
         => Auto m a b
         -> a
         -> m (Auto m a b)
execAuto a = liftM snd . stepAuto a

-- | Like 'stepAuto'', but drops the result and just gives the "next
-- 'Auto''".  'execAuto' for 'Auto''.
execAuto' :: Auto' a b
          -> a
          -> Auto' a b
execAuto' a = snd . stepAuto' a

-- | A special 'Auto' that acts like the 'id' 'Auto', but forces results as
-- they come through to be fully evaluated, when composed with other
-- 'Auto's.
forcer :: NFData a => Auto m a a
forcer = mkAuto_ $ \x -> x `deepseq` (x, forcer)
{-# INLINE forcer #-}

-- | A special 'Auto' that acts like the 'id' 'Auto', but forces results as
-- they come through to be evaluated to Weak Head Normal Form, with 'seq',
-- when composed with other 'Auto's.
seqer :: Auto m a a
seqer = mkAuto_ $ \x -> x `seq` (x, seqer)
{-# INLINE seqer #-}

interceptO :: Monad m => ((b, Auto m a b) -> m c) -> Auto m a b -> Auto m a c
interceptO f = go
  where
    go a0 = mkAutoM (go <$> loadAuto a0)
                    (saveAuto a0)
                  $ \x -> do
                        o@(_, a1) <- stepAuto a0 x
                        y <- f o
                        return (y, go a1)

-- compMAuto :: (Monad m, Monad m') => Auto m b (m' c) -> Auto m a (m' b) -> Auto m a (m' c)
-- compMAuto g f = AutoArbM undefined
--                          undefined
--                          $ \x -> do
--                              Output y f' <- stepAuto f x
--                              undefined


-- doesn't work like you'd think lol.
-- serialForcer :: Monad m => Auto m a a
-- serialForcer = a
--   where
--     a = mkAuto_ $ \x -> let outp = Output x a
--                         in  forceSerial a `seq` outp

-- | Construct an 'Auto' by explicity giving its serialization,
-- deserialization, and the (pure) function from @a@ to @b@ and the "next
-- 'Auto'".
--
-- Ideally, you wouldn't have to use this unless you are making your own
-- framework.  Try your best to make what you want by assembling
-- primtives together.
mkAuto :: Get (Auto m a b)          -- ^ resuming/loading 'Get'
       -> Put                       -- ^ saving 'Put'
       -> (a -> (b, Auto m a b))    -- ^ step function
       -> Auto m a b
mkAuto = AutoArb
{-# INLINE mkAuto #-}

-- | Construct an 'Auto' by explicitly giving its serializiation,
-- deserialization, and the (monadic) function from @a@ to @b@ and the
-- "next 'Auto'".
--
-- Ideally, you wouldn't have to use this unless you are making your own
-- framework.  Try your best to make what you want by assembling
-- primtives together.
--
-- TODO: Tutorial!!!!
mkAutoM :: Get (Auto m a b)             -- ^ resuming/loading 'Get'
        -> Put                          -- ^ saving 'Put'
        -> (a -> m (b, Auto m a b))     -- ^ (monadic) step function
        -> Auto m a b
mkAutoM = AutoArbM
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
mkAuto_ :: (a -> (b, Auto m a b))       -- ^ step function
        -> Auto m a b
mkAuto_ f = mkAuto (pure (mkAuto_ f)) (return ()) f
{-# INLINE mkAuto_ #-}

-- | Like 'mkAutoM', but without any way of meaningful serializing or
-- deserializing.
--
-- Be careful!  This 'Auto' can still carry arbitrary internal state, but
-- it cannot be meaningfully serialized or re-loaded/resumed.  You can
-- still pretend to do so using
-- 'loadAuto'/'saveAuto'/'encodeAuto'/'decodeAuto' (and the type system
-- won't stop you), but when you try to "resume"/decode it, its state will
-- be reset.
mkAutoM_ :: (a -> m (b, Auto m a b))    -- ^ (monadic) step function
         -> Auto m a b
mkAutoM_ f = mkAutoM (pure (mkAutoM_ f)) (return ()) f
{-# INLINE mkAutoM_ #-}

-- | Construct the 'Auto' that always yields the given value, ignoring its
-- input.
--
-- Provided for API constency, but you should really be using 'pure' from
-- the 'Applicative' instance, from "Control.Applicative", which does the
-- same thing.
mkConst :: b            -- ^ constant value to be outputted
        -> Auto m a b
mkConst = AutoFunc . const
{-# INLINE mkConst #-}

-- | Construct the 'Auto' that always "executes" the given monadic value at
-- every step, yielding the result and ignoring its input.
--
-- Provided for API consistency, but you shold really be using 'effect'
-- from "Control.Auto.Effects", which does the same thing.
mkConstM :: m b           -- ^ monadic action to be executed at every step
         -> Auto m a b
mkConstM = AutoFuncM . const
{-# INLINE mkConstM #-}

-- | Construct a stateless 'Auto' that simply applies the given (pure)
-- function to every input, yielding the output.
--
-- This is rarely needed; you should be using 'arr' from the 'Arrow'
-- instance, from "Control.Arrow".
mkFunc :: (a -> b)        -- ^ pure function
       -> Auto m a b
mkFunc = AutoFunc
{-# INLINE mkFunc #-}

-- | Construct a statelss 'Auto' that simply applies and executes the givne
-- (monadic) function to every input, yielding the output.
--
-- It's recommended that you use 'arrM' from "Control.Auto.Effects".  This
-- is only really provided for consistency.
mkFuncM :: (a -> m b)     -- ^ "monadic" function
        -> Auto m a b
mkFuncM = AutoFuncM
{-# INLINE mkFuncM #-}

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
-- either write a meaningful one, provide the serialization methods
-- manually with 'mkState'', or throw away serializability and use
-- 'mkState_'.
mkState :: Serialize s
        => (a -> s -> (b, s))       -- ^ state transformer
        -> s                        -- ^ intial state
        -> Auto m a b
mkState = AutoState (get, put)
{-# INLINE mkState #-}

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
-- either write a meaningful one, provide the serialization methods
-- manually with 'mkStateM'', or throw away serializability and use
-- 'mkStateM_'.
mkStateM :: Serialize s
         => (a -> s -> m (b, s))      -- ^ (monadic) state transformer
         -> s                         -- ^ initial state
         -> Auto m a b
mkStateM = AutoStateM (get, put)
{-# INLINE mkStateM #-}

-- | A version of 'mkState', where the internal state doesn't have
-- a 'Serialize' instance, so you provide your own instructions for getting
-- and putting the state.
mkState' :: Get s                     -- ^ 'Get'; strategy for reading and deserializing the state
         -> (s -> Put)                -- ^ 'Put'; strategy for serializing given state
         -> (a -> s -> (b, s))        -- ^ state transformer
         -> s                         -- ^ intial state
         -> Auto m a b
mkState' = curry AutoState
{-# INLINE mkState' #-}

-- | A version of 'mkStateM', where the internal state doesn't have
-- a 'Serialize' instance, so you provide your own instructions for getting
-- and putting the state.
mkStateM' :: Get s                      -- ^ 'Get'; strategy for reading and deserializing the state
          -> (s -> Put)                 -- ^ 'Put'; strategy for serializing given state
          -> (a -> s -> m (b, s))       -- ^ (monadic) state transformer
          -> s                          -- ^ initial state
          -> Auto m a b
mkStateM' = curry AutoStateM
{-# INLINE mkStateM' #-}

-- | A version of 'mkState', where the internal state isn't serialized.  It
-- can be "saved" and "loaded", but the state is lost in the process.
--
-- Useful if your state @s@ cannot have a meaningful 'Serialize' instance.
mkState_ :: (a -> s -> (b, s))    -- ^ state transformer
         -> s                     -- ^ initial state
         -> Auto m a b
mkState_ f s0 = AutoState (return s0, \_ -> return ()) f s0
{-# INLINE mkState_ #-}

-- | A version of 'mkStateM', where the internal state isn't serialized.
-- It can be "saved" and "loaded", but the state is lost in the process.
--
-- Useful if your state @s@ cannot have a meaningful 'Serialize' instance.
mkStateM_ :: (a -> s -> m (b, s))   -- ^ (monadic) state transformer
          -> s                      -- ^ initial state
          -> Auto m a b
mkStateM_ f s0 = AutoStateM (return s0, \_ -> return ()) f s0
{-# INLINE mkStateM_ #-}

-- | Construct an 'Auto' from a "folding" function: @b -> a -> b@ yields an
-- @'Auto' m a b@.  Basically acts like a 'foldl' or a 'scanl'.  There is
-- an internal accumulator that is "updated" with an @a@ at every step.
-- Must be given an initial accumulator.
--
-- Example: an 'Auto' that sums up all of its input.
--
-- >>> let summer = accum (+) 0
-- >>> let Output sum1 summer' = stepAuto summer 3
-- >>> sum1
-- 3
-- >>> let Output sum2 _       = stepAuto summer'' 10
-- >>> sum2
-- 13
--
-- If your accumulator @b@ does not have a 'Serialize' instance, then you
-- should either write a meaningful one, or throw away serializability and
-- use 'accum_'.
accum :: Serialize b
        => (b -> a -> b)      -- ^ accumulating function
        -> b                  -- ^ initial accumulator
        -> Auto m a b
accum f = mkState (\x s -> let y = f s x in (y, y))
{-# INLINE accum #-}

-- | Construct an 'Auto' from a "monadic" "folding" function: @b -> a ->
-- m b@ yields an @'Auto' m a b@.  Basically acts like a 'foldM' or 'scanM'
-- (if it existed).  here is an internal accumulator that is "updated" with
-- an input @a@ with the result of the executed @m b@ at every step.  Must
-- be given an initial accumulator.
--
-- If your accumulator @b@ does not have a 'Serialize' instance, then you
-- should either write a meaningful one, or throw away serializability and
-- use 'accumM_'.
accumM :: (Serialize b, Monad m)
         => (b -> a -> m b)       -- ^ (monadic) accumulating function
         -> b                     -- ^ initial accumulator
         -> Auto m a b
accumM f = mkStateM (\x s -> liftM (join (,)) (f s x))
{-# INLINE accumM #-}

-- | A version of 'accum_, where the internal accumulator isn't
-- serialized. It can be "saved" and "loaded", but the state is lost in the
-- process.
--
-- Useful if your accumulator @b@ cannot have a meaningful 'Serialize'
-- instance.
accum_ :: (b -> a -> b)   -- ^ accumulating function
         -> b               -- ^ intial accumulator
         -> Auto m a b
accum_ f = mkState_ (\x s -> let y = f s x in (y, y))
{-# INLINE accum_ #-}

-- | A version of 'accumM_, where the internal accumulator isn't
-- serialized. It can be "saved" and "loaded", but the state is lost in the
-- process.
--
-- Useful if your accumulator @b@ cannot have a meaningful 'Serialize'
-- instance.
accumM_ :: Monad m
          => (b -> a -> m b)    -- ^ (monadic) accumulating function
          -> b                  -- ^ initial accumulator
          -> Auto m a b
accumM_ f = mkStateM_ (\x s -> liftM (join (,)) (f s x))
{-# INLINE accumM_ #-}

-- | A "delayed" version of 'accum', where the first output is actually
-- the initial state of the accumulator.  Useful in recursive bindings.
accumD :: Serialize b
         => (b -> a -> b)      -- ^ accumulating function
         -> b                  -- ^ initial accumulator
         -> Auto m a b
accumD f = mkState (\x s -> (s, f s x))
{-# INLINE accumD #-}

-- | A "delayed" version of 'accumM', where the first output is actually
-- the initial state of the accumulator.  Useful in recursive bindings.
accumMD :: (Serialize b, Monad m)
          => (b -> a -> m b)       -- ^ (monadic) accumulating function
          -> b                     -- ^ initial accumulator
          -> Auto m a b
accumMD f = mkStateM (\x s -> liftM (s,) (f s x))
{-# INLINE accumMD #-}

-- | The non-resuming/non-serializing version of 'accumD'.
accumD_ :: (b -> a -> b)   -- ^ accumulating function
          -> b               -- ^ intial accumulator
          -> Auto m a b
accumD_ f = mkState_ (\x s -> (s, f s x))
{-# INLINE accumD_ #-}

-- | The non-resuming/non-serializing version of 'accumMD'.
accumMD_ :: Monad m
           => (b -> a -> m b)    -- ^ (monadic) accumulating function
           -> b                  -- ^ initial accumulator
           -> Auto m a b
accumMD_ f = mkStateM_ (\x s -> liftM (s,) (f s x))
{-# INLINE accumMD_ #-}

instance Monad m => Functor (Auto m a) where
    fmap = rmap
    {-# INLINE fmap #-}

instance Monad m => Applicative (Auto m a) where
    pure      = mkConst
    {-# INLINE pure #-}
    af <*> ax = case (af, ax) of
                  (AutoFunc f, AutoFunc x)  ->
                      AutoFunc (f <*> x)
                  (AutoFunc f, AutoFuncM x) ->
                      AutoFuncM $ \i -> liftM (f i) (x i)
                  (AutoFunc f, AutoState gp x s) ->
                      AutoState gp (\i s' -> first (f i) (x i s')) s
                  (AutoFunc f, AutoStateM gp x s) ->
                      AutoStateM gp (\i s' -> liftM (first (f i)) (x i s')) s
                  (AutoFunc f, AutoArb l s x) ->
                      AutoArb (fmap (af <*>) l) s $ \i -> (f i *** (af <*>)) $ x i
                  (AutoFunc f, AutoArbM l s x) ->
                      AutoArbM (fmap (af <*>) l) s $ \i -> liftM (f i *** (af <*>)) (x i)
                  (AutoFuncM f, AutoFunc x) ->
                      AutoFuncM $ \i -> liftM ($ x i) (f i)
                  (AutoFuncM f, AutoFuncM x) ->
                      AutoFuncM $ \i -> f i `ap` x i
                  (AutoFuncM f, AutoState gp x s) ->
                      AutoStateM gp (\i s' -> liftM (($ x i s') . first) (f i)) s
                  (AutoFuncM f, AutoStateM gp x s) ->
                      AutoStateM gp (\i s' -> liftM2 first (f i) (x i s')) s
                  (AutoFuncM f, AutoArb l s x) ->
                      AutoArbM (fmap (af <*>) l) s $ \i -> liftM (($ x i) . (*** (af <*>))) (f i)
                  (AutoFuncM f, AutoArbM l s x) ->
                      AutoArbM (fmap (af <*>) l) s $ \i -> liftM2 (*** (af <*>)) (f i) (x i)
                  (AutoState gp f s, AutoFunc x) ->
                      AutoState gp (\i s' -> first ($ x i) (f i s')) s
                  (AutoState gp f s, AutoFuncM x) ->
                      AutoStateM gp (\i s' -> liftM (\x' -> first ($ x') (f i s')) (x i)) s
                  (AutoState gpf f sf, AutoState gpx x sx) ->
                      AutoState (mergeStSt gpf gpx)
                                (\i (sf', sx') -> let (f', sf'') = f i sf'
                                                      (x', sx'') = x i sx'
                                                  in  (f' x', (sf'', sx'')))
                                (sf, sx)
                  (AutoState gpf f sf, AutoStateM gpx x sx) ->
                      AutoStateM (mergeStSt gpf gpx)
                                 (\i (sf', sx') -> do let (f', sf'') = f i sf'
                                                      (x', sx'') <- x i sx'
                                                      return (f' x', (sf'', sx'')))
                                 (sf, sx)
                  (AutoStateM gp f s, AutoFunc x) ->
                      AutoStateM gp (\i s' -> liftM (first ($ x i)) (f i s')) s
                  (AutoStateM gp f s, AutoFuncM x) ->
                      AutoStateM gp (\i s' -> do (f', s'') <- f i s'
                                                 x' <- x i
                                                 return (f' x', s'')
                                    ) s
                  (AutoStateM gpf f sf, AutoState gpx x sx) ->
                      AutoStateM (mergeStSt gpf gpx)
                                 (\i (sf', sx') -> do (f', sf'') <- f i sf'
                                                      let (x', sx'') = x i sx'
                                                      return (f' x', (sf'', sx''))
                                 ) (sf, sx)
                  (AutoStateM gpf f sf, AutoStateM gpx x sx) ->
                      AutoStateM (mergeStSt gpf gpx)
                                 (\i (sf', sx') -> do (f', sf'') <- f i sf'
                                                      (x', sx'') <- x i sx'
                                                      return (f' x', (sf'', sx''))
                                 ) (sf, sx)
                  -- i give up!
                  _ -> uncurry ($) <$> (af &&& ax)
    {-# INLINE (<*>) #-}

-- Should this even be here?  It might be kind of dangerous/unexpected.
instance (Monad m, Alternative m) => Alternative (Auto m a) where
    empty     = mkConstM empty
    a1 <|> a2 = mkAutoM ((<|>) <$> loadAuto a1 <*> loadAuto a2)
                        (saveAuto a1 *> saveAuto a2)
                        $ \x -> let res1  = second (<|> a2) `liftM` stepAuto a1 x
                                    res2  = second (a1 <|>) `liftM` stepAuto a2 x
                                in  res1 <|> res2


instance Monad m => Category (Auto m) where
    id      = mkFunc id
    ag . af = case (ag, af) of
                (AutoFunc g, AutoFunc f)          ->
                    AutoFunc   (g . f)
                (AutoFunc g, AutoFuncM f)         ->
                    AutoFuncM  (return . g <=< f)
                (AutoFunc g, AutoState gpf f s)   ->
                    AutoState gpf (\x s' -> first g (f x s')) s
                (AutoFunc g, AutoStateM gpf f s)  ->
                    AutoStateM gpf (\x s' -> liftM (first g) (f x s')) s
                (AutoFunc g, AutoArb l s f)       ->
                    AutoArb (fmap (ag .) l) s $ \x -> (g *** fmap g) (f x)
                (AutoFunc g, AutoArbM l s f)      ->
                    AutoArbM (fmap (ag .) l) s $ \x -> liftM (g *** fmap g) (f x)
                (AutoFuncM g, AutoFunc f)         ->
                    AutoFuncM (g <=< return . f)
                (AutoFuncM g, AutoFuncM f)        ->
                    AutoFuncM (g <=< f)
                (AutoFuncM g, AutoState gpf f s)  ->
                    AutoStateM gpf (\x s' -> firstM g (f x s')) s
                (AutoFuncM g, AutoStateM gpf f s) ->
                    AutoStateM gpf (\x s' -> firstM g =<< f x s') s
                (AutoFuncM g, AutoArb l s f)      ->
                    AutoArbM (fmap (ag .) l)
                             s
                           $ \x -> do
                               let (y, af') = f x
                               y' <- g y
                               return (y', ag . af')
                (AutoFuncM g, AutoArbM l s f)     ->
                    AutoArbM (fmap (ag .) l)
                             s
                           $ \x -> do
                               (y, af') <- f x
                               y' <- g y
                               return (y', ag . af')
                (AutoState gpg g sg, AutoFunc f)  ->
                    AutoState gpg (g . f) sg
                (AutoState gpg g sg, AutoFuncM f) ->
                    AutoStateM gpg (\x sg' -> liftM (`g` sg') (f x)) sg
                (AutoState gpg g sg, AutoState gpf f sf) ->
                    AutoState (mergeStSt gpg gpf)
                              (\x (sg', sf') -> let (y, sf'') = f x sf'
                                                    (z, sg'') = g y sg'
                                                in  (z, (sg'', sf'')) )
                              (sg, sf)
                (AutoState gpg g sg, AutoStateM gpf f sf) ->
                    AutoStateM (mergeStSt gpg gpf)
                               (\x (sg', sf') -> do
                                    (y, sf'') <- f x sf'
                                    let (z, sg'') = g y sg'
                                    return (z, (sg'', sf'')) )
                               (sg, sf)
                (AutoState gpg@(gg,pg) g sg, AutoArb l s f) ->
                    AutoArb (liftA2 (\sg' af' -> AutoState gpg g sg' . af') gg l)
                            (pg sg *> s)
                            $ \x -> let (y, af') = f x
                                        (z, sg') = g y sg
                                        ag'      = AutoState gpg g sg'
                                    in  (z, ag' . af')
                (AutoState gpg@(gg,pg) g sg, AutoArbM l s f) ->
                    AutoArbM (liftA2 (\sg' af' -> AutoState gpg g sg' . af') gg l)
                             (pg sg *> s)
                             $ \x -> do
                                 (y, af') <- f x
                                 let (z, sg') = g y sg
                                     ag'      = AutoState gpg g sg'
                                 return (z, ag' . af')
                (AutoStateM gpg g sg, AutoFunc f)       ->
                    AutoStateM gpg (g <=< return . f) sg
                (AutoStateM gpg g sg, AutoFuncM f)      ->
                    AutoStateM gpg (\x sg' -> flip g sg' =<< f x) sg
                (AutoStateM gpg g sg, AutoState gpf f sf) ->
                    AutoStateM (mergeStSt gpg gpf)
                               (\x (sg', sf') -> do
                                  let (y, sf'') = f x sf'
                                  (z, sg'') <- g y sg'
                                  return (z, (sg'', sf'')) )
                               (sg, sf)
                (AutoStateM gpg g sg, AutoStateM gpf f sf) ->
                    AutoStateM (mergeStSt gpg gpf)
                               (\x (sg', sf') -> do
                                  (y, sf'') <- f x sf'
                                  (z, sg'') <- g y sg'
                                  return (z, (sg'', sf'')) )
                               (sg, sf)
                (AutoStateM gpg@(gg,pg) g sg, AutoArb l s f) ->
                    AutoArbM (liftA2 (\sg' af' -> AutoStateM gpg g sg' . af') gg l)
                             (pg sg *> s)
                             $ \x -> do
                                 let (y, af') = f x
                                 (z, sg') <- g y sg
                                 let ag' = AutoStateM gpg g sg'
                                 return (z, ag' . af')
                (AutoStateM gpg@(gg,pg) g sg, AutoArbM l s f) ->
                    AutoArbM (liftA2 (\sg' af' -> AutoStateM gpg g sg' . af') gg l)
                             (pg sg *> s)
                             $ \x -> do
                                 (y, af') <- f x
                                 (z, sg') <- g y sg
                                 let ag' = AutoStateM gpg g sg'
                                 return (z, ag' . af')
                (AutoArb l s g, AutoFunc f)  ->
                    AutoArb (fmap (. af) l) s (second (. af) . g . f)
                (AutoArb l s g, AutoFuncM f) ->
                    AutoArbM (fmap (. af) l) s (return . second (. af) . g <=< f)
                (AutoArb l s g, AutoState gpf@(gf,pf) f sf) ->
                    AutoArb (liftA2 (\ag' sf' -> ag' . AutoState gpf f sf') l gf)
                            (s *> pf sf)
                            $ \x -> let (y, sf') = f x sf
                                        af'      = AutoState gpf f sf'
                                        (z, ag') = g y
                                    in  (z, ag' . af')
                (AutoArb l s g, AutoStateM gpf@(gf,pf) f sf) ->
                    AutoArbM (liftA2 (\ag' sf' -> ag' . AutoStateM gpf f sf') l gf)
                             (s *> pf sf)
                             $ \x -> do
                                 (y, sf') <- f x sf
                                 let af'      = AutoStateM gpf f sf'
                                     (z, ag') = g y
                                 return (z, ag' . af')
                (AutoArb lg sg g, AutoArb lf sf f) ->
                    AutoArb (liftA2 (.) lg lf)
                            (sg *> sf)
                            $ \x -> let (y, af') = f x
                                        (z, ag') = g y
                                    in  (z, ag' . af')
                (AutoArb lg sg g, AutoArbM lf sf f) ->
                    AutoArbM (liftA2 (.) lg lf)
                             (sg *> sf)
                             $ \x -> do
                                 (y, af') <- f x
                                 let (z, ag') = g y
                                 return (z, ag' . af')
                (AutoArbM l s g, AutoFunc f)  ->
                    AutoArbM (fmap (. af) l)
                             s
                             (liftM (second (. af)) . g . f)
                (AutoArbM l s g, AutoFuncM f) ->
                    AutoArbM (fmap (. af) l)
                             s
                             (liftM (second (. af)) . g <=< f)
                (AutoArbM l s g, AutoState gpf@(gf,pf) f sf) ->
                    AutoArbM (liftA2 (\ag' sf' -> ag' . AutoState gpf f sf') l gf)
                             (s *> pf sf)
                             $ \x -> do
                                 let (y, sf') = f x sf
                                     af'      = AutoState gpf f sf'
                                 (z, ag') <- g y
                                 return (z, ag' . af')
                (AutoArbM l s g, AutoStateM gpf@(gf,pf) f sf) ->
                    AutoArbM (liftA2 (\ag' sf' -> ag' . AutoStateM gpf f sf') l gf)
                             (s *> pf sf)
                             $ \x -> do
                                 (y, sf') <- f x sf
                                 let af' = AutoStateM gpf f sf'
                                 (z, ag') <- g y
                                 return (z, ag' . af')
                (AutoArbM lg sg g, AutoArb lf sf f) ->
                    AutoArbM (liftA2 (.) lg lf)
                             (sg *> sf)
                             $ \x -> do
                                 let (y, af') = f x
                                 (z, ag') <- g y
                                 return (z, ag' . af')
                (AutoArbM lg sg g, AutoArbM lf sf f) ->
                    AutoArbM (liftA2 (.) lg lf)
                             (sg *> sf)
                             $ \x -> do
                                 (y, af') <- f x
                                 (z, ag') <- g y
                                 return (z, ag' . af')
    {-# INLINE (.) #-}

mergeStSt :: (Get s, s -> Put)
          -> (Get s', s' -> Put)
          -> (Get (s, s'), (s, s') -> Put)
mergeStSt (gg, pg) (gf, pf) = (liftA2 (,) gg gf, uncurry (*>) . (pg *** pf))

instance Monad m => Profunctor (Auto m) where
    lmap f = a_
      where
        a_ a = case a of
                 AutoFunc fa         -> AutoFunc (fa . f)
                 AutoFuncM fa        -> AutoFuncM (fa . f)
                 AutoState gpg fa s  -> AutoState gpg (fa . f) s
                 AutoStateM gpg fa s -> AutoStateM gpg (fa . f) s
                 AutoArb l s fa      -> AutoArb (a_ <$> l)
                                                s
                                              $ \x -> let (y, a') = fa (f x)
                                                      in  (y, a_ a')
                 AutoArbM l s fa     -> AutoArbM (a_ <$> l)
                                                 s
                                              $ \x -> do
                                                  (y, a') <- fa (f x)
                                                  return (y, a_ a')
    {-# INLINE lmap #-}
    rmap g = a_
      where
        a_ a = case a of
                 AutoFunc fa         -> AutoFunc (g . fa)
                 AutoFuncM fa        -> AutoFuncM (liftM g . fa)
                 AutoState gpg fa s  -> AutoState gpg (\x -> first g . fa x) s
                 AutoStateM gpg fa s -> AutoStateM gpg (\x -> liftM (first g) . fa x) s
                 AutoArb l s fa      -> AutoArb (a_ <$> l)
                                                s
                                              $ \x -> let (y, a') = fa x
                                                      in  (g y, a_ a')
                 AutoArbM l s fa     -> AutoArbM (a_ <$> l)
                                                 s
                                               $ \x -> do
                                                   (y, a') <- fa x
                                                   return (g y, a_ a')
    {-# INLINE rmap #-}
    dimap f g = a_
      where
        a_ a = case a of
                 AutoFunc fa         -> AutoFunc (g . fa . f)
                 AutoFuncM fa        -> AutoFuncM (liftM g . fa . f)
                 AutoState gpg fa s  -> AutoState gpg (\x -> first g . fa (f x)) s
                 AutoStateM gpg fa s -> AutoStateM gpg (\x -> liftM (first g) . fa (f x)) s
                 AutoArb l s fa      -> AutoArb (a_ <$> l)
                                                s
                                              $ \x -> let (y, a') = fa (f x)
                                                      in  (g y, a_ a')
                 AutoArbM l s fa     -> AutoArbM (a_ <$> l)
                                                 s
                                               $ \x -> do
                                                   (y, a') <- fa (f x)
                                                   return (g y, a_ a')
    {-# INLINE dimap #-}

instance Monad m => Strong (Auto m) where
    first'  = first
    second' = second

instance Monad m => Choice (Auto m) where
    left'  = left
    right' = right

instance MonadFix m => Costrong (Auto m) where
    unfirst = loop

instance Monad m => Arrow (Auto m) where
    arr     = mkFunc
    first a = case a of
                AutoFunc f         ->
                    AutoFunc (first f)
                AutoFuncM f        ->
                    AutoFuncM (firstM f)
                AutoState gp fa s  ->
                    AutoState gp (\(x, z) -> first (,z) . fa x) s
                AutoStateM gp fa s ->
                    AutoStateM gp (\(x, z) -> liftM (first (,z)) . fa x) s
                AutoArb l s f      ->
                    AutoArb (first <$> l)
                            s
                          $ \(x, z) -> let (y, a') = f x
                                       in  ((y, z), first a')
                AutoArbM l s f     ->
                    AutoArbM (first <$> l)
                             s
                           $ \(x, z) -> do
                               (y, a') <- f x
                               return ((y, z), first a')
    second a = case a of
                 AutoFunc f         ->
                     AutoFunc (second f)
                 AutoFuncM f        ->
                     AutoFuncM (secondM f)
                 AutoState gp fa s  ->
                     AutoState gp (\(z, x) -> first (z,) . fa x) s
                 AutoStateM gp fa s ->
                     AutoStateM gp (\(z, x) -> liftM (first (z,)) . fa x) s
                 AutoArb l s f      ->
                     AutoArb (second <$> l)
                             s
                           $ \(z, x) -> let (y, a') = f x
                                        in  ((z, y), second a')
                 AutoArbM l s f     ->
                     AutoArbM (second <$> l)
                              s
                            $ \(z, x) -> do
                                (y, a') <- f x
                                return ((z, y), second a')

instance Monad m => ArrowChoice (Auto m) where
    left a0 = a
      where
        a = case a0 of
              AutoFunc f        ->
                  AutoFunc (left f)
              AutoFuncM f       ->
                  AutoFuncM (\x -> case x of
                               Right y -> return (Right y)
                               Left y  -> liftM Left (f y))
              AutoState gp f s  ->
                  AutoState gp (\x s' -> case x of
                                  Right y -> (Right y, s')
                                  Left y  -> first Left (f y s')
                               ) s
              AutoStateM gp f s ->
                  AutoStateM gp (\x s' -> case x of
                                   Right y -> return (Right y, s')
                                   Left y  -> liftM (first Left) (f y s')
                                ) s
              AutoArb l s f     ->
                  AutoArb (left <$> l)
                          s
                        $ \x -> case x of
                                  Right y -> (Right y, a)
                                  Left y  -> (Left *** left) (f y)
              AutoArbM l s f    ->
                  AutoArbM (left <$> l)
                           s
                         $ \x -> case x of
                                   Right y -> return (Right y, a)
                                   Left y  -> liftM (Left *** left) (f y)
    {-# INLINE left #-}
    right a0 = a
      where
        a = case a0 of
              AutoFunc f ->
                  AutoFunc (fmap f)
              AutoFuncM f ->
                  AutoFuncM (sequence . fmap f)
              AutoState gp f s  ->
                  AutoState gp (\x s' -> case x of
                                  Left y  -> (Left y, s')
                                  Right y -> first Right (f y s')
                               ) s
              AutoStateM gp f s ->
                  AutoStateM gp (\x s' -> case x of
                                   Left y  -> return (Left y, s')
                                   Right y -> liftM (first Right) (f y s')
                                ) s
              AutoArb l s f     ->
                  AutoArb (right <$> l)
                          s
                        $ \x -> case x of
                                  Left y  -> (Left y, a)
                                  Right y -> (Right *** right) (f y)
              AutoArbM l s f    ->
                  AutoArbM (right <$> l)
                           s
                         $ \x -> case x of
                                   Left y  -> return (Left y, a)
                                   Right y -> liftM (Right *** right) (f y)
    {-# INLINE right #-}

instance MonadFix m => ArrowLoop (Auto m) where
    loop a = case a of
                AutoFunc f        ->
                    AutoFunc (\x -> fst . fix $ \ ~(_, d) -> f (x, d))
                AutoFuncM f       ->
                    AutoFuncM (\x -> liftM fst . mfix $ \ ~(_, d) -> f (x, d))
                AutoState gp f s  ->
                    AutoState gp (\x s' -> first fst . fix $ \ ~(~(_, d), _) -> f (x, d) s') s
                AutoStateM gp f s ->
                    AutoStateM gp (\x s' -> liftM (first fst) . mfix $ \ ~(~(_, d), _) -> f (x, d) s') s
                AutoArb l s f     ->
                    AutoArb (loop <$> l)
                            s
                          $ \x -> (fst *** loop)
                                . fix
                                $ \ ~(~(_, d), _) -> f (x, d)
                AutoArbM l s f    ->
                    AutoArbM (loop <$> l)
                             s
                           $ \x -> liftM (fst *** loop)
                                 . mfix
                                 $ \ ~(~(_, d), _) -> f (x, d)
    {-# INLINE loop #-}

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
    negate      = fmap negate
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance (Monad m, Fractional b) => Fractional (Auto m a b) where
    (/)          = liftA2 (/)
    recip        = fmap recip
    fromRational = pure . fromRational

instance (Monad m, Floating b) => Floating (Auto m a b) where
    pi      = pure pi
    exp     = fmap exp
    sqrt    = fmap sqrt
    log     = fmap log
    (**)    = liftA2 (**)
    logBase = liftA2 logBase
    sin     = fmap sin
    tan     = fmap tan
    cos     = fmap cos
    asin    = fmap asin
    atan    = fmap atan
    acos    = fmap acos
    sinh    = fmap sinh
    tanh    = fmap tanh
    cosh    = fmap cosh
    asinh   = fmap asinh
    atanh   = fmap atanh
    acosh   = fmap acosh


-- Utility functions

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f ~(x, y) = liftM (, y) (f x)
{-# INLINE firstM #-}

secondM :: Monad m => (a -> m b) -> (c, a) -> m (c, b)
secondM f ~(x, y) = liftM (x,) (f y)
{-# INLINE secondM #-}
