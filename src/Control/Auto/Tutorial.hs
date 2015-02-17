-- |
-- Module      : Control.Auto.Tutorial
-- Description : Tutorial for getting started with Auto
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : stable
-- Portability : portable
--
-- Welcome to the tutorial module for getting started with Auto!
--
-- This is actually just a basic overview of the library and some basic
-- programs, enough to get started, hopefully; for further information,
-- check out <https://github.com/mstksg/auto-examples auto-examples> for
-- more real-world examples, and some of my writeups on
-- <http://blog.jle.im my blog>.
--

module Control.Auto.Tutorial (
  -- * Auto
  -- $auto
  ) where

-- $auto
--
-- An 'Auto' is like a stateful function.  You can feed it values, and
-- it'll return an output and a "next 'Auto'".
--
-- >>> let Output x a2 = stepAuto' (sumFrom 0) 5
-- >>> x
-- 5
--
-- Here, we just stepped the @'sumFrom' 0@ 'Auto', feeding it with an input
-- of 5.  The result (@x@) is the running total of the inputs --- in this
-- case, 5.  Neat!  We named our "next 'Auto'" @a2@.  Let's try it again:
--
-- >>> let Output y _ = stepAuto' a2 3
-- >>> y
-- 8
--
-- We feed now an 3 to our next 'Auto', @a2@, and get that the result
-- @y@ is now 8!
--
-- Now, normally we don't really often step our 'Auto's manually...if we
-- have an input stream (like a list), we can streamingly run them all
-- through our 'Auto's one at a time, for example:
--
-- >>> take 10 $ streamAuto' (sumFrom 0) [1..]
-- [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
--
-- Or even make it run in an IO loop:
--
-- >>> interact (Just <$> mappender)
-- > hello
-- hello
-- > world
-- helloworld
-- > goodbye
-- helloworldgoodbye
--
-- ('mappender' is the 'Auto' that is much like 'sumFrom', but it returns
-- the 'mconcat' of all of its inputs so far; we fmap 'Just'
-- because as soon as 'interact' sees a 'Nothing', it stops looping)
--
-- 'Auto's can also be effectful, because why not?
--
-- >>> streamAuto (arrM print) [1..5]
-- 1
-- 2
-- 3
-- 4
-- 5
-- >>> flip runState 10 (streamAuto (arrM (\x -> modify (+x))) [1..5])
--
--





