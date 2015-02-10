Auto
====

Welcome to the tutorial for getting started with Auto!

This is actually just a basic overview of the library and some basic programs,
enough to get started, hopefully; for further information, check out
[auto-examples][] for more real-world examples, and some of my writeups on [my
blog][blog].

[auto-examples]: https://github.com/mstksg/auto-examples
[blog]: http://blog.jle.im

Auto
----

An `Auto` is like a stateful function.  You can feed it values, and
it'll return an output and a "next Auto".

~~~haskell
-- sumFrom 0 :: Auto' Int Int
-- ("function" from Int to Int)
ghci> let Output x a2 = stepAuto' (sumFrom 0) 5
ghci> x
5
~~~

Here, we just stepped the `sumFrom 0` Auto, feeding it with an input
of 5.  The result (`x`) is the running total of the inputs --- in this
case, 5.  Neat!  We named our "next 'Auto'" `a2`.  Let's try it again:

~~~haskell
-- a2 :: Auto' Int Int
ghci> let Output y _ = stepAuto' a2 3
ghci> y
8
~~~haskell

We feed now an 3 to our next 'Auto', `a2`, and get that the result
`y` is now 8!

Now, normally we don't really often step our 'Auto's manually...if we
have an input stream (like a list), we can streamingly run them all
through our 'Auto's one at a time, for example:

~~~haskell
ghci> take 10 $ streamAuto' (sumFrom 0) [1..]
[1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
~~~

Or even make it run in an IO loop:

~~~haskell
-- mappender          :: Auto' String String
-- Just <$> mappender :: Auto' String (Maybe String)
ghci> interact (Just <$> mappender)
> hello
hello
> world
helloworld
> goodbye
helloworldgoodbye
~~~

('mappender' is the 'Auto' that is much like 'sumFrom', but it returns
the 'mconcat' of all of its inputs so far; we fmap 'Just'
because as soon as 'interact' sees a 'Nothing', it stops looping)

'Auto's can also be effectful, because why not?

~~~haskell
-- print      :: Show a => a -> IO ()
-- arrM print :: Show a => Auto IO a ()
-- streamAuto (arrM print) :: IO [()]
ghci> streamAuto (arrM print) [1..5]
1
2
3
4
5
~~~

Composing and creating
----------------------

`Auto`s are useful because complex `Auto`s with complex "internal state" can
be created through various interfaces.

### Functor, Profunctor

The `Functor` interface allows you to apply normal functions on the output of
an `Auto`:

~~~haskell
ghci> streamAuto' (sumFrom 0) [1..5]
[1,3,6,10,15]
ghci> streamAuto' (show <$> sumFrom 0) [1..5]
["1","3","6","10","15"]
~~~

The `Profunctor` interface gives the same power, and also allows you to apply
functions "before" the input of an `Auto`:

~~~haskell
ghci> let repln x = replicate x x
ghci> streamAuto' (lmap repln mappender) [5,2,3]
[[5,5,5,5,5], [5,5,5,5,5,2,2], [5,5,5,5,5,2,2,3,3,3]]
ghci> streamAuto' (dimap repln (concatMap show) mappender) [5,2,3]
["55555", "5555522", "5555522333"]
~~~

### Applicative

The `Applicative` interface allows you create `Auto`s that "always produce" a
certain value:

~~~haskell
ghci> take 10 $ streamAuto' (pure 4) [1..]
[4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
~~~

It also allows you to "fork" an input to two `Auto`s and then combine their
outputs later:

~~~haskell
ghci> streamAuto' (sumFrom 0) [1..5]
[ 1, 3,  6, 10,  15]
ghci> streamAuto' (productFrom 1) [1..5]
[ 1, 2,  6, 24, 120]
ghci> streamAuto' (liftA2 (+) (sumFrom 0) (productFrom 1)) [1..5]
[ 2, 5, 12, 34, 135]
~~~

### Category

The `Category` instance lets you compose `Auto`s by feeding their inputs one
through the other, updating their states together.

~~~haskell
ghci> streamAuto' (sumFrom 0) [1..5]
[1,3,6,10,15]
ghci> streamAuto' (productFrom 1) [1,3,6,10,15]
[1,3,18,180,2700]
ghci> streamAuto' (productFrom 1 . sumFrom 0) [1..5]
[1,3,18,180,2700]
~~~

It also gives you an identity to this composition, `id`:

~~~haskell
ghci> streamAuto' id [1..5]
[1,2,3,4,5]
ghci> streamAuto' (productFrom 1 . id . sumFrom 0) [1..5]
[1,3,18,180,2700]
~~~

(Remember to use `(.)` and `id` from `Control.Category`)

### Arrow

The Arrow instance gives you (among other things) a way to turn "pure" `a -> b`
functions into `Auto m a b`'s, Autos from `a` to `b`:

~~~haskell
ghci> streamAuto' (arr (*2)) [1..5]
[2,4,6,8,10]
~~~

But most importantly, it gives you the ability to compose complex Autos using
*proc* notation:

~~~haskell
~~~







