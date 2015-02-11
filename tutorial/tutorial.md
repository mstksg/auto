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
case, 5.  Neat!  We named our "next `Auto`" `a2`.  Let's try it again:

~~~haskell
-- a2 :: Auto' Int Int
ghci> let Output y _ = stepAuto' a2 3
ghci> y
8
~~~

We feed now an 3 to our next `Auto`, `a2`, and get that the result
`y` is now 8!

Now, normally we don't really often step our `Auto`s manually...if we
have an input stream (like a list), we can streamingly run them all
through our `Auto`s one at a time, for example:

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

(`mappender` is the `Auto` that is much like `sumFrom`, but it returns
the `mconcat` of all of its inputs so far; we fmap `Just`
because as soon as `interact` sees a `Nothing`, it stops looping)

`Auto`s can also be effectful, because why not?

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

### What's in a type?

The most important type in this library is `Auto m a b` (well, also its
variant, `Auto' a b`).

An `Auto' a b` is, conceptually, in english, read as an *auto* (stateful
function) that *turns `a`s into `b`s* (takes `a`s, returns `b`s).  Feed in
`a`s, `b`s come out.

An `Auto m a b` is like an `Auto' a b`, except the process of popping the `b`
out (and updating the internal state of the `Auto`) happens in the context of
a `Monad` `m`.  (Note that `Auto' a b` is just `Auto Identity a b`).

So, if `Auto'` is a fancy internally-stateful function `a -> b`, `Auto m a b`
is a fancy internally-stateful function `a -> m b`.

For the most part, your things will be fine with just `Auto'`...however, there
are useful `Auto`s with effects, so it's best to leave your functions
parameterized over `Monad m => Auto m a b`, so you can use them as both an
`Auto' a b` *and* as an `Auto m a b`, if you wanted to, without any explicit
conversions.

Another important type you see is `Output m a b`, which is just a glorified
tuple with a `b` and an `Auto m a b`.  As you can see above, when you "step"
an `Auto` (feed it an `a` manually), you get, as a result, a `b` and a "next
`Auto`".  An `Output` type is a tuple containing these two.  There is also an
`Output' a b`, which you get when you step an `Auto' a b`, which contains a
`b` and a next `Auto'`.

~~~haskell
data Output m a b = Output { a :: outRes, Auto m a b :: outAuto }

type Output' = Output Identity
~~~

There are also two more types, `Interval` (a type synonym) and `Blip`, that
will show up later.

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
foo :: Auto' Int (Int, Maybe Int)
foo = proc x -> do
    sumX     <- sumFrom 0          -< x
    prodX    <- productFrom 1      -< x + sumX
    lastEven <- hold . emitOn even -< x
    id -< (prodX, lastEven)
~~~

~~~haskell
ghci> streamAuto' foo [4,7,3,6,5,1]
[ (    4, Just 4), (    144, Just 4), (    2448, Just 4)
, (63648, Just 6), (1909440, Just 6), (51554880, Just 6) ]
~~~

`emitOn even` produces a "Blip" stream that emits whenever the input (`x`) is
even, and `hold` is a `Maybe` that holds the value of the most recent received
emitted value.

Most of what was just done could be written with the `Applicative`
instance as well, but seeing it laid out almost like a dependency graph yields
powerful expressiveness.

The syntax for proc blocks is that each line is of the form:

~~~haskell
auto -< input
~~~

And, if you want to "bind" and name the result for later:

~~~haskell
output <- auto -< intput
~~~

Kind of like a little ASCII art arrow!  Cute, huh?

The result of the entire block is the output of the final line, just like in
monadic do blocks.

You can actually get pretty fancy with `proc` blocks, and use conditions and
even recursive bindings:

~~~haskell
foo :: Auto' Int (Int, String)
foo = proc goal -> do
    rec let goUp = curr < goal
        curr <- sumFromD 0 -< if goUp
                                then 4
                                else -1
    mesg <- if goUp
              then
                id -< "went up from " ++ show curr
              else do
                numUps <- sumFrom 0 -< 1 :: Int
                id -< "went down, #" ++ show numUps

    id -< (curr, mesg)
~~~

~~~haskell
ghci> streamAuto' foo (replicate 10 6)
[ (0, "went up from 0"), (4, "went up from 4"), (8, "went down, #1")
, (7, "went down, #2"), (6, "went down, #3"), (5, "went up from 5") ]
~~~

`sumFromD` is like `sumFrom`, but always outputs the accumulator *before
adding the input*, isntead of after adding it; sort of like `c++` instead of
`++c`.  Put in another way, it ouputs a running sum excluding for the most
recent input.

What happens here?  Well, the auto receives an input --- a "goal number" that
it tries to get `curr` to.  If `curr` is too low, then it is increased by 4;
if it is too high, it is decreased by 1.  Note that `curr`'s increase/decrease
depends on `goUp`, and `goUp` depends on `curr`, so we have a cyclic
relationship.  Luckily, the library handles this for us.

There's also a message that gets popped out too; if the thing is to be
increased, then output a mesasge "went up to"; if it decreased, output "went
down", and keep track of how many times it has been decreased (using `sumFrom
0`), and output that.

And that's proc notation, and the Arrow instance!


