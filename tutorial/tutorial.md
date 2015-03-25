Auto
====

Welcome to the tutorial for getting started with Auto!

This is actually just a basic overview of the library and some basic programs,
enough to get started, hopefully; for further information, check out
[auto-examples][] for more real-world examples, and some of my writeups on [my
blog][blog].  Up-to-date documentation is, at the moment, hosted [on
github][docs]...and the latest version of this tutorial itself can be found on
[the development branch][tutorial], normally!

[auto-examples]: https://github.com/mstksg/auto-examples
[blog]: http://blog.jle.im
[docs]: https://mstksg.github.io/auto/
[tutorial]: https://github.com/mstksg/auto/blob/develop/tutorial/tutorial.md

Auto
----

Before we start, let's remember our imports!

~~~haskell
import Control.Auto                 -- the main entry point
import Prelude hiding ((.), id)     -- we use generalized versions from
                                    -- Control.Category, so we have to hide
                                    -- these.
~~~

### Semantic Picture

Semantically, a `Auto` describes *a relationship* between an input and an
output that is preserved over multiple steps.

In a way, you can think about `Auto`s as *stream transformers*.  A stream of
sequential inputs come in one at a time, and a stream of outputs pop out one
at a time as well.  You can think of `streamAuto'` as taking an `Auto' a b`
and "unwrapping" its internal `[a] -> [b]`.

An `Auto` is a relationship; the simplest relationship is probably a straight
up apply-a-function-to-each-input-to-get-each-output relationship.  For that,
check out the `Auto` `arr (*2)`, where the outputs are the doubles of the
inputs:

~~~haskell
-- streamAuto' :: Auto' a b -> [a] -> [b]
-- [ 1, 2, 3, 4, 5, 6, 7, 8, 9,10...        -- the inputs
ghci> take 10 $ streamAuto' (arr (*2)) [1..]
   [ 2, 4, 6, 8,10,12,14,16,18,20]          -- the outputs
~~~

`streamAuto' (arr f)` is just `map f`, as you can see!

In general, the input-output relationship is allowed to depend on the history
of the inputs, as well.  For example, we have the `Auto` `sumFrom 0` --- the
relationship is that the output is always the cumulative sum of the inputs
received so far:

~~~haskell
-- [ 1, 2, 3, 4, 5, 6, 7, 8, 9,10...        -- the inputs
ghci> take 10 $ streamAuto' (sumFrom 0) [1..]
   [ 1, 3, 6,10,15,21,28,36,45,55]          -- the outputs
~~~

A bit on types --- `sumFrom n` is `Num a => Auto m a a` ... or, if
specializing it helps, `Auto' Int Int`.  You can read this as "a relationship
between two `Int`s fixed over the stream", or "a one-by-one mapping of an
`Int` stream to another `Int` stream".  For `sumFrom n`, the relationship is
that the output is always the cumulative sum of the inputs.

Note that these relationships are always *causual*; the nth item of the output
can only depend on the first n items of the input.  We say that they are
"real-time" stream transformers in that every time you get an input, exactly
one output pops out.

That's what they are semantically, and an `Auto` denotes exactly such an
input-output relationship that is maintained over several steps.

### Operational picture

Operationally, an `Auto` does this by acting as a "stateful function" that we
can "run" with `stepAuto`.  A function with "internal state".

~~~haskell
-- stepAuto' :: Auto' a b -> a -> (b, Auto' a b)
ghci> let (x, nextAuto ) = stepAuto' (sumFrom 0) 5
ghci> x
5
ghci> let (y, nextAuto2) = stepAuto' nextAuto 3
ghci> y
8
ghci> evalAuto' nextAuto2 4
12
~~~

`stepAuto'` lets you take an `Auto' a b`, give it an `a` as an input, and
returns an `b` as the output, and a "next/updated `Auto'`", which is the
`Auto'` with an updated internal state.  Running the "next `Auto'`" given will
continue along with the new updated state.  (`evalAuto'` is like `stepAuto'`
but throws away the "next `Auto`")   In this case, the "internal state" is an
accumulator, the sum of all received elements so far.

In practice, this is usually going to be your "main loop", or "game loop":

1.  Collect input from the world (using IO, or whatever you need)
2.  Step the `Auto` you have with that input.
3.  Get the output from that `Auto`, and the next `Auto`
4.  Show or render your output to the world however you want.
5.  Repeat all again, but with the new `Auto` from step 3.

(If your program doesn't need any outside input, then you can just use
`stepAutoN` with `()`, or `streamAuto'` with an infinite list.)

There are some built-in "loops" like this in the *[Control.Auto.Run][]*
module, for running in `IO` by reading and showing inputs and ouputs
(`interactAuto`, `interactRS`) if you want to try these out!

What's in a type?
-----------------

Enough handwaving!  What do these types even mean?  What are the type
parameters?

An `Auto' a b` describes *a relationship* between a stream of inputs `a` and a
stream of outputs `b` that is maintained over several steps of inputs.

One way to look at it is that, with `streamAuto'` an `Auto' a b` gives you the
"unwrapped" `[a] -> [b]`.

From an operational perspective, you can think of an `Auto' a b` as a function
with internal state that, when fed an `a`, gives you a `b` and a "next/updated
`Auto`".  With `stepAuto'`, an `Auto' a b` gives you an `a -> (b, Auto' a b)`.
An `Auto' a b` is basically a `a -> b` with "internal state".

The more general type is actually `Auto m a b` --- an `Auto' a b` is actaully
just a type alias for `Auto Identity a b`.

An `Auto m a b` describes *a relationship*, again, between a stream of inputs
`a` and a stream of outputs `b` maintained over several steps of inputs...and
maintains this relationship with within an underlying monadic context `m`.

If `streamAuto'` from an `Auto' a b` gives you an `[a] -> [b]`, then
`streamAuto` from an `Auto m a b` gives you the "unwrapped" `[a] -> m [b]`.

Operationally, if `Auto' a b` is a `a -> b` with internal state, then `Auto m
a b` is a `a -> m b` with internal state.  If you feed it an `a`, it'll return
a `b` and a "next/updated `Auto`" in a monadic context --- with `stepAuto`,
you get a `a -> m (b, Auto m a b)`.

This monadic context means that in the process of "stepping" or "running" the
`Auto`, you can perform effects and get input from an outside world.

For the most part, real-life `Auto`s will be written parameterized over
`Monad` or some `Monad`-based typeclass:

~~~haskell
myAuto :: Monad m => Auto m Int Bool
~~~

Working with `Monad m => Auto m a b` is practically identical to working with
`Auto' a b`, so there really isn't ever a real point to actually *write* an
`Auto'`.  However, specializing to `Auto'` lets us use simple "running"
functions like `streamAuto'` and `stepAuto'`.

While we're on the subject, there is another type alias for `Auto`s: an
`Interval m a b` is an `Auto m a (Maybe b)` (they're just type aliases).
Semantically, it represents an `Auto` that is "on" or "off" for durations of
steps.  Similarly, `Interval' a b` is an `Auto' a (Maybe b)`.  You get the
picture, I hope!  We'll learn more about `Interval` later.

Building up Autos
-----------------

So of course, having simple `Auto`s like this being your whole program isn't
very reasonable...do you think I have a `chatBot` or `chessEngine` `Auto` in
the library? :)

The "magic" of this library is that you have the ability to build up complex
and intricate relationships and behaviors (and programs) by composing small
"primitive" `Auto`s.  These combinators are exposed both through familiar
typeclasses we know and love, and also through functions in this library.

### Modifying and combining `Auto`s

For example, with the `Functor` instance, you can apply functions to the
"output" of an `Auto`:

~~~haskell
ghci> streamAuto' (sumFrom 0) [1..5]
[ 1 , 3 , 6 , 10 , 15 ]
ghci> streamAuto' (show <$> sumFrom 0) [1..5]
["1","3","6","10","15"]
~~~

`lmap` from `Profunctor` lets you apply functions "before" the input of the
`Auto`:

~~~haskell
-- mappender :: Monoid m => Auto' m m
ghci> streamAuto' mappender ["1","2","3"]
["1","12","123"]
ghci> streamAuto' (lmap show mappender) [1,2,3]
["1","12","123"]
~~~

(`mappender` is an `Auto` where the output is always the cumulative `mconcat`
of all of the inputs so far)

The `Applicative` instance gives you a "constant `Auto`", which ignores its
input and whose output is always a constant value:

~~~haskell
ghci> take 10 $ streamAuto' (pure 4) [1..]
[4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
~~~

The `Applicative` instance also gives you the ability to "fork" the input
streams of two `Auto`s and then re-combine their output streams later:

~~~haskell
ghci> streamAuto' (sumFrom 0) [1..5]
[ 1, 3,  6, 10,  15]
ghci> streamAuto' (productFrom 1) [1..5]
[ 1, 2,  6, 24, 120]
ghci> streamAuto' (liftA2 (+) (sumFrom 0) (productFrom 1)) [1..5]
[ 2, 5, 12, 34, 135]
~~~

You can also "fork" an input stream to two `Auto`s, and then throw away the
output stream of one: (very useful for `Auto`s like `effect`, which we will
see later, where we only care about the monadic effects and not about the
actual output stream)

~~~haskell
ghci> streamAuto' (sumFrom 0 *> productFrom 1) [1..5]
[ 1, 2,  6, 24, 120]
ghci> streamAuto' (sumFrom 0 <* productFrom 1) [1..5]
[ 1, 3,  6, 10,  15]
~~~

Heck, you can even `sequenceA` several!

~~~haskell
sequenceA :: [Auto m a b] -> Auto m a [b]
~~~

It will take a list of `Auto`s and return an `Auto` that "forks" the input
stream into *all* of the original `Auto`s and aggregates together all of the
output streams.  A multi-way fork.

We also have the Applicative-derived instances like `Monoid`, so any `Auto m
a b` is a `Monoid` if `b` is a `Monoid`.

~~~haskell
mconcat :: Monoid b => [Auto m a b] -> Auto m a b
~~~

A lot of times you'll have a lot of things handling the same input in
different ways, and you'll want to recombine them all at the end.  Well,
`mconcat`, `sequence`, etc. are at your service!

This is the principle of "[scalable program architectures][spa]" at work!
The `mappend` of two `Auto`s is...another `Auto`!

[spa]: http://www.haskellforall.com/2014/04/scalable-program-architectures.html

Of course there the Applicative-derived `Num` (and assorted numerical
instances) too:

~~~haskell
ghci> streamAuto' (0 * sumFrom 0) [1..5]
[0, 0, 0, 0, 0,]
ghci> streamAuto' (negate (sumFrom 0)) [1..5]
[-1, -3, -6, -10, -15]
ghci> streamAuto' (10 + sumFrom 0) [1..5]
[11, 13, 16, 20, 25]
ghci> streamAuto' (sumFrom 0 + productFrom 1) [1..5]
[ 2, 5, 12, 34, 135]
~~~

Just don't go too crazy with these, okay?

Now, the `Category` instance is probably the most powerful tool at your
disposal.  As a first treat, it gives you `id :: Auto m a a`, an `Auto` whose
output is always exactly the corresponding input.

But more importantly, you can "chain together" `Auto`s end-to-end.  Compose
them as if they were functions.

You know how an `Auto` takes a stream and outputs a stream?  Well,
"chaining"/"composing" two `Auto`s will "pipe together" the streams.  `a2 .
a1` will be a new `Auto` that runs an input stream through both `a1` and `a2`.

~~~haskell
ghci> streamAuto' (sumFrom 0) [1..5]
[1,3,6,10,15]
ghci> streamAuto' (productFrom 1) [1,3,6,10,15]
[1,3,18,180,2700]
ghci> streamAuto' (productFrom 1 . sumFrom 0) [1..5]
[1,3,18,180,2700]
~~~

`sumFrom 0`'s output stream is the cumulative sum of the input stream.
`productFrom 1`'s output stream is the cumulative product of the input stream.
So their chaining/piping/composition is the cumulative product of the
cumulative sum.

~~~haskell
(.) :: Auto m b c -> Auto m a b -> Auto m a c
~~~

If you imagine an `Auto'` as an `[a] -> [b]`, then you can think of this as
"composing" the `[a] -> [b]` functions:

~~~haskell
-- streamAuto' gives us an [Int] -> [Int], so we can compose them using normal
-- function composition:
ghci> streamAuto' (productFrom 1) . streamAuto' (sumFrom 0) $ [1..5]
[1,3,18,180,2700]
-- composing `Auto`s is like composing their resulting `[a] -> [b]`s
ghci> streamAuto' (productFrom 1 . sumFrom 0) $ [1..5]
[1,3,18,180,2700]
~~~

(Math nuts might recognize this as saying that `streamAuto'` is a "category
homomorphism"...aka, a functr :)  Seeing that `streamAuto' (id :: Auto' a a)
== (id :: [a] -> [a])`, of course!)

Operationally, at every "step", it passes in each input to the first `Auto`,
and gets the output of that and passes it into the second `Auto`, and uses the
output of the second `Auto` as the result, updating *each* internal state.

Another example, here we have an `Auto` that takes an input stream and and
outputs a `Blip` stream (more on that later) that emits whenever there is a
multiple of 5:

~~~haskell
       -- emitOn5s :: Auto' Int (Blip Int)
ghci> let emitOn5s = emitOn (\x -> x `mod` 5 == 0)
ghci> streamAuto' emitOn5s [1,5,9,3,10,2]
[NoBlip, Blip 5, NoBlip, NoBlip, Blip 10, NoBlip]
ghci> streamAuto' (hold . emitOn5s) [1,5,9,3,10,2]
[Nothing, Just 5, Just 5, Just 5, Just 10, Just 10]
~~~

`hold :: Auto' (Blip a) (Maybe a)` takes a stream of `Blip`s and returns a
stream that is `Maybe a`, where it is `Nothing` until the first emitted `Blip`
value, and `Just x` as the last received `Blip` value.

So here, we "chain" `hold` onto `emitOn5s`.  `emitOn5s` emits on everything
that is a multiple of `5`, and `hold` "holds on" to all of the emitted values.
Neat!

This can be used in conjunction with the `Applicative` instance for great
power.  In the end, your programs will really just be `(.)`-composed `Auto`s
with forks and re-cominings from `Applicative` and `Arrow` methods.

Speaking of `Arrow`, we also have a neat interface exposed by `Arrow`,
`ArrowPlus`, and `ArrowLoop`.  First of all, we get `arr :: (a -> b) -> Auto m
a b`, which basically an `Auto` that is a constant, pure function (the output
is the corresponding input applied to the given function).  We get the ability
to make an `Auto` run on "only the first item in a tuple" (`first`), or "only
`Left`s that come in" (`left`).  Also, we get proc notation!

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

Most of what was just done could be written with the `Applicative`
instance as well...but in this way, the entire thing looks a lot like a
dependency graph, and it's pretty expressive and powerful.

#### Brief Primer on Proc Notation

An explanation on the syntax; when you see:

~~~haskell
sumX <- sumFrom 0 -< x
~~~

This reads as you are defining a binding `sumX`, and *the relationship between
sumX and x* is that `sumX` is the *cumulative sum* of `x`.

(from the first line, `foo = proc x -> do`, `x` is the input of the entire
`Auto`)

When we see:

~~~haskell
prodX <- productFrom 1 -< x + sumX
~~~

This reads as you are defining a binding `prodX`, and `prodX` is maintained as
the cumulative product of `x + sumX`.

The result of the last line of the proc block is the result of the entire
block:

~~~haskell
id -< (prodX, lastEven)
~~~

Means that the output of the entire block is just echoing the tuple `(prodX,
lastEven)`.

(Operationally, you can imagine that, at every step, `x` is "fed into"
`sumFrom 0`, and the result is named `sumX`; `x + sumX` is "fed into"
`productFrom 1`, etc.)

The power here is that it really reads like a straight-up dependency graph...a
graph of relationships to names.  Lay out your relationships explicitly and
declaratively, and the library takes care of the rest!  The semantic model of
an `Auto` representing a maintained relationship is made very clear in `proc`
notation.

Later on you can see that `proc` blocks can be pretty expressive --- using
if/then's and case statements, and also recursive bindings (so you can even
declare recursive graphs of concepts, and the library will figure out how to
solve it for you).

By the way, there are some "scoping" issues to be aware of.  Remember that
proc more or less builds a graph of relationships between values using `Auto`s
at compile-time; the whole graph and chaining-together-of-`Auto`s is done at
compile time.  So, the `Auto`s themselves have to be known at compile time.
We can't do someothing like this:

~~~haskell
foo :: Auto' Int Int
foo = proc x -> do
    y <- productFrom 1 -< x
    z <- sumFrom y     -< x
    id -< y + z
~~~

We can't do `sumFrom y`, because `y` is not an actual value that we have at
"compile"/"building" time.  `y` is what we're calling the result of
`productFrom 1`, at every step, so its value changes at every step, and every
`Auto` has to be a **fixed `Auto`**.  Remember, `Auto` relationships are
"forever" and fixed, declarative style.  So the `Auto` where `sumFrom` is,
there, has to be a fixed thing that doesn't change at every step...but `y` is
a value that will very as the stream marches on.

You can however do something like:

~~~haskell
bar :: Int -> Auto' Int Int
bar x0 = proc x -> do
    y <- productFrom 1 -< x
    z <- sumFrom x0    -< x
    id -< y + z
~~~

Because when we are "building" `bar x0`, we *have* `x0`!  It'll be `sumFrom
x0`, forever!

### Anyways!

Anyways!  Those are the primary typeclass based interfaces; explore the
library for more!

### From scratch

If you have to, when creating `Auto`s from scratch, we have:

~~~haskell
pure   :: b          -> Auto m a b
effect :: m b        -> Auto m a b
arr    :: (a -> b)   -> Auto m a b
arrM   :: (a -> m b) -> Auto m a b
~~~

`pure` and `effect` give you "constant-producing `Auto`"s that ignore their
input; `pure x` is an `Auto` that ignores its input and always outputs `x`.
`effect m` is an `Auto` that ignores its input and executes/sequences `m` at
every "step", and outputs the result at every step.  `arr` is an `Auto` that
maps every input to an output by running a pure function, and `arrM` is an
`Auto` that does the same but with a "monadic" function.

Here is a handy little summary!

~~~haskell
streamAuto' (pure x)  == map (const x)
streamAuto (effect m) == mapM (const m)
streamAuto' (arr f)   == map f
streamauto (arrM f)   == mapM f
~~~

None of these `Auto`s have "internal state"; however, we can make our own
internally stateful `Auto`s from scratch:

~~~haskell
iterator  :: (b -> b)             -> b -> Auto m a b
iteratorM :: (b -> m b)           -> b -> Auto m a b
accum     :: (b -> a -> b)        -> b -> Auto m a b
accumM    :: (b -> a -> m b)      -> b -> Auto m a b
mkState   :: (a -> s -> (b, s))   -> s -> Auto m a b
mkStateM  :: (a -> s -> m (b, s)) -> s -> Auto m a b
mkAuto_   :: (a -> (b, Auto m a b))    -> Auto m a b
mkAutoM_  :: (a -> m (b, Auto m a b))  -> Auto m a b
~~~

You can look at the documentation for all of these, but these all basically
work with "internal state" --- `iterator` ignores its input and repeatedly
applies a function to a value and pops it out at every step.  `accum`
maintains that the *output* is always the result of "folding together" (a la
`foldl`) all of the inputs so far, with a starting value.  `mkState` is
like a more powerful `accum`, which keeps an internal state that is updated
at every step.  `mkAuto_` lets you describe an `Auto` by its behavior under
`stepAuto'`.

~~~haskell
ghci> take 10 $ streamAuto' (iterator (+1) 0) (repeat ())
[0,1,2,3,4,5,6,7,8,9]
ghci> take 10 $ streamAuto' (accum (+) 0)   [1..]
[1,3,6,10,15,21,28,36,45,55]
~~~

It is recommended to only use `accum`, `mkState`, `mkAuto` only when
absolutely necessary; usually you can make what you want from combining
smaller, simple, pre-made `Auto`s.  But sometimes the case does arrive.

The Big Picture
---------------

So, at this point, let's look at the "big picture".  A program written with
`Auto` will involve, at every "step", gathering input, feeding into the
"master program `Auto`", getting the output, rendering it somehow, and
repeating.  But how do we build our `Auto`?  What is the advantage of using
`Auto` instead of `State`, etc.?

`Auto` lets you compose little meaning-bits into more complex meaning bits, by
specifying *invariant relationships* between *items of streams*.  These are
"forever-relationships" --- they don't just describe step-by-step, iterative,
stateful actions --- they describe invariant relationships.  And you can
create your own by composing, modifying, chaining, etc. all of the primitives.

Building a program in `Auto` is basically specifying relationships that are
maintained "forever"...and thinking about your program in that manner.

For example:

~~~haskell
sumAndProd :: Auto' Int Int
sumAndProd = proc x -> do
    sumX  <- sumFrom 0     -< x
    prodX <- productFrom 1 -< x
    id -< sumX + prodX

-- sumAndProd = liftA2 (+) (sumFrom 0) (productFrom 1)
~~~

`sumX` is a "forever" quantity...and so is `x`.  We say that the relationship
between `sumX` and `x` is that `sumX` is the cumulative sum (`sumFrom 0`) of
`x`.  The relationship between `prodX` and `x` is that `prodX` is the
cumulative product...and the relationship between `x` and the output is that
the output is the sum of `sumX` and `prodX` at every point in time.

Operationally, you also have a huge advantage here over using something like
`State` in that each `Auto` really contains its own "internal state" that is
inaccessible by the world.  For example, in that last example, `sumFrom 0`
works by maintaining its own internal state.  `productFrom 1` also maintains
its own internal state.

Nobody can ever "touch" or "inspect" the internal state of `sumFrom 0` and
`prudctFrom 1`.  It maintains it on its own.  This is in big contrast to
`State`-based solutions, which necessarily work on "global state", and
managing global vs. local state with monad morphisms.

Note that this "composes"; we can use `sumAndProd` in another `Auto`:

~~~haskell
foo :: Auto' Int String
foo = proc x -> do
    sp <- sumAndProd -< x
    y  <- blah blah  -< sp + x
    id -< show y
~~~

And `sumAndProd` now is its own "internally stateful" thing...you can take it
and pop it onto any other chain.  In `State`, you'd open yourself up to having
to create new sum types for extra state...whenever you combined any two
stateful operations on different states.

This locally stateful property truly allows us to "compose" ideas together and
relationships together and think of them as fixed invariants in a big picture.
Because each `Auto` "denotes" a relationship, and we build up bigger `Auto`s
by combining small denotative promitives to create bigger things that denote
more complex relationships, it really allows us to create a denotative
"language", where we declare relationships by building up smaller units of
meaning into bigger units of meaning.

Now...how do we actually implement the behavior that we want?  This is a job
for the primitive `Auto`s, but also really much a big job for ... the semantic
tools that come with the library!

Semantic Tools
--------------

An `Auto` represents a relationship between an input stream and an output
stream, but in order to build more expressive programs, this library also
comes with more semantic tools to work with in characterizing your streams
with "meaning", and tools to manipulate them and compose them in powerful ways
(within this framework of meaning) to express your programs.

The two main ones are `Blip` and `Interval`.

### Blip

We say that, in the context of inputs/outputs of `Auto`, a `Blip a` represents
a "blip stream" that occasionally, in isolated incidents, emits a value of
type `a`.

For example, `Auto' a (Blip b)` is an `Auto'` that a stream of `a`'s as input
and outputs a *blip stream* that occasionally emits with a `b`.  An `Auto'
(Blip a) b` is an `Auto'` that takes a *blip stream* that occasionally emits
with a `a` and outputs a stream of `b`'s.

If an `Auto` takes or outputs a "blip stream", it comes with some "semantic"
contracts on to how the stream behaves.  The main contract is that your `Blip`
stream should only output on (meaningfully) "isolated" incidents, and never on
continuous regions of the input stream.

This isn't enforced by the type system, but almost all of the `Auto`s offered
in this library will preserve this property!  And we encourage any that you
make to also preserve this property, in order to make "blip streams" *useful
in the first place*.

We saw an example earlier,

~~~haskell
ghci> let emitOn5s = emitOn (\x -> x `mod` 5 == 0)
ghci> streamAuto' emitOn5s [1,5,9,3,10,2]
[NoBlip, Blip 5, NoBlip, NoBlip, Blip 10, NoBlip]
~~~

Let's see if we can play around with it!  Well, we can "tag" blip emissions:

~~~haskell
ghci> streamAuto' (tagBlips "hey" . emitOn5s) [1,5,9,3,10,2]
[NoBlip, Blip "hey", NoBlip, NoBlip, Blip "hey", NoBlip]
~~~

And with proc blocks, we can even "name" blip streams and manipulate them as
streams!  Oh, also, `Blip` is a `Functor`, so you can use `fmap` and `(<$)`.

~~~haskell
blippy :: Monad m => Auto m Int String
blippy = proc x -> do
    on3s  <- tagBlips "3!" . emitOn3s -< x
    on5s' <- emitOn5s    -< x
    let on5s  = "5!" <$ on5s        -- from Data.Functor: replace all emitted
                                    -- values with the string "5!"
        on35s = on3s `mergeL` on5s  -- merge the streams, favoring the left
    intro  <- immediately -< "hello!"
    middle <- inB 6       -< "#6!"
    wut    <- never       -< "this should never happen!"
    id -< mergeLs [never, intro, middle, on35s] -- merge all, favoring firsts
~~~

~~~haskell
ghci> streamAuto' blippy [5,7,15,10,13,15,2]
[Blip "hello!", NoBlip, Blip "3!", Blip "5!", NoBlip, Blip "#6!", NoBlip]
--    ^ intro             ^ on3s     ^ on5s            ^ middle
~~~

Blip streams and "blip contracts"/"blip semantics" are useful because a lot of
the other semantic abstractions in `Auto` (like switches, and `Interval`) all
work with the "idea" of a "discrete", occasional, conceptually
"non-contiguous" blip stream.

Check out all of the built-in blip stream combinators at
*[Control.Auto.Blip][]*.

### Interval

The "opposite" of `Blip` and blip streams are "intervaled" `Auto`s: `Auto`s
that are "on" or "off" for (conceptually) contiguous chunks of steps.

An `Interval' a b` represents an `Auto` that takes a stream of `a`s as input,
and outputs a stream of `b`s that is "on" or "off", at contiguous swaths.

In truth, `Interval' a b` is just a type synonym for `Auto' a (Maybe b)`, and
`Interval m a b` is just a type synonym for `Auto m a (Maybe b)`.  But, if you
see a library auto with type `Interval`, or if you make an auto with type
`Interval`, it comes with "contracts".  These contracts help us really use
`Interval`s in a meaningful way --- that they are supposed to represent
`Auto`s that output things that are "on" or "off" for contiguous steps.

`Blip`s are "blippy", `Interval`s are "chunky".

We've already seen an `Interval` earlier:

~~~haskell
ghci> streamAuto' (hold . emitOn5s) [1,5,9,3,10,2]
[Nothing, Just 5, Just 5, Just 5, Just 10, Just 10]
~~~

`hold :: Interval' (Blip a) a`, so it turns a blip stream into a stream of
`a`s that are on and off.  In this case, it starts off "off", and is "on"
after the first emitted value, with the last emitted value.

`Interval`s are nice because you can have "choices" between two "on-off"
`Auto`s:

~~~haskell
ghci> let a1 = (onFor 3 . arr (+ 100)) <|!> whenI (> 6) <|!> arr (+ 200)
ghci> take 10 $ streamAuto' a1 [1..]
[101, 102, 203, 204, 205, 206, 7, 8, 9, 10]
ghci> let a2 = chooseInterval [offFor 8, onFor 3 . arr (+ 100)]
ghci> take 10 $ streamAuto' a2 [1..]
[Just 101, Just 102, Just 103, Nothing, Nothing, Just 6, Just 7]
~~~

(`<|!>`) forks the input into both `Interval`s, and the outputted one is the
first one that is "on".  You can chain them as long as the "final" `Auto` is
an `Auto`, and not an `Interval`:

~~~haskell
(<|!>) :: Interval m a b -> Auto m a b -> Auto m a b
~~~

`onFor n` lets the input pass for `n` steps.  `whenI` lets the input "pass
through" when the predicate is true (being sure to pick a meaningful predicate
based on the expected input for "chunky" output)

You can also "chain" `Interval`s with `bindI` and `compI`:

~~~haskell
ghci> streamAuto' (whenI (< 3) `compI` whenI (> 6)) [1..8]
[Just 1, Just 2, Nothing, Nothing, Nothing, Just 6, Just 7, Just 8]
ghci> streamAuto' (bindI (whenI (< 3)) . whenI (> 6)) [1..8]
[Just 1, Just 2, Nothing, Nothing, Nothing, Just 6, Just 7, Just 8]
~~~

Intervals are also used for things that want their `Auto`s to "signal" when
they are "off".  `Interval` is the universal language for, "you can be done
with me", when it is needed.  For example, the `interactAuto` loop takes an
`Interval String String`, and "turns off" on the first `Nothing` or "off"
value.

~~~haskell
ghci> interactAuto (onFor 4 . (++ "!!!"))
> hello
hello!!!
> how
how!!!
> are
are!!!
> you
you!!!
> today
--- (end of output)
~~~

Like with blip streams, intervals are used to great effect with switches, like
the useful `(-->)` combinator:

~~~haskell
ghci> let a1 = whileI (<= 4) --> pure 0
ghci> streamAuto' a1 [1..10]
[1, 2, 3, 4, 0, 0, 0, 0, 0, 0]
               -- look, recursion!
ghci> let a2 = (onFor 3 . pure "hi") --> (onFor 2 . pure "bye") --> a2
ghci> take 10 $ streamAuto' a2 (repeat ())
["hi", "hi", "hi", "bye", "bye", "hi", "hi", "hi", "bye", "bye"]
~~~

You can see all of the built-in `Interval` combinators in
*[Control.Auto.Interval][]*.

### More Tools

#### Switching

A powerful grab-bag of tools that can be used with intervals and blip streams
is the idea of "switching", as mentioned earlier.  `Auto`s that behave like
one `Auto` for a while, and then another afterwards.

For example, `switchOn_` and `switchOnF` lets you have an `Auto` that behaves
like one `Auto`, until the blip stream it is receiving emits something ---
then, it behaves like a totally new one, based on the emitted value.

`switchFrom_` and `switchFromF` also gives you an `Auto` that behaves like one
`Auto`...except that `Auto` has the ability to "replace itself" by having its
output blip stream emit a value.  The value determines what it wants to
replace itself with.

These are really useful for implementing things like "modes" --- your program
has different modes of behavior, which you can represet with a different
`Auto` for each mode...and you can switch between them with these switches!

See the documentation for thise at the *[Control.Auto.Swtich][]* module for
more information!

#### Collections

In *[Control.Auto.Collection][]*, we have a bunch of "`Auto` boxes" and
"`Auto` collections", which maintain `Auto`s that are dynamic collections of
`Auto`s.

For example, you have `zipAuto`, which takes a list of `Auto`s and returns an
`Auto` taking in a list, that feeds each item in the input list into each
corresponding `Auto`.  It's like running multiple `Auto`s in parallel on
different inputs.

For example, you have `mux f :: Auto m (k, a) b`, which stores a bunch of
`Auto m a b`s indexed by a key `k`.  At every step, it takes a `(k, a)`,
looks up the `Auto` at that `k`, feeds in the `a`, and outputs that output
`b`.  You can use this to store several `Auto`s in parallel and really just
run the one you want at any given time.

There's also `gather f :: Auto m (k, a) (Map k b)`, which again stores a bunch
of `Auto m a b`s indexed by a key `k`.  At every step, it *updates* only the
`Auto` at that key `k`, but outputs a `Map` of all the outputs so far by all
of the internal `Auto`s.

See the documentation at *[Control.Auto.Collection][]* for more!

### Recursive relationships

Not exactly a tool per se, but the *auto* library has the ability to state and
"solve" for recursive relationships.

We can define an `Auto` that "chases" its input:

~~~haskell
chaseFrom :: Num a => a -> Auto' a a
chaseFrom x0 = proc target -> do
    rec let step = signum (target - x)  -- 1 if target is bigger
                                        -- 0 if matches
                                        -- -1 if smaller

        x <- sumFromD 0 -< step

    id -< x
~~~

~~~haskell
ghci> streamAuto' (chaseFrom 0) [3,3,3,3,3,-1,-1,-1,-1,-1]
  [0,1,2,3,3,3,2,1,0,-1]
-- ^ chasing 3 ^
--             ^ chasing -1
~~~

`x` is the cumulative sum of each `step` and the `step` is determined based on
the `target` and the current position `x`.  So `x`'s relationship is that it
is the cumulative sum of `step`, and `step`'s relationship is that it is the
difference between `x` and `target`.  It's a recursive relationship!

The *auto* library will attempt to find a "fixed point" of the recursive
relationship...sort of "solving for" the output stream that will match this
recursive relationship.  However, it needs a little help.  For every step, it
needs a way to get a "first value" from *something* without needing any input.
That is, at least *one* of the `Auto`s in your proc block has to be able to
pop out its *first* result without an input.

This is what `sumFromD` is for...we don't use `sumFrom`, but `sumFromD`.
`sumFromD` will always output *its original accumulator first*, before taking
into account the inputs:

~~~haskell
ghci> streamAuto' (sumFrom 0) [1..10]
[1,3,6,10,15,21,28,36,45,55]
ghci> streamAuto' (sumFromD 0) [1..10]
[0,1,3,6,10,15,21,28,36,45]
~~~

This is how the *auto* library will "tie" the loop and find the fixed point.
Have this, and everything works!  Cyclic relationships and feedback loops...
just like in real life!


Serialization
-------------

One of this library's features is that the `Auto` type offers an interface in
which you can serialize ("freeze") and "resume" an Auto, in `ByteString`
(binary) form.

You can "freeze" any `Auto` into a `ByteString` using `encodeAuto`, and you
can "resume" any `Auto` from a `ByteString` using `decodeAuto`.

Note `decodeAuto` and `loadAuto` "resume" a *given `Auto`*.  That is, if
you call `decodeAuto` on a "fresh `Auto`", it'll decode a `ByteString`
into *that `Auto`, but "resumed"*.  That is, it'll "fast forward" that
`Auto` into the state it was when it was saved.

For example, let's look at `sumFrom 0`.  If it is fed 3 and 10, it'll have its
internal accumulator as 13, keeping track of all the numbers it has seen so
far.

~~~haskell
ghci> let a         = sumFrom 0
ghci> let (_, a')   = stepAuto' a  3
ghci> let (_, a'')  = stepAuto' a' 10
~~~

`encodeAuto` can be used to "freeze"/"save" the `Auto` into the `ByteString`
`bs`:

~~~haskell
ghci> let bs            = encodeAuto a''
~~~

`decodeAuto` can be used to "resume" from the *original* `a`.  Remember, `a`
was the original `Auto`, the summer `Auto` with a starting accumulator of 0.
`decodeAuto` will "resume" it, with and resume it with its internal
accumulator at 13.

~~~haskell
ghci> let Right resumed = decodeAuto a bs
ghci> let (y, _)        = stepAuto' resumed 0
13
~~~

Note that not all `Auto`s in this library can be resumed.  By default, you can
assume that they *can*...while those that can't will by naming convention be
suffixed with a `_`:  `sumFrom` vs. `sumFrom_`, for example.  This means that
when you "save" the `Auto`, you don't really save any state...and when you
"resume" it, nothing is really resumed, and resuming is a no-op:

~~~haskell
-- sumFrom_ can't be saved/resumed, so it "goes nowhere" when resumed.
decodeAuto (sumFrom_ 0) bs = Right (sumFrom_ 0)
~~~

This feature is useful for "save states" of certain `Auto`s, or just for
serialization and resuming in general.

You can play some fun tricks with the *[Control.Auto.Serialize][]*
module...for example, `saving "foo.dat"` will turn any `Auto` into an `Auto`
that serializes itself at every step to "foo.dat"

~~~haskell
ghci> let a1 = saving "foo.dat" (sumFrom 0) :: Auto IO Int Int
ghci> streamAuto a1 [1..10]         -- saves the Auto as it goes along
[ 1, 3, 6,10,15,21,28,36, 45, 55]
ghci> a2 <- readAutoErr "foo.dat" a1 :: Auto IO Int Int
ghci> streamAuto a2 [1..10]         -- a2 is resumed to where a1 was last
[56,58,61,65,70,76,83,91,100,110]
~~~

If you want to make your own `Auto` combinators and transformers that work
with serialization, see the mini-tutorial at the documentation for
[mkAutoM][] in the [Control.Auto.Core][] module

### Serialization composes

The magic of implicit serialization is that the serliazation of complex
`Auto`s is preserved under combination and manipulation with the various
instances and combinators in this library.  For example, serializing the
complex `blippy` example above, or a huge complex application, is all done
automatically!  The overall serialization structure is implicitly built and
inferred.  Think of it like the library analyzing what needs to be serialized
in your program, and coming up with a serialization and reloading strategy.

This is used to great effect in [auto-examples][], where entire applications and
chat bots are serialized..."for free".  Build complex chat bots, and the
serialization is handled implicitly.

### Safecopy problem

There is one slightly drawback however...the "safecopy" problem.  If you
alter the structure of your `Auto` by adding another aspect that needs to be
serialized...your `Auto` can no longer "read"/resume from the binary
serialization of its older version, because it'll expect the previous
serialization strategy, and be unable to read it.  This means that, if you
publish programs, save files might become unloadable by new versions of your
`Auto`.

One solution is to *serialize individual portions* only of your program ---
portions that you know will stay fixed.  You can do this by techniques in
[chatbot][], where each individual module of the chatbot is serialized to its
own place on disk using `serializing`, a variation of `saving` from above.
That way, if you add more modules to the chat bot, it can still individually
resume its smaller modules without caring about the rest.

[chatbot]: https://github.com/mstksg/auto-examples#chatbot

(I'll admit that this is not a perfect solution; more research and experiments
are continually being done.  Feel free to talk to me if you have any ideas or
leads!)

Final partings
--------------

One last note before finishing up...if you ever want to implement a low-level
library, or implement a "backend", defining your own `Auto`s and working with
them has its own rules.  You're a bit "on your own", in this sense; the
optimization game might take you to places that really get rid of the nice
semantic denotative ideals of this library. I plan on writing a
framework/low-level guide soon (for writing, say, a GUI framework, or hooking
on GUI).

However, one good principle is just to *separate* your "two hats" as much as
possible.  There's the hat you wear when you are thinking about your program
logic, dealing with compositions of ideas ... and there's the hat you wear
when you are at the nitty-gritty interface between your system and the real
world. One goal in Haskell is always to be able to create as clear a divide as
possible...so you can really enjoy the best of both worlds.  So just make sure
that the `Auto`s and API that you export behave in meaningful ways that you
can reason about...just what we expect from using `Auto` :)

Anyways, I recommend just looking over the combinators available to you in the
various modules, like *[Control.Auto.Blip][]*, *[Control.Auto.Interval][]*,
and *[Control.Auto.Switch][]*.  We didn't go over anything close to all of
them in this tutorial, so it's nice for getting a good overview.  The most
up-to-date documentation at this point in time is on [the github pages][docs]

A good next step too wouild be also just looking at the [auto-examples][]
directory and peruse over the examples, which each highlight a different
aspect of the library, so you can see how all of these ideas work together.
There will also be writeups on [my blog][blog] coming up too!

Help is always available on the *#haskell-auto* channel on freenode IRC; you
can also email me at <justin@jle.im>, or find me on twitter as
[mstk][twitter].  There is no mailing list or message board yet, but for now,
feel free to abuse the [github issue tracker][issues].

[twitter]: https://twitter.com/mstk
[issues]: https://github.com/mstksg/auto/issues

Now go forth and make locally stateful, denotative, declarative programs!

[Control.Auto.Blip]: http://mstksg.github.io/auto/Control-Auto-Blip.html
[Control.Auto.Collection]: http://mstksg.github.io/auto/Control-Auto-Collection.html
[Control.Auto.Interval]: http://mstksg.github.io/auto/Control-Auto-Interval.html
[Control.Auto.Run]: http://mstksg.github.io/auto/Control-Auto-Run.html
[Control.Auto.Serialize]: http://mstksg.github.io/auto/Control-Auto-Serialize.html
[Control.Auto.Switch]: http://mstksg.github.io/auto/Control-Auto-Switch.html
[Control.Auto.Core]: http://mstksg.github.io/auto/Control-Auto-Core.html
[mkAutoM]: http://mstksg.github.io/auto/Control-Auto-Core.html#v:mkAutoM
