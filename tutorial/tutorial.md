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

### Semantic Picture

Semantically, a `Auto` describes a relationship between an input and an
output that is preserved over multiple steps.

In a way, you can think about `Auto`s as *stream transformers*.  A stream of
sequential inputs come in one at a time, and a stream of outputs pop out one
at a time as well.

A trivial exmple is just a static stateless function --- here we have the
`Auto` `arr (*2)`, which always maps every input value to an output value that
is the input value, doubled:

~~~haskell
-- streamAuto' :: Auto' a b -> [a] -> [b]
-- [ 1, 2, 3, 4, 5, 6, 7, 8, 9,10...    -- the inputs
ghci> take 10 $ streamAuto' (arr (*2)) [1..]
   [ 2, 4, 6, 8,10,12,14,16,18,20]      -- the outputs
~~~

For a more interesting example, you can have an `Auto` where the output
corresponding to an input depends on the "history", as well.  A "stateful"
function.  For example, we have the `Auto` `sumFrom n`, whose output is always
the cumulative sum of all items received so far.

~~~haskell
-- [ 1, 2, 3, 4, 5, 6, 7, 8, 9,10...    -- the inputs
ghci> take 10 $ streamAuto' (sumFrom 0) [1..]
   [ 1, 3, 6,10,15,21,28,36,45,55]      -- the outputs
~~~

A bit on types --- `sumFrom n` is `Num a => Auto m a a` ... or, if
specializing it helps, `Auto' Int Int`.  You can read this as "a relationship
between two `Int`s fixed over the stream", or "a one-by-one mapping of an
`Int` stream to another `Int` stream".  For `sumFrom n`, the relationship is
that the output is always the cumulative sum of the inputs.

That's what they are semantically, and that's what an `Auto` would denote.

### Operational picture

Operationally, an `Auto` does this by acting as a "stateful function" that we
can "run" with `stepAuto`.  A function with "internal state".

~~~haskell
-- stepAuto' :: Auto' a b -> a -> Output' a b
ghci> let Output x nextAuto  = stepAuto' (sumFrom 0) 5
ghci> x
5
ghci> let Output y nextAuto2 = stepAuto' nextAuto 3
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

It returns the output and the next `Auto` in an `Output'` data type, which is
just a glorified tuple for convenience:

~~~haskell
data Output' a b = Output' { outRes  :: b
                           , outAuto :: Auto' a b
                           }
~~~

In practice, this is usually going to be your "main loop", or "game loop":

1.  Collect input from the world (using IO, or whatever you need)
2.  Step the `Auto` you have with that input.
3.  Get the output from that `Auto`, and the next `Auto`
4.  Show or render your output to the world however you want.
5.  Repeat all again, but with the new `Auto` from step 3.

(If your program doesn't need any outside input, then you can just use
`stepAutoN` with `()`, or `streamAuto'` with an infinite list.)

There are some built-in "loops" like this in the `Control.Auto.Run` module,
for running in `IO` by reading and showing inputs and ouputs (`interact`,
`interactRS`) if you want to try these out!

What's in a type?
-----------------

Enough handwaving!  What do these types even mean?  What are the type
parameters?

An `Auto' a b` describes *a relationship* between a stream of inputs `a` and a
stream of outputs `b` that is maintained over several steps of inputs.

One way to look at it is that, with `streamAuto'` an `Auto' a b` gives you an
`[a] -> [b]`.

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
`streamAuto` from an `Auto m a b` gives you an `[a] -> m [b]`.

Operationally, if `Auto' a b` is a `a -> b` with internal state, then `Auto m
a b` is a `a -> m b` with internal state.  If you feed it an `a`, it'll return
a `b` and a "next/updated `Auto`" in a monadic context --- with `stepAuto`,
you get a `a -> m (b, Auto m a b)`.

This monadic context means that in the process of "stepping" or "running" the
`Auto`, you can perform effects and get input from an outside world.

For the most part, real-life `Auto`s will be written parameterized over
`Monad` or some `Monad` typeclass:

~~~haskell
myAuto :: Monad m => Auto m Int Bool
~~~

Working with `Monad m => Auto m a b` is practically identical to working with
`Auto' a b`, so there really isn't ever a real point to actually *write* an
`Auto'`.  However, specializing to `Auto'` lets us use simple "running"
functions like `streamAuto'` and `stepAuto'`.

You probably also already know about `Output m a b`, which is a glorified `(a,
Auto m a b)` tuple, and `Output' a b`, which is a glorified `(a, Auto' a b)`
tuple.  You can pattern match on it using the `Output` constructor, or just
access them using `outRes` and `outAuto`.

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

Now, the `Category` instance is probably the most powerful tool at your
disposal.  As a first treat, it gives you `id :: Auto m a a`, an `Auto` whose
output is always exactly the corresponding input.

But more importantly, you can "chain together" `Auto`s end-to-end.  Compose
them as if they were functions:

~~~haskell
ghci> streamAuto' (sumFrom 0) [1..5]
[1,3,6,10,15]
ghci> streamAuto' (productFrom 1) [1,3,6,10,15]
[1,3,18,180,2700]
ghci> streamAuto' (productFrom 1 . sumFrom 0) [1..5]
[1,3,18,180,2700]
~~~

`sumFrom 0`'s output stream is the cumulative sum of the inputs.  `productFrom
1` is the cumulative product of the inputs.  So `productFrom 1 . sumFrom 0`'s
output stream is the result of using the output stream of `sumFrom 0` as the
input stream to `productFrom 1`:

~~~haskell
(.) :: Auto m b c -> Auto m a b -> Auto m a c
~~~

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
is the corresponding input applied to the given function).  But more
importantly, we get proc notation!

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

Those are the primary typeclass based interfaces; explore the library for
more!

TODO: What else to put here?

### From scratch

Creating `Auto`s from scratch, we have:

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
mkAccum   :: (b -> a -> b)        -> b -> Auto m a b
mkAccumM  :: (b -> a -> m b)      -> b -> Auto m a b
mkState   :: (a -> s -> (b, s))   -> s -> Auto m a b
mkStateM  :: (a -> s -> m (b, s)) -> s -> Auto m a b
mkAuto_   :: (a -> Output m a b)       -> Auto m a b
mkAutoM_  :: (a -> m (Output m a b))   -> Auto m a b
~~~

You can look at the documentation for all of these, but these all basically
work with "internal state" --- `iterator` ignores its input and repeatedly
applies a function to a value and pops it out at every step.  `mkAccum`
maintains that the *output* is always the result of "folding together" (a la
`foldl`) all of the inputs so far, with a starting value.  `mkState` is
like a more powerful `mkAccum`, which keeps an internal state that is updated
at every step.  `mkAuto_` lets you describe an `Auto` by its behavior under
`stepAuto'`.

~~~haskell
ghci> take 10 . streamAuto' (iterator (+1) 0) $ repeat ()
[0,1,2,3,4,5,6,7,8,9]
ghci> take 10 . streamAuto' (mkAccum (+) 0)   $ [1..]
[1,3,6,10,15,21,28,36,45,55]
~~~

It is recommended to only use `mkAccum`, `mkState`, `mkAuto` only when
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

