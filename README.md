Auto
====

(Working name)

**Auto** is a Haskell DSL/library providing declarative, compositional,
denotative semantics for discrete-step, locally stateful, interactive
programs, games, and automations, with implicitly derived serialization.

*   **Haskell DSL/library**: It's a Haskell library that provides a
    domain-specific language for composing and declaring your programs/games.

    Why Haskell?  Well, Haskell is one of the only languages that has a type
    system expressive enough to allow type-safe compositions without getting
    in your way.  Every composition and component is checked at compile-time
    to make sure they even make sense, so you can work with an assurance that
    everything fits together in the end --- and also in the correct way.  The
    type system can also guide you in your development as well.  All this
    without the productivity overhead of explicit type annotations.  In all
    honesty, it cuts the headache of large projects down --- and what you need
    to keep in your head as you develop and maintain --- by at least 90%.

*   **Declarative**: It's not imperative.  That is, unlike in other
    languages, you don't program your program by saying "this happens, then
    this happens...and then in case A, this happens; in case B, something else
    happens".  Instead of specifying your program/game by a series of
    state-changing steps and procedures (a "game loop"), you instead declare
    "how things are".  You declare fixed or evolving relationships between
    entities and processes and interactions.  And this declaration process is
    high-level and pure.

*   **Denotative**: Instead of your program being built of pieces that change
    things and execute things sequentially, your entire program is composed of
    meaningful semantic building blocks that "denote" constant relationships
    and concepts.  The composition of such building blocks also denote new
    concepts.

*   **Compositional**: You build your eventually complex program/game out of
    small, simple components.  These simple components compose with eachother;
    and compositions of components compose as well with other components.
    Every "layer" of composition is seamless.  It's the [scalable program
    architecture][spa] principle in practice: If you combine an A with an A,
    you don't get a B; you get another A, which can combine with any other A.

    Like unix pipes, where you can build up complex programs by simply piping
    together simple, basic ones.

*   **Discrete-step**: This library is meant for things that step discretely;
    there is no meaningful concept of "continuous time".  Good examples
    include turn-based games, chat bots, and cellular automata; bad examples
    include real-time games and day trading simulations.

*   **Locally stateful**: Every component encapsulates its own local (and
    "hidden") state.  There is no global or impicitly shared state.  This is
    in contrast to those "giant state monad" libraries/abstractions where you
    carry around the entire game/program state in some giant data type, and
    have your game loop simply be an update of that state.

    If you have a component representing a player, and a component
    representing an enemy --- the two components do not have to ever worry
    about the state of the other, or the structure of their shared state.

    Also, you never have to worry about something reading or modifying a part
    of the shared/global state it wasn't meant to read or modify!  (Something
    you cannot guaruntee in the naive implementatation of the "giant state
    monad" technique).

*   **Interactive**: The behavior and structure of your program can respond
    and vary dynamically with outside interaction.  I'm not sure how else to
    elaborate on the word "interactive", actually!

*   **Interactive programs, games and automations**: Programs, games, and
    automations/simulations.  If you're making anything discrete-time that
    encapsulates some sort of internal state, especially if it's interactive,
    this is for you!! :D

*   **Implicitly derived serialization**: All components and their
    compositions by construction are automatically "freezable" and
    serializable, and re-loaded and resumed with all internal state restored.
    As it has been called by ertes, it's "save states for free".

[spa]: http://www.haskellforall.com/2014/04/scalable-program-architectures.html

The current development documentation server is found at
<https://mstksg.github.io/auto>.  You can find examples and demonstrations in
the [auto-examples][] repo on github; they are constantly being kept
up-to-date with the currently super unstable API.

[auto-examples]: https://github.com/mstksg/auto-examples

More examples and further descriptions will appear here as development
continues.

## Support

Though this library is not officially released yet, the official support and
discussion channel is #haskell-auto on freenode.  You can also usually find me
as *jle`* on #haskell and #haskell-game.  Also, contributions to documentation
and tests are welcome! :D

Why Auto?
---------

Auto is distinct from a "state transformer" (state monad, or explicit state
passing) in that it gives you the ability to implicitly *compose and isolate*
state transformers and state.

That is, imagine you have two different state monads with different states,
and you can compose them together into one giant loop, and:

1.  You don't have to make a new "composite type"; you can add a new component
    dealing with its own state without changing the total state type.

2.  You can't write anything cross-talking.  You can't write anything that
    can interfere with the internal state of any components; each one is
    isolated.

So --- Auto is useful over a state monad/state transformer approach in cases
where you like to build your problem out of multiple individual components,
and compose them all together at once.

Examples include a multiple-module stateful chat bot, where every module of
the chat bot consists of its own internal state.

If you used a state monad approach, every time you added a new module with its
own state, you'd have to "add it into" your total state type.

This simply does *not* scale.

Imagine a large architecture, where every composition adds more and more
complexity.

Now, imagine you can just throw in another module with its own state without
any other component even "caring".  Or be able to limit access implicitly,
without explicit "limiting through lifting" with `zoom` from lens, etc.
(Without that, you basically have "global state" --- the very thing that we
went to Functional Programming/Haskell to avoid in the first place!  And the
thing that languages have been trying to prevent in the last twenty years of
language development.  Why go "backwards"?)

In addition to all of these practical reasons, State imposes a large
*imperative* shift in your design.

State forces you to begin modeling your problem as "this happens, then this
happens, then this happens".  When you choose to use a State monad or State
passing approach, you immediately begin to frame your entire program from an
imperative approach.

Auto lets you structure your program *denotatively* and declaratively.  It
gives you that awesome style that functional programming promised in the first
place.

Instead of saying "do this then that", you say "this is how things...just
*are*.  This is the structure of my program, and this is the nature of the
relationship between each component".

If you're already using Haskell...I shouldn't have to explain to you the
benefits of a high-level declarative style over an imperative one :)

Why not Auto?
-------------

That being said, there are cases where **Auto** is either the wrong tool or
not very helpful.

*   Cases involving inherently continuous time.  **Auto** is meant for
    situations where time progresses in discrete ticks --- integers, not
    reals.  Auto is not suggested even to "simulate" continuous time with
    discrete sampling. You can do it...but FRP is a much, much better
    abstraction/system for handling this than **Auto** is.  See the later
    section on FRP.

*   Cases where you really don't have interactions/compositions between
    different stateful components.  If all your program is just one `foldr` or
    `scanl` or `iterate`, and you don't have multiple interacting parts of
    your state, **Auto** really can't offer much.  If, however, you have
    multiple folds or states that you want run together and compose, then this
    might be useful!

*   Intense IO stuff and resource handling.  **Auto** is not *pipes* or
    *conduit*. All IO is done "outside" of the **Auto** components; **Auto**
    can be useful for file processing and stream modification, but only if you
    separately handle the IO portions.  **Auto** works very well with *pipes*
    or *conduit*; those libraries are used to "connect" **Auto** to the
    outside word, and provide a safe interface.


Relation to FRP
---------------

**Auto** borrows a lot of concepts from *[Functional Reactive
Programming][frp]* --- especially arrowized, locally stateful libraries like
[netwire][].  **Auto** attempts to bring an applicable subset of FRP's
high-level concepts and semantics and transplant them into the world of
fundamentally discrete-step/discrete-time contexts.  Users of such libraries
would likely be able to quickly pick up **Auto**, and the reverse is
(hopefully) true too.

Note that this library is not meant to be any sort of meaningful substitution
for implementing situations which involve concepts of continuous ("real
number-valued", as opposed to "integer valued") time (like real-time games);
you can "fake" it using **Auto**, but in those situations, FRP provides a much
superior semantics and set of concepts for working in such contexts.

[frp]: http://en.wikipedia.org/wiki/Functional_reactive_programming
[netwire]: https://hackage.haskell.org/package/netwire
