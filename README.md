Auto
====

(Working name)

**Auto** is a Haskell DSL/library providing denonational, composable semantics
for discrete-step, locally stateful, interactive programs, games, and
automations.

*   **Haskell DSL/library**: It's a Haskell library that provides a
    domain-specific language for composing and declaring your programs/games.

*   **Denotational**: It's not imperative.  That is, unlike in other
    languages, you don't program your program by saying "this happens, then
    this happens...and then in case A, this happens; in case B, something else
    happens".  Instead of specifying your program/game by a series of
    state-changing steps and procedures (a "game loop"), you instead declare
    "how things are".  You declare fixed or evolving relationships between
    entities and processes and interactions.  And this declaration process is
    high-level and pure.

*   **Composable**: You build your eventually complex program/game out of
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

[spa]: http://www.haskellforall.com/2014/04/scalable-program-architectures.html

Examples and further descriptions will appear here as development continues.

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

