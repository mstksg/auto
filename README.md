Auto
====

~~~bash
cabal install auto
~~~

Check it out!
-------------

~~~haskell
-- Let's impliement a PID feedback controller over a black box system.

import Control.Auto
import Prelude hiding ((.), id)

-- We represent a system as `System`, an `Auto` that takes stream of `Double`s
-- as input and transforms it into a stream of `Double`s as output.  The `m`
-- means that a `System IO` might do IO in the process of creating its ouputs,
-- for instance.
--
type System m = Auto m Double Double

-- A PID controller adjusts the input to the black box system until the
-- response matches the target.  It does this by adjusting the input based on
-- the current error, the cumulative sum, and the consecutative differences.
--
-- See http://en.wikipedia.org/wiki/PID_controller
--
-- Here, we just lay out the "concepts"/time-varying values in our system as a
-- recursive/cyclic graph of dependencies.  It's a feedback system, after all.
--
pid :: MonadFix m => (Double, Double, Double) -> System m -> System m
pid (kp, ki, kd) blackbox = proc target -> do
    rec --  err :: Double
        --  the difference of the response from the target
        let err        = target - response

        -- cumulativeSum :: Double
        -- the cumulative sum of the errs
        cumulativeSum <- sumFrom 0 -< err

        -- changes :: Maybe Double
        -- the consecutive differences of the errors, with 'Nothing' at first.
        changes       <- deltas    -< err

        --  adjustment :: Double
        --  the adjustment term, from the PID algorithm
        let adjustment = kp * err
                       + ki * cumulativeSum
                       + kd * fromMaybe 0 changes

        -- the control input is the cumulative sum of the adjustments
        control  <- sumFromD 0 -< adjustment

        -- the response of the system, feeding the control into the blackbox
        response <- blackbox   -< control

    -- the output of this all is the value of the response
    id -< response
~~~


What is it?
-----------

**Auto** is a Haskell DSL and platform providing an API with declarative,
compositional, denotative semantics for discrete-step, locally stateful,
interactive programs, games, and automations, with implicitly derived
serialization.  It is suited for any domain where your program's input or
output is a stream of values, input events, or output views.  At the
high-level, it allows you to describe your interactive program or simulation
as a *stream transformer*, by composition and transformation of other stream
transformers.

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

*   **Platform**: Not only gives the minimal tools for creating your programs,
    but also provides a platform to run and develop and integrate them, as
    well as many library/API functions for common processes.

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
    concepts.  Your building blocks are well-defined *ideas*.

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

Intrigued?  Excited?  Start at [the tutorial][tutorial]!

[tutorial]: https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md

It's a part of this package directory and also on github at the above link.
The current development documentation server is found at
<https://mstksg.github.io/auto>. You can find examples and demonstrations in
the [auto-examples][] repo on github; they are constantly being kept
up-to-date with the currently super unstable API.

[auto-examples]: https://github.com/mstksg/auto-examples

More examples and further descriptions will appear here as development
continues.

### Support

Though this library is not officially released yet, the official support and
discussion channel is #haskell-auto on freenode.  You can also usually find me
(the maintainer and developer) as *jle`* on #haskell and #haskell-game.  Also,
contributions to documentation and tests are welcome! :D

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
    reals.  You can "fake" it by faking continuous time with discrete
    sampling...but FRP is a much, much more powerful and safe
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
[netwire][].  At best, **Auto** can be said to bring a lot of API ideas and
borrows certain aspects of the semantic model of FRP and incorporates them as
a part of a broader semantic model more suitable for discrete-time
discrete-stel contexts.  But, users of such libraries would likely be able to
quickly pick up **Auto**, and the reverse is (hopefully) true too.

Note that this library is not meant to be any sort of meaningful substitution
for implementing situations which involve concepts of continuous ("real
number-valued", as opposed to "integer valued") time (like real-time games);
you can "fake" it using **Auto**, but in those situations, FRP provides a much
superior semantics and set of concepts for working in such contexts.  That is,
you can "fake" it, but you then lose almost all of the benefits of FRP in the
first place.

[frp]: http://en.wikipedia.org/wiki/Functional_reactive_programming
[netwire]: https://hackage.haskell.org/package/netwire

A chatbot
---------

~~~haskell
import qualified Data.Map as M
import Data.Map (Map)
import Control.Auto
import Prelude hiding ((.), id)

-- Let's build a big chat bot by combining small chat bots.
-- A "ChatBot" is going to be an `Auto` taking in a tuple of an incoming nick,
-- message, and timestamp at every step; the result is a "blip stream" that
-- emits with messages whenever it wants to respond.

type Message   = String
type Nick      = String
type ChatBot m = Auto m (Nick, Message, UTCTime) (Blip [Message])


-- Keeps track of last time a nick has spoken, and allows queries
seenBot :: Monad m => ChatBot m
seenBot = proc (nick, msg, time) -> do
    -- seens :: Map Nick UTCTime
    -- Map containing last time each nick has spoken
    seens <- accum addToMap M.empty -< (nick, time)

    -- query :: Blip Nick
    -- blip stream emits whenever someone queries for a last time seen;
    -- emits with the nick queried for
    query <- emitJusts getRequest -< words msg

        -- a function to get a response from a nick query
    let respond :: Nick -> [Message]
        respond qry = case M.lookup qry seens of
                        Just t  -> [qry ++ " last seen at " ++ show t ++ "."]
                        Nothing -> ["No record of " ++ qry ++ "."]

    -- output is, whenever the `query` stream emits, map `respond` to it.
    id -< respond <$> query
  where
    addToMap :: Map Nick UTCTime -> (Nick, UTCTime) -> Map Nick UTCTime
    addToMap mp (nick, time) = M.insert nick time mp
    getRequest ("@seen":request:_) = Just request
    getRequest _                   = Nothing


-- Users can increase and decrease imaginary internet points for other users
karmaBot :: Monad m => ChatBot m
karmaBot = proc (_, msg, _) -> do
    -- karmaBlip :: Blip (Nick, Int)
    -- blip stream emits when someone modifies karma, with nick and increment
    karmaBlip <- emitJusts getComm -< msg

    -- karmas :: Map Nick Int
    -- keeps track of the total karma for each user by updating with karmaBlip
    karmas    <- scanB updateMap M.empty -< karmaBlip

    -- function to look up a nick, if one is asked for
    let lookupKarma :: Nick -> [Message]
        lookupKarma nick = let karm = M.findWithDefault 0 nick karmas
                           in  [nick ++ " has a karma of " ++ show karm ++ "."]

    -- output is, whenever `karmaBlip` stream emits, look up the result
    id -< lookupKarma . fst <$> karmaBlip
  where
    getComm :: String -> Maybe (Nick, Int)
    getComm msg = case words msg of
                    "@addKarma":nick:_ -> Just (nick, 1 )
                    "@subKarma":nick:_ -> Just (nick, -1)
                    "@karma":nick:_    -> Just (nick, 0)
                    _                  -> Nothing
    updateMap :: Map Nick Int -> (Nick, Int) -> Map Nick Int
    updateMap mp (nick, change) = M.insertWith (+) nick change mp


-- Echos inputs prefaced with "@echo"...unless flood limit has been reached
echoBot :: Monad m => ChatBot m
echoBot = proc (nick, msg, time) -> do
    -- echoBlip :: Blip [Message]
    -- blip stream emits when someone wants an echo, with the message
    echoBlip   <- emitJusts getEcho  -< msg

    -- newDayBlip :: Blip UTCTime
    -- blip stream emits whenever the day changes
    newDayBlip <- onChange           -< utctDay time

    -- echoCounts :: Map Nick Int
    -- `countEchos` counts the number of times each user asks for an echo, and
    -- `resetOn` makes it "reset" itself whenever `newDayBlip` emits.
    echoCounts <- resetOn countEchos -< (nick <$ echoBlip, newDayBlip)

        -- has this user flooded today...?
    let hasFlooded = M.lookup nick echoCounts > Just floodLimit
        -- output :: Blip [Message]
        -- blip stream emits whenever someone asks for an echo, limiting flood
        output | hasFlooded = ["No flooding!"] <$ echoBlip
               | otherwise  = echoBlip

    -- output is the `output` blip stream
    id -< output
  where
    floodLimit = 5
    getEcho msg = case words msg of
                    "@echo":xs -> Just [unwords xs]
                    _          -> Nothing
    countEchos :: Auto m (Blip Nick) (Map Nick Int)
    countEchos = scanB countingFunction M.empty
    countingFunction :: Map Nick Int -> Nick -> Map Nick Int
    countingFunction mp nick = M.insertWith (+) nick 1 mp

-- Our final chat bot is the `mconcat` of all the small ones...it forks the
-- input between all three, and mconcats the outputs.
chatBot :: Monad m => ChatBot m
chatBot = mconcat [seenBot, karmaBot, echoBot]

-- Here, our chatbot will automatically serialize itself to "data.dat"
-- whenever it is run.
chatBotSerialized :: ChatBot IO
chatBotSerialized = serializing' "data.dat" chatBot
~~~

Open questions
--------------

*   In principle very little of your program should be over `IO` as a
    monad...but sometimes, it becomes quite convenient for abstraction
    purposes.  Handling IO errors in a robust way isn't quite my strong point,
    and so while almost all `Auto` idioms avoid `IO` and runtime, for some
    applications it might be unavoidable.  Providing industry-grade tools for
    making `IO` robust would be a good next priority.

*   "Safecopy problem"; serialization schemes are implicitly derived, but if
    your program changes, it is unlikely that the new serialization scheme
    will be able to resume something from the old one.  Right now the solution
    is to only serialize small aspects of your program that you *can* manage
    and manipulate directly when changing your program.  A better solution
    might exist.

*   Tests; tests aren't really done yet, sorry!  Working on those :)
