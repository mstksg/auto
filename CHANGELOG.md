0.4.2.1
-------
<https://github.com/mstksg/auto/releases/tag/v0.4.2.1>

*   Added support for building with GHC 7.6.x.

0.4.2.0
-------
<https://github.com/mstksg/auto/releases/tag/v0.4.2.0>

*   Removed all upper bounds on dependencies.
*   **Control.Auto.Blip**: Companions to `emitJusts` and `onJusts` added, for
    `Either`: `emitEithers` and `onEithers`.  Emit every item inputted, but
    fork them into one of two output blit streams based on `Right` or `Left`
    properties.  Only preserves blip semantics/makes sense if any given
    input's `Right` or `Left`ness is expected to be independent from the last
    received one.
*   **Control.Auto.Blip**: New "blip stream collapsers", `asMaybes` and
    `substituteB`.  `asMaybes` turns a blip stream into a stream of `Maybe`s,
    `Just` when something was emitted, and `Nothing` when not.  `substituteB`
    takes a regular stream and a blip stream, and outputs the values of the
    regular stream whenever the blip stream doesn't emit and the emitted value
    when it does --- basically a more powerful version of `fromBlips`, where
    the "default" value now comes from a stream instead of being always the
    same.
*   **Control.Auto.Blip**: New blip stream creator, `collectN`, which emits
    every `n` steps with the last `n` items received.
*   **Control.Auto.Blip**: New blip stream modifiers, `collectB` and
    `collectBs`.   `collectB` waits on two blip streams and emits after it has
    received something from *both*.  `collectBs` is like `collectN`, except
    emits after every `n` emitted values received with the past `n` emitted
    values.
*   **Control.Auto.Collection**: "Intervaled" counterparts to `mux` and
    `muxMany`, `muxI` and `muxManyI`.  They store `Interval`s instead of
    `Auto`s...and when the `Interval`s turned "off", they are removed from the
    collection.
*   **Control.Auto.Switch**: A new "count-down" switcher, `switchIn`, which
    acts a bit like `(-->)` and `(-?>)`, except the switch happens
    deterministically after a pre-set given number of steps.  Act like the
    first `Auto` for a given number of steps, and then act like the second
    ever after.

0.4.1.0
-------
<https://github.com/mstksg/auto/releases/tag/v0.4.1.0>

*   Adapted to more consistent semantic versioning scheme, where the third
    number is a new update, and the fourth number is reserved for bug fixes.
*   **Control.Auto.Blip**: `foldrB` and `foldlB'` officially **deprecated** in
    their current forms.  From version `0.5`, they will have corrected
    functionality and a new type signature.  The current functionality doesn't
    really make sense, and was a mistake during their implementation.  You can
    begin using the new versions now, with:

    ```
    foldrB  = foldr  (merge f) mempty
    foldlB' = foldl' (merge f) mempty
    ```
*   **Control.Auto.Effects**: New "sealing" mechanisms for underlying
    `Reader`: `sealReaderMVar` and `sealReaderM`.  `sealReaderMVar` allows
    things like "hot swapping" configuration data; at every step, the `Auto`
    asks for its environment from an `MVar`, that could be changed/modified
    from a different thread with new configuration data.  `sealReaderM` is a
    more general/potentially dangerous version where the environment is
    retrieved through an arbitrary action in the underlying monad.
*   **Control.Auto.Run**: New powerful combinator `throughT`, letting you
    "lift" an `Auto` to run over/through any `Traversable`.  Can replace
    `during`, `perBlip`, `accelOverList`, etc.  The specialized versions will
    remain more performant, though.
*   **Control.Auto.Run**: In the spirit of the hip and current Foldable
    Traversable Proposal, `overTraversable` added to complement `overList`, so
    you can now "stream" `Auto`s over `IntMap`s, `Maybe`s, `Const`s...or any
    `Traversable`.  Not replacing `overList` completely, though, for
    performance reasons.
*   **Control.Auto.Blip**: Removed unnecessary `Monad` constraints on
    `became_`, `became'`, `noLonger_`, and `noLonger'`.
*   **Control.Auto.Interval**: Bug fix on `holdFor` and `holdFor_`, where they
    had the potential to overflow `Int` and begin "holding" forever when
    given specifically malformed input.
*   **Control.Auto.Time**: Performance boost on `accelOverList` by using
    strict `Writer` over lazy.


0.4.0.0
-------
<https://github.com/mstksg/auto/releases/tag/v0.4.0.0>

*   Bug fix version *reverting* breaking changes from `0.3.0.0`.  `0.4.x`
    should be able to run all `0.2.x` programs with full backwards
    compatibility.
*   **Control.Auto.Effects**: Reverted back to lazy `StateT` and `WriterT`,
    because of situations where *auto* cannot resolve fixed points for
    recursive bindings.

0.3.0.0
-------
<https://github.com/mstksg/auto/releases/tag/v0.3.0.0>

**DEPRECATED:** Please use `0.4.0.0`!

*   **Control.Auto.Effects**: Breaking change: switched to strict `StateT`
    and `WriterT`.
*   **Control.Auto.Effects**: Added `readerA` and `writerA`, for convenience
    in "creating" `Auto`s under `ReaderT` and `WriterT`; also added `stateA`
    and `accumA` for completeness.

0.2.0.6
-------
<https://github.com/mstksg/auto/releases/tag/v0.2.0.6>

*   **Control.Auto.Run**: As a part of an effort to provide integration with
    *disciplined* effectful streaming, introduced `streamAutoEffects` and
    `toEffectStream`, which convert `Auto m a b`'s to *streams of effects* in
    `m` that can be processed and manipulated and integrated with any
    [`ListT`-compatible library][1], like *pipes*.  See documentation for more
    details.  These were also added to the exports of `Control.Auto`.
*   **Control.Auto.Interval**: New `Auto` `holdJusts`, which stretches the
    last seen "on"/`Just` value over the duration of a "off"/`Nothing`
    interval.
*   Documentation fixes to emphasize *auto*'s focus on *value* streams, not
    *effect* streams, in contrast to *pipes*, *conduit*, etc.
*   Version restrictions on some packages relaxed on *profunctors*,
    *semigroups*, and *base*.

[1]: http://www.haskellforall.com/2014/11/how-to-build-library-agnostic-streaming.html

0.2.0.5
-------
<https://github.com/mstksg/auto/releases/tag/v0.2.0.5>

*   **Control.Auto.Process.Random**: Added combinators and sealers dealing
    for working with an underlying `Rand` or `RandT` monad.
*   Because of this, committed to adding *MonadRandom* as a dependency.

0.2.0.4
-------
<https://github.com/mstksg/auto/releases/tag/v0.2.0.4>

*   **Control.Auto**: Added `unserialize`, `delay`, and `delay_` to
    `Control.Auto`'s exports.
*   **Control.Auto.Blip**: New blip stream manipulator: `forkB`, which forks a
    blip stream into to separate ones based on whether or not the emitted
    values match a predicate.
*   **Control.Auto.Time**: Added a generalized version of `stretch`,
    `stretchAccumBy` which allows access to the "skipped" inputs during the
    stretched periods, as well as the ability to control the outputs during
    the stretched periods.


0.2.0.3
-------
<https://github.com/mstksg/auto/releases/tag/v0.2.0.3>

*   **Control.Auto.Collection**: Bug for `dynZipF` fixed, where newly added
    `Auto`s would overwrite ones alreay stored.
*   **Control.Auto**: `fromInterval` added to `Control.Auto`'s exports.


0.2.0.2
-------
<https://github.com/mstksg/auto/releases/tag/v0.2.0.2>

**DEPRECATED:** Please use `0.2.0.3`!

*   **Control.Auto.Collection**: `dynZipF` and `dynMapF`, implicit-serialization
    dynamic collections.


0.2.0.1
-------
<https://github.com/mstksg/auto/releases/tag/v0.2.0.1>

*   **Control.Auto.Effects**: `catchA` added to `Control.Auto.Effects`,
    allowing explicit catching of runtime exceptions thrown in underlying
    `IO`.


0.2.0.0
-------
<https://github.com/mstksg/auto/releases/tag/v0.2.0.0>

*   First official release.  No backwards-incompatible changes until
    `0.3.0.0`.
