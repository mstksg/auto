0.2.0.6
-------
<https://github.com/mstksg/auto/releases/tag/v0.2.0.6>

*   **Control.Auto.Run**: As a part of an effort to provide integration with
    *disciplined* effectful streaming, introduced `streamAutoEffects` and
    `toEffectStream`, which convert `Auto m a b`'s to *streams of effects* in
    `m` that can be processed and manipulated and integrated with any
    [`ListT`-compatible library][1], like *pipes*.  See documentation for more
    details.
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

*   First official release.  No backwards-incompatible changes until 0.3.0.0.
