0.2.0.6
-------
<https://github.com/mstksg/auto/releases/tag/v0.2.0.6>

*   *Control.Auto.Interval*: Added new combinator `holdJusts` dealing with
    intervals; stretches the last received "on"/`Just` value over the duration
    of any "off"/`Nothing` periods.
*   Some documentation changes to the index page and README that were not
    fixed with a manual re-upload.

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
