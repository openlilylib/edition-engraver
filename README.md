# edition-engraver

Apply tweaks to LilyPond scores by addressing items externally

This library is part of
[openLilyLib](https://github.com/openlilylib). For information
regarding the installation of openLilyLib please refer to
[oll-core](https://github.com/openlilylib/oll-core).

Jan-Peter Voigt <jp.voigt@gmx.de>

Licence: GPL V3

## What it is

The edition-engraver is used to apply or inject layout modifications
to a score without polluting the musical source with tagged
overrides. The external overrides are themself tagged with an
edition-target-id, so they can be easily activated or deactivated.

## Current state

The current implementation is tested and used in production by a small
group of users.  It is able to apply overrides, context property set,
breaks and some more modifications.

## Development

The edition-engraver shall be developed for relative mods in time -
e.g. apply mod 3/4 quarters beyond mark X. This version is able to
propagate mods if the measure-position exceeds the measure-length. A
mod addressed at 3 5/4 will take effect at 4 1/4 if the current
measure-length is 4/4.

And it shall deal with ids to apply tweaks to designated grobs or
objects.
