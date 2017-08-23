# edition-engraver
Apply tweaks to LilyPond scores by addressing items externally

This library is part of [openLilyLib](https://github.com/openlilylib). For information
regarding the installation of openLilyLib please refer to [oll-core](https://github.com/openlilylib/oll-core).

Jan-Peter Voigt <jp.voigt@gmx.de>

Licence: GPL V3

# What it is
The edition-engraver is used to apply or inject layout modifications to a score without
polluting the musical source with tagged overrides. The external overrides are themself
tagged with an edition-target-id, so they can be easily activated or deactivated.

# Current state
The current implementation is developed from scratch. It is now (2016-03-08) able to do
almost the same, as the edition-engraver found in `openlilylib/editorial-tools/edition-engraver`
It should be seen as an alpha-version.

# Development
The edition-engraver shall be developed for relative mods in time - e.g. apply mod 3/4
quarters beyond mark X. And it shall deal with ids to apply tweaks to designated grobs
or objects.
