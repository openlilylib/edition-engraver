# usage examples

## Example files

### example-1.ly

This file shows some basic usage of the edition-engraver.

### example-2.ly

The new possibility of adding Slur-, Beam- and some more events is
shown in this file.

## Development files

### development-1.ly

This file introduces and tests functions

```
\startModList
\addModList
\finishModList
```

to add similar edition-mods with a batch-mechanism.

### development-2.ly

In this file the new facility of the edition-engraver to propagate
mods into the next measure if the measure-position exceeds the current
measure-lengh.

*TODO: what about cadanza periods? `Timing.timing = #f` does not have
 any effect right now!*

### development-3.ly

Edition mods are now stored in a structure called `dynamic-tree`. This
adapted tree is able to make use of procedures to implement wildcards
for context-addressing.
