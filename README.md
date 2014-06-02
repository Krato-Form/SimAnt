Ant Sim
==========================

This is like v0.001 (a first draft that I came up with over the weekend), but this is the kind of direction I want to play with.

Entities
--------

Three entities are involved here:
- Croods (pre-populated, stay fixed),
- Pheros (emitted by Croods, and dropped by Thrants)
- Thrants (move around randomly, following Pheros when possible)

Configuration
---------------

There are a whole lot of configurable parameters, ranging from how many initial Croods and Thrants are created, the energy of each, the size and color when they're drawn, and so on. See the beginning of `thrant.lisp` for the whole list.

Phero emission
---------------

Occasionally (with a configurable probability), Croods might emit Pheros (upto a configurable number) in a radial pattern.

Thrant movement
---------------

A thrant uses some (configurable) amount of energy per move, and there is a configurable amount of movement per unit energy.

It picks one of four possibilities at random, unless there is a Phero nearby (in which case that direction is preferred), or there is a Crood nearby (in which it eats the Crood and gets some energy).

It might also leave a new Phero at its previous position.

When it runs out of energy, it stops.

Dependencies
-------------

This depends on `lispbuilder-sdl`, which you can install using [Quicklisp](http://www.quicklisp.org/). It should work fine on Linux. On OSX, it _more or less works_, except that the SDL screen becomes unresponsive and has to be killed. Yeah, I don't know a fix for that yet.

Future Work
-----------

I'd like to explore 
- pre-built Crood patterns, instead of uniformly distributed ones, and see what patterns the Thrants come up with.
- "way stations" that Thrants move between
- Small state machines that Thrants can use to decide where to go
- Inventory that thrants can carry, to "build up" some structure somewhere
- A 3-d version instead of the current 2-d version, where a more interesting structure can be "assembled".