conway.hs
=========

Conway's [Game of Life][gol], implemented in Haskell!



Compile & Run
-------------

To compile and run:

    $ ghc conway.hs
    $ ./conway



Playing the Game
----------------

By default, the game begins with a lone glider, venturing out over the barren
cells of infinity...

If this is too sad for your pathetic human heart to bear, then edit the `main`
monad at the top of `conway.hs`, changing `glider` in the line:

    main = playGame glider


to any of the (currently 3) built-in initial
boards, defined at the bottom of the file:

* `blinker` - a line of 3 cells, which rotates or blinks every turn
* [`glider`][glider] - the simplest spaceship
* `lwss` - a slightly larger spaceship.


Otherwise you can define your own boards, and use them!


How it Works
------------

We'll ignore '¡EL MONSTER GRAPHICS ENGINE!' for now, since, it's somewhat ah.. incomplete, shall we say?

No, all the magic happens in the `iterateBoard` function! When it gets a
non-empty board, it generates a list of all the cells in that board which could
change in the next turn. And for each of those cells, it counts up how many of
its neighbours are alive right now.

By Conway's rules of life:

* If a living cell has less than 2 neighbours, it dies of
    loneliness (cue: 'Awwww's!)
* If it has more than 3 neighbours, then they step on each others toes or
    something - and the poor cell dies then, too!
* But if a living cell has just 2 or 3 neighbours, everything is fine and dandy.
* In fact, if an empty cell has exactly 3 neighbours, then something beautiful
    happens - a living cell appears between the 3 of them (it's acceptable in
    some cultures!)

All the above happens in the `iterateCell` function.

Everything else is really just a (complicated, computationally inefficient...)
helper function! Just the definitions down the page to get to the bottom of it
all, so to speak...

<!-- Links -->
[gol]:  http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
[glider]: http://en.wikipedia.org/wiki/Glider_(Conway%27s_Life)
