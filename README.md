conway.hs
=========

Conway's [Game of Life][gol], implemented in Haskell!



Compile & Run
-------------

To compile and run:

    $ ghc conway.hs
    $ ./conway

By default, the game begins with a lone glider, venturing out over the barren
cells of infinity...

If this is too sad for your pathetic human heart, edit the `main` monad at the
top of `conway.hs`, and change `glider` in the line

    main = playGame glider


to any of the (currently 3) built-in initial
boards, defined at the bottom of the file:

* `blinker` - a line of 3 cells, which rotates or blinks every turn
* [`glider`][glider] - the simplest spaceship
* `lwss` - a slightly larger spaceship.


Otherwise you can define your own boards, and use them!



<!-- Links -->
[gol]:  http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
[glider]: http://en.wikipedia.org/wiki/Glider_(Conway%27s_Life)
