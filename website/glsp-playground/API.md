# The GameLisp Playground

The code executed by the playground controls a very small HTML5 game engine. 

First, all of the toplevel forms are evaluated, and the engine reads the following global
variables:

- `play:width` and `play:height` must be integers between 80 and 800. They are the size of the
  back buffer, measured in pixels.
- `play:title` must be a string. It's used as the title for the introduction screen.
- `play:blurb` must be HTML text representing a number of `<p>` elements. Those paragraphs
  are displayed on the introduction screen.

Then, up to once per screen refresh, the engine will invoke the global function `play:update` with
a single argument `dt` - the elapsed time since the last `play:update` invocation, measured
in seconds.

From within `play:update`'s dynamic scope, you can call the following global functions:

    (play:down? button)
    (play:pressed? button)
    (play:released? button)

Returns `#t` if a mouse button or keyboard key is currently down, or if it was pressed or
released at least once during the previous frame interval.

`button` must be one of the symbols `a` through `z`, `up`, `down`, `left`, `right`, `space`,
`enter`, `lmb`, `mmb` or `rmb`.
    
    (play:mouse-x)
    (play:mouse-y)

Returns the current mouse coordinates as integers, relative to the top-left corner of the 
back buffer, measured in back buffer pixels.

    (play:fill x y width height r g b)

Fills a rectangle with a flat colour. All of the arguments are numbers. `r`, `g` and `b` are
clamped to the inclusive range 0 to 255.
    
    (play:draw sprite x y ..flags)

Draws a sprite. `x` and `y` are numbers representing the sprite's top-left corner within the
back buffer. `flags` may include:

- The symbols `hflip` or `vflip`, to flip the sprite horizontally or vertically.
- The symbol `frame` followed by a number, to select a particular frame from a multi-frame
  sprite.

`sprite` must be a symbol which names the sprite. All existing sprites are named somewhere in the
example games. For example, the smiley face at the top of Minefinder is named `face-smile`.
