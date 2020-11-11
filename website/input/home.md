**GameLisp** is a scripting language for [Rust](https://rust-lang.org) game development. It was 
created while working on [The&nbsp;Castle&nbsp;on&nbsp;Fire](tcof/).

- **No garbage collection pauses**. GameLisp has a unique garbage collector designed specifically 
  for game development. It&nbsp;runs once per frame, every frame, without causing any latency 
  spikes.

- **Seamless Rust API**. Integrating GameLisp into a Rust codebase is effortless, thanks to
  Rust's powerful type system. Installation is trivial - it's just a 
  [crate](https://crates.io/crates/glsp/)!

- **Memory-safe**. GameLisp is implemented entirely in Rust, with very few dependencies. 
  By&nbsp;default, its implementation doesn't use `unsafe` at all.

- **Feature-rich**. GameLisp has all of the convenience features you might expect from&nbsp;a 
  modern language. [Pattern&#8209;matching](reference/patterns.html), 
  [iterators](reference/iterators.html), [coroutines](reference/coroutines.html), 
  [macros](reference/macros.html)...

- **Easy entity scripting**. GameLisp has a unique object system, built around 
  [state&nbsp;machines](reference/state-machines.html) and [mixins](reference/code-reuse.html),
  designed specifically for scripting game entities.

If you're interested, take a look at the [Getting Started](reference/overview.html) page for 
more information.

<pre><code>(<b>defstruct</b> Rect
  x y w h

  (<b>met</b> overlaps? (other-rect)
    (<b>let</b> [x y w h] other-rect)
    (and (< @x (+ x w))
         (< x (+ @x @w))
         (< @y (+ y h))
         (< y (+ @y @h)))))

(<b>def</b> paddle-speed 220)
(<b>def</b> paddle-height 40)
(<b>def</b> paddle-start-y (-> play:height (- paddle-height) (/ 2)))

(<b>def</b> left-paddle (Rect
  (x 10)
  (y paddle-start-y)
  (w 6)
  (h paddle-height)))

(<b>def</b> right-paddle (Rect
  (x (- play:width 16)) 
  (y paddle-start-y)
  (w 6)
  (h paddle-height)))

(<b>def</b> ball-start-x (-> play:width (/ 2) (- 3)))
(<b>def</b> ball-start-y (-> play:height (/ 2) (- 3)))

(<b>def</b> ball (Rect
  (x ball-start-x)
  (y ball-start-y)
  (w 6)
  (h 6)))

(<b>def</b> ball-dx 0)
(<b>def</b> ball-dy 0)

(<b>defn</b> play:update (dt)

  ; update the paddles
  (<b>for</b> (paddle up-key down-key) in `((~left-paddle w s)
                                     (~right-paddle up down))
    (<b>when</b> (play:down? up-key)
      (dec! [paddle 'y] (* dt paddle-speed)))

    (<b>when</b> (play:down? down-key)
      (inc! [paddle 'y] (* dt paddle-speed)))

    (clamp! [paddle 'y] 0 (- play:height paddle-height)))

  ; update the ball
  (<b>when</b> (and (== ball-dx ball-dy 0)
             (any? play:pressed? '(w s up down)))
    (= ball-dx (* (rand-select -1 1) (rand 170 210)))
    (= ball-dy (* (rand-select -1 1) (rand 50 100))))

  (inc! [ball 'x] (* dt ball-dx))
  (inc! [ball 'y] (* dt ball-dy))

  (<b>when</b> (< [ball 'y] 0)
    (= ball-dy (abs ball-dy)))

  (<b>when</b> (>= (+ [ball 'y] [ball 'h]) play:height)
    (= ball-dy (- (abs ball-dy))))

  (<b>when</b> (or (and (.overlaps? ball left-paddle) (< ball-dx 0))
            (and (.overlaps? ball right-paddle) (> ball-dx 0)))
    (= ball-dx (- (* ball-dx (rand 1.03 1.08))))
    (inc! ball-dy (rand 50 -50))
    (clamp! ball-dy (- (abs ball-dx)) (abs ball-dx)))

  (<b>unless</b> (<= 0 [ball 'x] play:width)
    (= [ball 'x] ball-start-x)
    (= [ball 'y] ball-start-y)
    (= ball-dx 0)
    (= ball-dy 0))

  ; rendering
  (<b>let</b> midnight-blue '(25 25 112))
  (<b>let</b> turquoise '(64 224 208))

  (play:fill 0 0 play:width play:height ..midnight-blue)
  (play:fill ..[ball '(x y w h)] ..turquoise)
  (play:fill ..[left-paddle '(x y w h)] ..turquoise)
  (play:fill ..[right-paddle '(x y w h)] ..turquoise))

</code></pre>

The above code implements a simple game. Give it a try on the [playground](playground/#tennis)!
