# Garbage Collection

GameLisp provides automatic memory management, in the form of a tracing garbage collector (GC). 
This means that there's no need to worry about cyclic references - orphaned reference cycles 
will be collected automatically.

Traditional tracing GCs wait until a certain amount of memory has been allocated, 
and then attempt to scan or deallocate a large amount of memory all at once. Even incremental 
or concurrent GCs tend to wait for memory pressure to reach a certain level, switch 
themselves on for a while, and then switch themselves off - examples include [Unity's upcoming 
incremental collector][1], and the garbage collector currently provided by Lua and LuaJIT.

[1]: https://blogs.unity3d.com/2018/11/26/feature-preview-incremental-garbage-collection/

None of these options are the right choice for game development. In order for a fast-paced game to 
avoid feeling "choppy", it *must* meet the deadline set by the monitor's refresh rate, or an 
integer fraction of that refresh rate (say, 72 Hz for a 144 Hz monitor). A swap chain can allow 
the game to accumulate one or two frames of "borrowed time" without being detectable to the 
user, but whenever the cumulative delay exceeds those one or two frames, the debt is cashed in 
and the user will experience a frameskip.

(This is a simple thing to test - in your main loop, just sleep for 15ms every 40th frame,
which is equivalent to exceeding a 60 Hz frame budget by about 0.3ms per frame. If your game 
involves any fast movement, you'll find that it suddenly "feels wrong", even if you can't pinpoint 
exactly which frames are being skipped.)

When a game's GC is allowed to stop and start, and it takes up one millisecond per frame while
it's running, then the time budget is effectively one millisecond lower for *every* frame. If a 
particular scene normally runs at 16 milliseconds per frame, then when the garbage collector 
switches on it will suddenly take up 17 milliseconds and start skipping frames. In this scenario,
you'd be forced to set aside 6% of your entire time budget just for the GC!

The status quo is usually even worse - when a GC is allowed to "stop the world", it might pause
your game for 50 milliseconds or more. I've heard anecdotal reports of games experiencing GC 
pauses of several hundred milliseconds. This would be clearly noticeable even in a slow, casual 
game.


## GameLisp's GC

GameLisp's solution is a custom incremental GC algorithm which runs once per frame, every frame. 
The amount of work it performs increases in lockstep with the amount of new memory allocated 
during the previous frame (which, for a typical game, should be very consistent).

It turns out that when you spread out the work like this, garbage collection is extremely cheap.
[Appendix A](performance-figures.md) has some concrete performance numbers, but the headline figure
is that for a 2D sidescroller running on a mid-range laptop, the GC uses about 0.1 milliseconds
per frame, or 0.2 milliseconds for a really busy scene.

GameLisp will never invoke the GC behind your back. You need to do it explicitly, either by
calling the [`glsp::gc`] function from Rust, or the [`gc`](../std/gc) function from GameLisp. 

For a typical main loop, you should simply invoke the GC once per frame. If you're scripting a
program with an unusual main loop (say, a GUI event loop for your level editor), then you
should aim to invoke the GC about sixty times per second, although it's fine to temporarily stop
when your program isn't executing any GameLisp code.

There's currently only one way to tune the GC's behaviour: calling [`glsp::gc_set_ratio`] or 
`(= (gc-value 'ratio) r)` to assign a "heap ratio". This is the ratio between the average size of 
the GC heap, and the amount of long-lived memory which it stores. The [default value] is 
`1.5`, so if you store 10mb of useful long-lived data on the heap, it will also contain about 5mb 
of garbage.

When you set the ratio to a lower value, the GC will need to perform an exponentially higher
amount of work to keep up. The [minimum ratio] is currently `1.2`.

[`glsp::gc`]: https://docs.rs/glsp/*/glsp/fn.gc.html
[`glsp::gc_set_ratio`]: https://docs.rs/glsp/*/glsp/fn.gc_set_ratio.html
[default value]: https://docs.rs/glsp/*/glsp/constant.GC_DEFAULT_RATIO.html
[minimum ratio]: https://docs.rs/glsp/*/glsp/constant.GC_MIN_RATIO.html
