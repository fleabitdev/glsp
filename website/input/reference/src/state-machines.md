# State Machines

The behaviour of most game entities can be at least partially described by a [state machine].
The door might be opening, open, closing, or closed. The gold ring might be bouncing around, 
flickering as it's about to vanish, or disappearing in a flash of light as it's collected.
The ghost might be playing an animation as it springs out of its hiding-place, or it might be 
slowly gliding towards the player character. The main menu might be fading in, showing the title 
screen, showing the settings screen, or fading out.

[state machine]: https://en.wikipedia.org/wiki/Finite-state_machine

As a Rust programmer, you probably understand better than most the value of state machines for
modelling programs. Rust's `enums` are one of its greatest strengths: they take what would be a 
vague, implicit set of state transitions, and change them into something completely explicit
and obvious which can be statically checked by the compiler.

As we saw in the [Coroutines](coroutines.md) chapter, describing a state machine using traditional
object-oriented code is very challenging. Consider a frog which cycles between crouching, jumping
and croaking:
    
    (defclass Frog
      (field color 'green)

      (field state 'crouching)

      (field state-change-timer 3.0)
      (field next-state 'croaking)

      (field elevation 0.0)
      (field vertical-speed 0.0)
      (const gravity -9.81)

      (met on-step (elapsed)
        (match @state
          ('crouching
            (dec! @state-change-timer elapsed)
            (when (<= 0.0 @state-change-timer)
              (match @next-state
                ('crouching
                  (bail))
                ('jumping
                  (= @vertical-speed 3.0)
                  (= @state 'jumping))
                ('croaking
                  (= @state 'croaking)
                  (= @state-change-timer 2.0)))))

          ('jumping
            (inc! @elevation (* elapsed @vertical-speed))
            (inc! @vertical-speed (* elapsed @gravity))
            (when (>= @elevation 0.0)
              (= @elevation 0.0)
              (= @state 'crouching)
              (= @state-change-timer 1.0)
              (= @next-state 'croaking)))

          ('croaking
            (inc! @state-change-timer elapsed)
            (when (<= 0.0 @state-change-timer)
              (= @state 'crouching)
              (= @state-change-timer 3.0)
              (= @next-state 'jumping))))))

This is code which only a mother could love. The object scatters its state variables across a
mess of different toplevel fields, which stick around even after their state is complete, and
which are sometimes overloaded between one state and the next. The intended lifetime of each 
field is unclear. Understanding the flow of control takes a fair amount of conscious effort. 
Adding in nested state machines, or multiple simultaneous state machines, would be a great
challenge.

On the other hand, if we were to try to formalize this state machine in a language like Java or 
Rust, it could easily become over-engineered. You don't want to create an entirely separate type,
or potentially even a hierarchy of types, just to manage control flow in a forty-line class.

When programming a game, you'll face this dilemma for just about every entity which you come
across. It wouldn't be unusual for a game to contain hundreds, or even thousands, of state 
machines. When they're all defined using code which looks like the above, it's not a pretty sight.

This brings us to GameLisp's `state` clauses:
    
    (defclass Frog
      (field color 'green)

      (fsm
        (state* Crouching
          (field timer)
          (field next-state)

          (init-state ((? @timer 3.0) (? @next-state 'Jumping)))

          (met on-step (elapsed)
            (dec! @timer elapsed)
            (when (<= @timer 0.0)
              (@enab! @next-state))))

        (state Jumping
          (field elevation 0.0)
          (field vertical-speed 3.0)
          (const gravity -9.81)

          (met on-step (elapsed)
            (inc! @elevation (* elapsed @vertical-speed))
            (inc! @vertical-speed (* elapsed @gravity))
            (when (<= @elevation 0.0)
              (@enab! 'Crouching 1.0 'Croaking))))

        (state Croaking
          (field timer 2.0)

          (met on-step (elapsed)
            (dec! @timer elapsed)
            (when (<= @timer 0.0)
              (@enab! 'Crouching))))))


## States

A `state` represents part of a class which can be switched on and switched off.
[`state*` clauses](../std/statex-clause) are enabled by default, whereas
[`state` clauses](../std/state-clause) are disabled by default.

States may contain most of the same clauses which are permitted at the toplevel of a class:
`field`, `const`, `prop`, `met`, classmacros, and so on. The difference is that, while the state 
is disabled, its contents "stop existing". Its fields, constants and properties can't be accessed, 
and its methods can't be invoked.

A state can be enabled by calling the function [`(enab! obj name)`](../std/enab-mut), where 
`name` is the state's name as a symbol, e.g. `'Opening`. Similarly, [`disab!`](../std/disab-mut)
can be used to disable a state, and [`enab?`](../std/enab-p) to test whether a state is currently
enabled.

Within a method, `(@enab! name)` is shorthand for `(enab! @self name)`, and likewise for
`@disab!` and `@enab?`. `@state-name` will return the name of the enclosing state.

    (defclass Gem
      ...

      (state Sparkling
        (met stop-sparkling
          (@disab! @state-name))

        ...))

    (let gem (Gem))
    (prn (enab? gem 'Sparkling)) ; prints #f

    (enab! gem 'Sparkling)
    (prn (enab? gem 'Sparkling)) ; prints #t

    (.stop-sparkling gem)
    (prn (enab? gem 'Sparkling)) ; prints #f

    (.stop-sparkling gem) ; an error


## Finite State Machines

A single `state` by itself is rarely useful. What you usually need is a group of states which are 
mutually exclusive, so that no more than one of the states can be enabled at any given moment.
This can be achieved using an [`fsm` clause](../std/fsm-clause).
    
    (defclass Fighter
      (fsm
        (state* Neutral
          (const defense 50))
        (state Guarding
          (const defense 150))
        (state Staggered
          (const defense 0))))

When a state within an `fsm` clause is about to be enabled, but one of its siblings is already 
enabled, that sibling is automatically disabled first. In this case, if we were to call
`(@enab! 'Guarding)`, it would automatically call `(@disab! 'Neutral)` first.


## Nested States

A `state` clause may appear within another `state` clause, establishing a hierarchy of state
machines.

If you attempt to enable a child state, and its parent is disabled, the parent will automatically
be enabled first. 

Similarly, if you disable a parent state when any of its children are enabled, those child states 
will automatically be disabled first.
    
    (defclass Owl
      (fsm
        (state* Sleeping
          ...
          (met on-startled ()
            (@enab! 'Awake))) ; disables Sleeping, enables Fleeing

        (state Awake
          (fsm
            (state* Fleeing
              ...
              (met on-collide (other)
                (when (is? other TreeBranch)
                  (@enab! 'Perching other)))) ; disables Fleeing
            
            (state Perching
              ...
              (met on-step ()
                (unless (humans-nearby? @self)
                  (@enab! 'Sleeping)))))))) ; disables Perching and Awake



## Initialization and Finalization

A state may include an [`init-state` clause](../std/init-state-clause) and/or a 
[`fini-state` clause](../std/fini-state-clause). These are analogous to the `init` and `fini` 
clauses which can appear at the toplevel of a class.

`init-state` defines a method which is automatically invoked just after the state is enabled. Its 
arguments are the same arguments which were passed to `enab!` or `@enab!`. 

    (defclass Cog
      (fsm
        (state* Immobile
          (met on-activate ()
            (@enab! 'Mobile (rand-pick -1 1))))

        (state Mobile
          (field direction)
          (field rotation-rate)

          (init-state (@direction)
            (= rotation-rate (* @direction 0.3))))))

When a state is enabled automatically by GameLisp (e.g. if it's a parent state whose child state 
is enabled, or if it was defined using `state*` rather than `state`), then its initializer is 
invoked with no arguments.

`fini-state` defines a cleanup method which is automatically called just before the state is 
disabled. It will also be called if the object is 
[killed](object-oriented-programming.md#finalization).

It's possible to call `@enab!` and `@disab!` from within an `init-state` or `fini-state` method,
but it's not recommended. It can make the order of operations confusing, and in the worst 
case it might trigger an endless loop of state changes.

### Errors

If an error occurs during initialization or finalization, the object will be left in an incoherent
state. Child `state*` forms may not have been automatically enabled, a state passed to `disab!` 
may not actually have been disabled, an `init` method may have only been executed halfway, 
and so on.

This is almost never a recoverable situation, so GameLisp takes no chances: if an error bubbles 
through an `init`, `fini`, `init-state` or `fini-state` method, the object is immediately 
[killed](object-oriented-programming.md#finalization) without any finalizers being run.


## Shadowing

If you have a global or local variable bound to the name `dragon`, and you define a new local
variable using `(let dragon ...)`, then any references to the name `dragon` will refer to the
new binding rather than the older bindings. We say that the later local binding "shadows" the
earlier bindings.

The same is true for fields and constants in states. It's possible for a name to be bound by
several different states at the same time, all of which might be simultaneously enabled.
Under those circumstances, when GameLisp evaluates an expression like `@dragon` or `[obj 'dragon]`,
it needs to choose which binding takes priority.

The rules are:
    
- Names in child states will shadow names defined by their parent.

- Names in any state will shadow names defined by sibling states which appear textually earlier 
  in the `class` definition.

These are essentially the same rules which govern local variable bindings.

    (defclass ShoppingCentre
      (const tax-revenue 10_000)

      (state WellKnown
        (const tax-revenue 15_000))

      (state Damaged
        (const tax-revenue 2_000)

        (state Demolished
          (const tax-revenue 0))))

    (let shops (ShoppingCentre))
    (prn [shops 'tax-revenue]) ; prints 10000

    (enab! shops 'Demolished)
    (prn [shops 'tax-revenue]) ; prints 0

    (enab! shops 'WellKnown)
    (prn [shops 'tax-revenue]) ; prints 0


### Fully-Qualified Names

If you need to access a field or constant in a specific state, you can use its fully-qualified 
name, `StateName:field-name`, to bypass the normal shadowing rules. For the purpose of name lookup, 
all fields and constants defined in the toplevel of a class are considered to belong to a `Main`
state which can never be disabled.
    
    (defclass FancyChair
      (const comfort-points 75)
      (const room-points 40)
      (const fun-points 5)

      (state Grubby
        (const comfort-points 40)
        (const room-points 10)

        (state Filthy
          (const room-points -30))))

    (let chair (FancyChair))
    (enab! chair 'Filthy)

    (prn [chair 'comfort-points]) ; prints 40
    (prn [chair 'room-points]) ; prints -30
    (prn [chair 'fun-points]) ; prints 5

    (prn [chair 'Main:comfort-points]) ; prints 75
    (prn [chair 'Grubby:room-points]) ; prints 10
    (prn [chair 'Filthy:room-points]) ; prints -30

This highlights a quirky detail of how state namespaces work: state names don't actually form a 
hierarchy. A state `Child` defined within the state `Parent` defined within the `Main` state is 
just called `Child`, rather than `Main:Parent:Child`. This means that you can't 
simultaneously have, say, `Defending:KnockedBack` and `Attacking:KnockedBack` - that would be a 
name collision, because both states are actually just named `KnockedBack`.

This is a deliberate design choice. Being able to define multiple different states which share
the same name would be confusing, and typing out fully-qualified names would be too much effort.
Flattening the namespace hierarchy is an effective solution.


## Wrapper Methods

You will sometimes want to change the behaviour of a method depending on which states are enabled.
For example, when the main character is being controlled by a cutscene script rather than being 
directly controlled by the player, you might want to override their `on-input` event handler to 
do nothing.

A naive attempt to achieve this using name shadowing will fail:
    
    (defclass Character
      (met on-input (input-event)
        (match [input-event 'tag]
          ('left (@walk-left))
          ('right (@walk-right))
          ('pause (game:pause))))
      
      (state CutsceneControl
        (met on-input (input-event)
          ; do nothing
          #n)))

    (let mc (Character))
    (enab! mc 'CutsceneControl) ; error: name collision for 'on-input

It's not possible to have multiple active `met` forms which share the same name. This is because, 
although name-shadowing is adequate for fields and constants, it's not powerful enough for methods.
We provide a better alternative.

A [`(wrap ...)` clause](../std/wrap-clause) defines a "wrapper method": a method which replaces, 
and modifies, a method in another state.
    
    (defclass Character
      (met on-input (input-event)
        (match [input-event 'tag]
          ('left (@walk-left))
          ('right (@walk-right))
          ('pause (game:pause))))
      
      (state CutsceneControl
        (wrap Main:on-input (input-event)
          ; do nothing
          #n)))

In this case, when the `CutsceneControl` state is active, any calls to `(.on-input ch ev)` will be 
routed to the wrapper method in `CutsceneControl`. It would still be possible to invoke the 
original method using its fully-qualified name: `(.Main:on-input ch ev)` or `(@Main:on-input ev)`.

Within our wrapper method, we can invoke the original `met on-input` by calling 
[`(@base)`](../std/atsign-base). This is a versatile tool. We could ignore the base method 
altogether, execute some additional code before or after calling `(@base)`, transform the 
base method's arguments or return value, or even call the base method multiple times!

For example, some cutscenes might want to give the player a limited ability to move the main
character around, but still forbid them from opening the pause menu. This would be easy to achieve 
using `(@base)`:
    
    (defclass Character
      (met on-input (input-event)
        (match [input-event 'tag]
          ('left (@walk-left))
          ('right (@walk-right))
          ('pause (game:pause))))
      
      (state CutsceneControl
        (wrap Main:on-input (input-event)
          (unless (eq? [input-event 'tag] 'pause)
            (@base input-event))))

### Chained Wrappers

You will have noticed that the `wrap` clause receives a fully-qualified name for its target
method: in this case, `Main:on-input`. 

The target is usually a `met` form, but it's also possible to recursively wrap another `wrap` 
form. The wrapper methods form an orderly stack, with each `(@base)` call moving down the stack 
until it reaches the `met`.

Let's suppose that we're writing an action game (or a business simulation game?) with a 
`BerserkerBoss` entity who turns progressively more red and angry as the encounter goes on:
    
    (defclass BerserkerBoss
      (met ruddiness ()
        (+ @attacks-received @henchmen-defeated))

      (state Angry
        (wrap Main:ruddiness ()
          (match @difficulty-level
            ('easy
              (* 1.2 (@base)))
            ('hard
              (* 1.4 (+ 3 (@base))))))

        (state Furious
          (wrap Angry:ruddiness ()
            (* 1.5 (@base))))))

When the `Furious` state is enabled, its parent state `Angry` must also be enabled. The
original definition of `ruddiness` is wrapped by `Angry:ruddiness`, which is in turn wrapped
by `Furious:ruddiness`, so a non-specific call to the `ruddiness` method will end up invoking
`Furious:ruddiness`. Each wrapper delegates down the chain through successive calls to `(@base)`.

### Property Wrappers

You can wrap a property in much the same way that you might wrap a method. Simply define a
`(wrap-prop ...)` clause. If we wanted to refactor `ruddiness` to be a property rather than a 
method, we would write:
    
    (defclass BerserkerBoss
      (prop ruddiness (get (+ @attacks-received @henchmen-defeated)))

      (state Angry
        (wrap-prop Main:ruddiness
          (get
            (match @difficulty-level
              ('easy
                (* 1.2 (@base)))
              ('hard
                (* 1.4 (+ 3 (@base)))))))

        (state Furious
          (wrap-prop Angry:ruddiness (get (* 1.5 (@base)))))))

Property wrappers can't use the `@field` shorthand to access the original property's backing
storage. Instead, they should invoke the original getter or setter using `(@base)`.


## Zombie Methods

A state may be disabled partway through executing one of its own methods. Similarly, an object
may call `(obj-kill! @self)` from within one of its own methods. In both cases, this will land you
in an awkward grey area. Code will be executing which *appears* to belong to a state or object,
even though the state or object no longer exists. Under those circumstances, any `@name` 
field accesses or `(@name)` method calls will usually trigger an error. I call this situation a 
"zombie method".
    
    (defclass Person
      (met check-health ()
        (when (<= @health 0)
          (obj-kill! @self))

        ; execution continues after the object is killed. braaains...

        (when (== @health 100) ; error: nonexistent field 'health
          (prn "feeling pretty healthy!"))))

(For the record, this is a problem which already exists in many game state machines - it's just 
something which GameLisp makes explicit, rather than leaving it as a silent logic error.)

There's  no practical way for GameLisp to prevent this. It would require dynamic checks
to be inserted every time control flow leaves the body of a method, which would be difficult 
to implement and carry a huge performance cost.

Zombie methods tend to fail loudly rather than introducing subtle bugs, so they're mostly just
an annoyance. You could consider using two techniques to make zombie methods less common:

- Get into the habit of calling [`return`](../std/return) immediately after 
  `(@disab! 'CurrentStateName)`. You might like to combine the two calls by defining a 
  macro - perhaps `(done)`, `(end)`, or `(switch-to state-name)`.

- Postpone any `(obj-kill!)` calls until the very end of each frame. (As it so happens, most game 
  engines already do this by default. Entity deletion is a common source of bugs, probably 99% of 
  which can be prevented by postponing the deletion.)
