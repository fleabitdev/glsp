# Code Reuse

In game development, you'll often have a small piece of data and behaviour which you need to 
duplicate between many different classes. In a go-kart racing game, you might require all entities 
to register their bounding box with the collision system; or in a city simulator, you might have 
many different buildings which have their own population count and tax revenue; or in a 3D
exploration game, you may wish to plug hundreds of diverse entities into the same animation system.

You already have several ways to achieve this sort of code reuse:

- In many cases, the code can simply be [refactored into a free function]. "The `on-grow` method
  for all of my fruit tree classes is identical - they should be calling a shared `grow-fruit-tree`
  function instead."

- [Duck typing] can be an elegant solution for some simple cases, but you normally can't use it to 
  uphold complex invariants. "If I'm creating an entity, and I notice that has a `target-rect` 
  field, I will automatically register it with the combat-targeting system."

- [Composition over inheritance] will explicitly fracture your classes into small, reusable
  components which are separate from the whole. This will certainly make each component reusable,
  but it can be a hassle. "When I create a `Skyscraper`, it automatically creates a new
  `PopulationNode` and `TaxNode`."

- If the only thing the entities have in common is that they all belong to the same general 
  category, and some external system needs to manipulate them all in the same way, then you could 
  implement a tag database. "Each frame, my `Lava` entity looks up any nearby entities with the 
  `heat-sensitive` tag, and calls their `on-heat` method."

- Consider whether the differences between your entities can be described using plain data rather 
  than code. If you were to reimplement the original [Final Fantasy] in GameLisp, you wouldn't 
  need distinct classes for an `Ogre` and a `Creep` - you would just have a single `Enemy` class 
  which makes use of plain data like "hit points" and "sprite size". Likewise, if you were to 
  reimplement [The Sims], all of the different doorways would probably be modelled as a single 
  `Door` class which is capable of displaying a few different sprites.

[refactored into a free function]: https://twitter.com/ID_AA_Carmack/status/53512300451201024
[Duck typing]: https://en.wikipedia.org/wiki/Duck_typing
[Composition over inheritance]: https://en.wikipedia.org/wiki/Composition_over_inheritance
[Final Fantasy]: https://youtu.be/fTdlzqhSdt8?t=2879
[The Sims]: https://youtu.be/NG7lgdJNgB0?t=1806

Occasionally, none of those options will be sufficient. This usually happens when a component
is shared by a very large number of entities (dozens or hundreds), and those entities need to 
frequently interact with the component in some intrusive way, so splitting it into a separate 
object would make things too bureaucratic. For example...

- In an action-adventure game, we don't want to type out `[@collider 'coords]` every single time 
  an entity needs to know its own coordinates; we just want to type `@coords` instead. On the other 
  hand, an entity's physical location is a very fragile thing (if you get it wrong, entities will 
  start clipping through the scenery!), so our physics system shouldn't just manipulate a bare 
  `@coords` field using duck typing.

- In a theme-park simulation game, the code for saving and loading the game needs to be able
  to serialize and deserialize most of the game's entities. Explicitly writing `serialize` and
  `deserialize` methods for every building, every visitor and every worker would be 
  labour-intensive and tedious.

- In a massively-multiplayer online game, the code for scripting the user-interface might need
  to filter or intercept certain methods. For example, if a user-interface element has scrollbars, 
  then the arguments to its `on-mouse-click` method should be adjusted to give the illusion that 
  it belongs to a different coordinate system. If each new type of user-interface element was 
  forced to convert between coordinate systems manually, you would end up writing a lot of extra 
  code, and bugs would certainly creep in.

In situations like these, your first port of call should be a macro or a 
[classmacro](object-oriented-programming.md#classmacros). It would be straightforward to write 
a `defentity` macro which acts like `defclass`, but emits a few extra clauses to hook the 
resulting class into the savegame system and the collision system.

If you find that even macros aren't powerful enough, GameLisp does have one more trick up
its sleeve.


## Mixins

A mixin is a small class which can't be instantiated. Instead, it can only be "mixed into" the
definition of another class. The target class will incorporate all of the mixin's fields, methods, 
states, and so on, almost as though they were simply copied and pasted at the beginning of the 
class definition.
	
	(defmixin Sized
	  (field width)
	  (field height))

	(defclass Box
	  (mixin Sized)
	  (init (@width @height)))

	(let box (Box 20 15))
	(prn [box 'width] [box 'height]) ; prints 20 15
	(prn (is? box Box) (is? box Sized)) ; prints #t #t

This is similar to a classmacro, but it comes with several advantages:

- Mixins can customize their object's initialization and finalization. (This would be difficult
  to achieve using a classmacro, since each class may only have a single `init` clause and a
  single `fini` clause, which can't normally be wrapped.)

- Mixins introduce a new namespace. If your `SoftBody` mixin specifically wants to override a 
  method introduced by your `Collider` mixin, it can include the clause 
  `(wrap Collider:something ...)`.

- As demonstrated above, the `(is? obj class)` function can be used to test whether an object's 
  class implements a particular mixin.

<span></span>

	; a mixin which adds a `coords` property and a `move` method to a class, 
	; and ensures that the collision system is kept up-to-date whenever the 
	; coords are changed.
	(defmixin Coords
	  (prop coords 
	    (get)
	    (set (new-coords)
	      (= @field new-coords)
	      (colliders:update @self)))

	  (met move (dx dy)
	    (colliders:move @self dx dy))

	  (init-mixin (..args)
	    (@base ..args)
	    (colliders:register @self))

	  (fini-mixin ()
	    (colliders:unregister @self)))


## Advanced Wrapper Methods

In the [previous chapter](state-machines.md), we discussed wrapper methods, which can be used to 
override a specific `met` or `wrap` clause defined elsewhere in the class.

So far, the obvious limitation of wrapper methods is that they require you to know the entire 
structure of your class up front. There's a `Jumping` state, which wraps the `Active:energy-level` 
property, which wraps the `Main:energy-level` property...

Let's suppose you have an `on-step` method, and you want to write a mixin which spawns a particle 
effect every step, without replacing or modifying the entity's normal behaviour. GameLisp gives you
two options for achieving that.

The first option:
	
	(defmixin Cloudy
	  (wrap Main:on-step ()
	    (@base)
	    (spawn-particle @coords 'clouds)))

If `Main:on-step` is undefined, or if you include multiple states or mixins which all try to 
override `Main:on-step`, or if a state other than `Main` tries to define a `met on-step`, an 
error will occur. This is normally a good thing! It highlights the fact that your code has 
an ambiguous order of execution, and it prompts you to disambiguate it by, for example, changing 
one of your wrapper methods to override `Cloudy:on-step` instead.

In cases where you're absolutely sure that you don't care about the order of execution, you could
consider the second option:

	(defmixin Cloudy
	  (wrap _:on-step ()
	    (@base)
	    (spawn-particle @coords 'clouds)))

The underscore makes this a "wildcard wrapper method". It means "I want this code to be executed 
when `on-step` is called, but I don't care about what happens before or after".

Wildcard wrappers are much less strict than explicit wrappers. It's fine to have a wildcard
wrapper for `_:on-step`, even when there is no actual `met on-step` anywhere in the class. 
If there is no other `on-step` method, or if it's disabled, `(@base)` will be a silent no-op. 
There can be any number of `_:on-step` wrappers in each class; you can even put several in the 
same state!

Wildcard wrappers can only be invoked using their unqualified method name: `(.Cloudy:on-step ob)`
would fail, but `(.on-step ob)` would succeed. When you call an unqualified method like `on-step`,
`(@base)` will chain through all of the wildcard wrappers in an unspecified order, followed by all 
of the explicit wrappers, followed by the `met` form. Methods which belong to disabled states are 
skipped.

Although wildcard wrappers can lead to spaghetti code when overused, they're a powerful tool when
used responsibly.
	
	(class Monster
	  (met on-inspect ()
	    (prn "It's terrifying!"))
	  
	  (state OnFire
	    (wrap _:on-inspect ()
		  (@base)
		  (prn "Also, it's on fire!")))
	  
	  (state Howling
	    (wrap _:on-inspect ()
		  (@base)
		  (prn "It's howling, too!"))))


## Initialization and Finalization

Mixins are initialized using an [`init-mixin` clause](../std/init-mixin-clause), which defines a 
wrapper for the class's initializer method. If a class has three mixins and an `init` clause...
	
	(defclass
	  (mixin A B C)
	  (init
	    ...))

...then it effectively has a hidden initializer method `Main:init`, which is wrapped by 
`C:init-mixin`, which is wrapped by `B:init-mixin`, which is wrapped by `A:init-mixin`.

Like any other wrapper method, `init-mixin` is versatile. It can intercept leading or trailing
initializer arguments, modify arguments, and execute arbitrary code before or after calling 
`(@base)`. The only thing it's not capable of doing is inverting the flow of information - a class 
can't decide which arguments to pass to each of its mixins - but this is a deliberate design 
choice.

Finalization is simpler than initialization. When an object is killed, GameLisp will first call
the object's `fini` method, and then call [`fini-mixin`](../std/fini-mixin-clause) for each mixin 
from right to left. `fini-mixin` isn't a wrapper method, so it doesn't need to call `(@base)`.


## States in Mixins

Mixins may define states. However, it's an error for a mixin to define a state which is also
defined by the target class, or by another mixin. GameLisp provides no way to mix two states 
together, simply because it would be too confusing.

For similar reasons, mixins don't [shadow](state-machines.md#shadowing) their implementing class. 
If the toplevel of a mixin defines a field or constant which is also defined by the `Main` state 
of its implementing class, it's an error.

If a mixin defines a state, that state will participate in name-shadowing as normal, as though
it was copied-and-pasted in at the very start of the implementing class.
	
	(defmixin Heavy
	  (const kg 1000)
	  (state* Burdened
	    (const kg 1100)))

	; this is an error, because the name Weight:kg collides with Heavy:kg.
	; if Heavy's (const kg 1000) were commented out, the code would compile.
	(defclass Weight
	  (mixin Heavy)
	  (const kg 500))

### Mixin States

It's ordinarily an error for a mixin and a state to share the same name, because it would cause
a namespace collision:
	
	; which one of these two fields is named `Fighting:health`?
	(mixin Fighting
	  (field health)
	  (state Fighting
	    (field health)))

However, there's a special exception when a mixin only contains a single `state` or `state*`,
with no other clauses. In that case, the state "takes over" the mixin's namespace. In effect, you 
end up with a mixin which can be dynamically enabled and disabled - a useful abstraction.


## Aside: Why Not Inheritance?

Most object-oriented languages in common use include an inheritance hierarchy. Code reuse is
achieved by designating one or more "parent classes" for each class. All of the fields and methods
in the parent class are incorporated into the child class.

I find that this is often an unhelpful abstraction, for two main reasons:

- The boundary between a parent class and its children can be fiendishly difficult to manage.
  Designing `ClassA` so that it extends and improves `ClassB` sounds deceptively straightforward,
  but in reality it's anything but.

- The problems with multiple inheritance are well-documented, but single inheritance is too
  limited. It tends to create awkward, towering inheritance hierarchies, bringing in many
  features which the final object doesn't actually need.

Mixins vaguely resemble an inheritance hierarchy, but they're deliberately simpler. Mixins can't 
include or require other mixins, so they form a flat list rather than a tree. Mixins don't have
a general ability to override or shadow everything in the implementing class - they just have a 
limited ability to override initialization, finalization and methods. By convention, mixins are 
also much smaller than base classes: a mixin should define a small, reusable piece of code,
rather than defining the entire foundation upon which another class will be built.

### Emulating Inheritance

One thing which single inheritance excels at is defining multiple classes which are almost
identical, but with only small differences in their logic. In a military-strategy game, if you 
have a red soldier with an aggressive AI and a musket, and a blue soldier with a defensive AI and a
pike, then it would be natural to model them as:

```java
class Soldier extends Entity { ... }
class RedSoldier extends Soldier { ... }
class BlueSolider extends Soldier { ... }
```

If you're trying to achieve something like this in GameLisp, I would strongly advise against
defining a `Soldier` mixin. Mixins aren't designed to be used for inheritance; you'll be able
to make it work with a little effort, but it won't be elegant.

Instead, you should use runtime configuration: a single `Soldier` class which accepts a `color`
parameter. States make it easy to compartmentalize the class into two sub-types.

	(defclass Soldier
	  (init (color)
	    (match color
	      ('red (@enab! 'Red))
	      ('blue (@enab! 'Blue))
	      (_ (bail))))

	  (state Red
	    (const weapon-name 'musket)
	    (met select-action ()
	      ...))

	  (state Blue
	    (const weapon-name 'pike)
	    (met select-action ()
	      ...)))
