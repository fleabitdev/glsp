# Rust Bindings

In game development, scripting languages like GameLisp are used to provide a simple, convenient
interface to the game engine. If you need to change some small detail of your game, you'll have 
a much more pleasant experience editing a GameLisp or Lua script, rather than directly hacking 
on your engine's Rust or C++ code.

Most game engines will, therefore, include sprawling cross-language bindings which enable large
parts of the engine to be controlled by scripts. Scripts might need to construct native types, 
invoke native functions and methods, manipulate global data, and otherwise exert fine control 
over the engine.

Historically, this has been - not to put too fine a point on it - terrible. The APIs to bind
a native language to a scripting language usually feel like an afterthought. They're often
awkward to use, unsafe, and incomplete. This friction between the native language and the 
scripting language can act as a large, unnecessary maintenance burden.

Before discussing GameLisp's approach to this problem, let's take a tour of some existing
scripting language bindings. The task is simple: Define a global function which can be used to 
construct an empty OpenGL 2D texture, returning it as a scripting-language object. Then, attach 
two methods to that object so that the texture's width and height can be queried by scripts.


## Lua

[Lua](https://lua.org/) is the most popular game scripting language in existence.

Because Lua is written in C, its built-in foreign function interface comes with some major 
ergonomic problems. Rather than being based around normal function calls, arguments and return 
values, it instead requires the user to indirectly manipulate an opaque "stack" of Lua values.

```c
typedef struct Texture {
	GLuint tex_id;
	GLsizei width;
	GLsizei height;
} Texture;

static int create_texture(lua_State* L) {
	/* receive two integer arguments as "width" and "height" */
	int width = luaL_checkinteger(L, 1);
	int height = luaL_checkinteger(L, 2);

	/* generate the OpenGL texture object itself */
	GLuint tex_id = 0;
	glGenTextures(1, &tex_id);
	glBindTexture(GL_TEXTURE_2D, tex_id);
	glTexImage2D(
		GL_TEXTURE_2D, 0, GL_RGBA, width, height, 
		0, GL_RGBA, GL_UNSIGNED_BYTE, NULL
	);
	assert(glGetError() == GL_NO_ERROR);

	/* allocate a Texture object as a Lua userdata, push
	   it to the stack, and initialize it */
	Texture* texture = (Texture*) lua_newuserdata(L, sizeof(Texture));
	texture->tex_id = tex_id;
	texture->width = width;
	texture->height = height;

	/* push the Texture metatable to the stack, then pop it and
	   attach it to the userdata */
	luaL_getmetatable(L, "Texture");
	lua_setmetatable(L, -2);

	/* return the userdata to the caller, via the stack */
	return 1;
}

static int texture_get_width(lua_State* L) {
	/* receive a Texture userdata as the first argument */
	Texture* texture = (Texture*) luaL_checkudata(L, 1, "Texture");

	/* push its width onto the stack and return it */
	lua_pushinteger(L, texture->width);
	return 1;
}

static int texture_get_height(lua_State* L) {
	/* receive a Texture userdata as the first argument */
	Texture* texture = (Texture*) luaL_checkudata(L, 1, "Texture");

	/* push its height onto the stack and return it */
	lua_pushinteger(L, texture->height);
	return 1;
}

void init(lua_State* L) {
	/* construct the Texture metatable */
	luaL_newmetatable(L, "Texture");

	/* push the string key "__index" to the stack */
	lua_pushstring(L, "__index");

	/* construct a table which contains the width/height accessor
	   functions, bound to the string keys "width" and "height" */
	lua_createtable(L, 0, 2);

	lua_pushstring(L, "width");
	lua_pushcfunction(L, texture_get_width);
	lua_settable(L, -3);

	lua_pushstring(L, "height");
	lua_pushcfunction(L, texture_get_height);
	lua_settable(L, -3);

	/* bind that table to the Texture metatable's "__index" key */
	lua_settable(L, -3);

	/* remove the Texture metatable from the stack */
	lua_pop(L, 1);

	/* bind the create_texture function to a global variable */
	lua_register(L, "create_texture", create_texture);
}
```

I believe this code speaks for itself! Let's move on...


## LuaBridge

[LuaBridge](https://github.com/vinniefalco/LuaBridge) is an excellent third-party library which 
hides all of the above complexity by exploiting C++'s template system.

```c++
class Texture {
  public:
	Texture(GLsizei width, GLsizei height)
	: mTexId(0), mWidth(width), mHeight(height)
	{
		//generate the OpenGL texture object itself
		glGenTextures(1, &mTexId);
		glBindTexture(GL_TEXTURE_2D, mTexId);
		glTexImage2D(
			GL_TEXTURE_2D, 0, GL_RGBA, width, height, 
			0, GL_RGBA, GL_UNSIGNED_BYTE, NULL
		);
		assert(glGetError() == GL_NO_ERROR);
	}

	GLsizei width() { return mWidth; }
	GLsizei height() { return mHeight; }

  private:
	GLuint mTexId;
	GLsizei mWidth;
	GLsizei mHeight;
};

void init(lua_State* L) {
	luabridge::getGlobalNamespace(L)
		.beginClass<Texture>("Texture")
			.addConstructor<void (*) (::GLsizei, ::GLsizei)>()
			.addFunction("width", &Texture::width)
			.addFunction("height", &Texture::height)
		.endClass();
}
```

This is a huge improvement - LuaBridge has taken the dozens of lines of confusing glue code we 
saw above, and trimmed them down to only six lines!

Notice that LuaBridge is capable of binding *existing* C++ code to Lua, without requiring 
the user to rewrite any of their native code, or write wrapper functions to translate Lua 
data into C++ data. The constructor for the `Texture` class, and its `width()` and `height()` 
methods, are the sort of code you might write even if you weren't working with Lua at all. 
Arguments and return values are automatically translated, and so the glue code "just works". 
This is about as good as it gets!

Unfortunately, LuaBridge is built on top of C++'s template system. Rust doesn't have
a template system - its trait system is much more safe and rigorous, but also much more
restrictive. In Rust, this kind of magic trick is more difficult to pull off.


## `rlua`

[`rlua`](https://github.com/amethyst/rlua) is a high-level set of Lua bindings for Rust.
In my opinion, it's the best Lua-to-Rust binding currently available.

Here is the Rust code which we'll be trying to bind from here on:

```rust
use gl::types::{GLint, GLsizei, GLuint};
use std::ptr::{null};

struct Texture {
	tex_id: GLuint,
	width: GLsizei,
	height: GLsizei
}

impl Texture {
	fn empty(width: GLsizei, height: GLsizei) -> Texture {
		let mut tex_id = 0;

		unsafe {
			//generate the OpenGL texture object itself
			gl::GenTextures(1, &mut tex_id);
			gl::BindTexture(gl::TEXTURE_2D, tex_id);
			gl::TexImage2D(
				gl::TEXTURE_2D, 0, gl::RGBA as GLint, width, height,
				0, gl::RGBA, gl::UNSIGNED_BYTE, null()
			);
			assert!(gl::GetError() == gl::NO_ERROR);
		}

		//construct the Texture
		Texture { tex_id, width, height }
	}

	fn width(&self) -> GLsizei { self.width }
	fn height(&self) -> GLsizei { self.height }
}
```

And here's the `rlua` glue code:

```rust
use rlua::{Context, prelude::LuaResult, UserData, UserDataMethods};

impl UserData for Texture {
	fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
		methods.add_method("width", |_, this, _: ()| {
			Ok(this.width())
		});

		methods.add_method("height", |_, this, _: ()| {
			Ok(this.height())
		});
	}
}

fn init(lua_ctx: Context) -> LuaResult<()> {
	let create_texture = lua_ctx.create_function(
		|_, (width, height): (GLsizei, GLsizei)| {
			Ok(Texture::empty(width, height))
		}
	)?;

	let globals = lua_ctx.globals();
	globals.set("create_texture", create_texture)?;

	Ok(())
}
```

`rlua` is a great library, but this glue code is definitely a backwards step, compared to 
LuaBridge. We're forced to write some impenetrable hieroglyphics every time we want to 
make a Rust function callable from Lua. Binding global variables isn't as straightforward
as it could be, and `UserData`'s method signature isn't exactly a thing of beauty. 
We can do better.


## `pyo3`

`pyo3` is the leading set of Python bindings for Rust. Its glue code requires us to go back and
add some attribute macros to our `Texture` type:

```rust
use pyo3::prelude::*;

#[pyclass]
struct Texture {
	tex_id: GLuint,
	width: GLsizei,
	height: GLsizei
}

#[pymethods]
impl Texture {
	#[new]
	fn empty(width: GLsizei, height: GLsizei) -> Texture {
		//...
	}

	fn width(&self) -> GLsizei { self.width }
	fn height(&self) -> GLsizei { self.height }
}
```

In some ways, this is the best approach we've seen so far. Argument and return-value conversions 
are handled automatically. There's no need to explicitly register any classes or methods; they're 
all registered automatically at startup, using the [`ctor` crate]. This means that each method
only needs to be named once, rather than twice.

[`ctor` crate]: https://docs.rs/ctor/*/ctor/index.html

However, this macro-based approach comes with several downsides. Procedural macros require the
heavyweight dependencies `proc-macro2`, `quote` and `syn`. The `ctor` crate has poor availability
on some platforms, such as game consoles and WebAssembly. The `#[pysomething]` attributes 
use a [complex domain-specific language](https://pyo3.rs/v0.12.4/class.html#customizing-the-class), 
which can make them difficult to learn and difficult to customise. Those macros can only be used
to bind Rust code at its definition site; the macros can't be used to bind third-party code to
Python. In general, I'd describe this approach as "convenient, but inflexible".

I did consider using macros for GameLisp, but I eventually decided to try a different tack.


## GameLisp

GameLisp's glue code looks like this:

```rust
use glsp::prelude::*;

fn init() -> GResult<()> {
	glsp::bind_rfn("create-texture", &Texture::empty)?;

	RClassBuilder::<Texture>::new()
		.met("width", &Texture::width)
		.met("height", &Texture::height)
		.build();

	Ok(())
}
```

By tying Rust's type system in knots, I've managed to devise a Rust API which has the same
flexibility and convenience as LuaBridge. It performs automatic conversions for function
arguments and return values; works well with Rust methods, closures and generic functions; permits 
arbitrary Rust data to be moved onto the garbage-collected heap; enables you to bind third-party 
crates so that they can be scripted by GameLisp code; and provides a very convenient way to store, 
and access, global Rust data.

Over the next four chapters, we'll go through each of these APIs in detail.
