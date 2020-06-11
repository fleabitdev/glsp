The GameLisp website has four parts: the homepage, the playground, the stdlib docs, and the 
reference. (The Rust API docs are linked from the homepage, but they're generated and hosted 
[by docs.rs](https://docs.rs/glsp/).)

This crate is a basic static site generator for the above. It's not published to crates.io -
it should be cloned manually instead. After `cargo run` in this directory, the `output` directory 
contains the full website: `output/index.html` for the homepage, `output/playground` for the
playground, `output/reference/` for the book, and `output/std/` for the stdlib docs. 

All hyperlinks are relative, so the `output` directory can be directly uploaded to a static site 
host such as GitHub Pages. However, we do have to generate two files, "sitemap.xml" and
"robots.txt", which are forced to use absolute URLs - you can either delete those files, or
configure their prefix by changing the `SITEMAP_PREFIX` constant.

By default, the web host must be configured so that the page `/dir/page.html` can be accessed via 
the URL `/dir/page`, with no file extension. (You can override this requirement by enabling the 
`"suffix-paths"` feature - this is useful for local testing.)

Shared resources, such as `style.css`, are simply copied and pasted from files in the `input` 
directory.

The manual is generated from Markdown files in the `input/manual/` directory, using the `mdbook`
crate. (This is a very large dependency, so the first `cargo run` will be quite slow.)

The stdlib documentation pages are generated from TOML files in the `input/std/` directory, using 
a small custom static site generator implemented in Rust.

The playground is partially defined by some scripts and images in the `input/playground/`
directory. The spritesheet's `.png` and `.json` files should be generated from the `.ase` file 
using [Aseprite](https://www.aseprite.org/). You'll also need to compile a WebAssembly GameLisp 
runtime from the crate in the `glsp-playground` directory.
