#![feature(proc_macro_hygiene)]
#![feature(str_strip)]

use image::{self, DynamicImage, GenericImageView, imageops, Rgba};
use maud::{DOCTYPE, html, PreEscaped};
use mdbook::{MDBook};
use pulldown_cmark::{html::push_html, Parser};
use serde::{Deserialize};
use std::{fs};
use std::collections::{HashMap};
use std::convert::TryFrom;
use std::error::Error;
use std::ffi::{OsStr, OsString};
use std::fmt::Write;
use std::mem::replace;
use std::path::{Path, PathBuf};
use std::time::{Instant, SystemTime};
use walkdir::{self, DirEntry, WalkDir};


//-------------------------------------------------------------------------------------------------
// constants
//-------------------------------------------------------------------------------------------------

const STYLESHEET: &str = "style.css";
const SEARCH_JS: &str = "search.js";
const PRELOAD_JS: &str = "preload.js";
const PLAYGROUND_JS: &str = "playground.js";
const GLSP_PLAYGROUND_JS: &str = "glsp_playground.js";
const GLSP_PLAYGROUND_BG_WASM: &str = "glsp_playground_bg.wasm";
const TCOF_SPLASH_IMAGE: &str = "tcof-splash.png";

const DEMOS: &[&str] = &[
	"minefinder.glsp",
	"quadris.glsp",
	"tennis.glsp"
];

const SITEMAP_PREFIX: &str = "https://gamelisp.rs";


//-------------------------------------------------------------------------------------------------
// Pages and rendering
//-------------------------------------------------------------------------------------------------

//a Page is the final data structure which is rendered to produce the <body> of a particular stdlib 
//page: the index, an api description, or a category listing.
struct Page {
	filename: String,
	title: String,
	parent_href: Option<String>,
	link_list: Vec<SidebarLink>,
	content: Content,

	//for rendering the nav
	prev: Option<PageName>,
	next: Option<PageName>
}

struct PageName {
	is_code: bool,
	user_facing: String,
	filename: String,
	href: String
}

struct SidebarLink {
	current: bool,
	level: usize,
	name: PageName
}

enum Content {
	Api {
		name: String,
		args: Vec<ApiArg>,
		returns: Option<ApiType>,
		kinds: Vec<Kind>,
		markdown_desc: String,
		pre_text: Option<String>
	},
	CategoryPage {
		name: String,
		markdown_blurb: String,
		subcategories: Vec<CodeTable>
	},
	Index {
		tables: Vec<IndexTable>
	},
	AlphabeticIndex {
		tables: Vec<CodeTable>
	}
}

//a table with api names on the left and the first line of each api's description on the right
struct CodeTable {
	heading: Option<String>,
	rows: Vec<CodeRow>
}

struct CodeRow {
	name: PageName,
	show_filename: bool,
	markdown_first_line: String
}

//a table with an optional category name on the left and a list of api links on the right
struct IndexTable {
	heading: String,
	href: String,
	rows: Vec<IndexRow>
}

struct IndexRow {
	name: Option<String>,
	apis: Vec<PageName>
}

struct ApiArg {
	name: String,
	to_describe: bool,
	ty: ApiType,
	reps: ArgReps
}

enum ApiType {
	Form,
	Pat,
	Place,
	Val,
	Nil,
	Int,
	Flo,
	Num,
	Char,
	Bool,
	Sym,
	Arr,
	Str,
	Deque,
	Tab,
	Iter,
	Iterable,
	Obj,
	Class,
	RFn,
	GFn,
	Callable,
	Expander,
	Coro,
	RData,
	Or2(Box<(ApiType, ApiType)>),
	Or3(Box<(ApiType, ApiType, ApiType)>)
}

impl ApiType {
	fn from_str(text: &str) -> ApiType {
		use ApiType::*;

		match text {
			"form" => Form,
			"pat" => Pat,
			"place" => Place,
			"val" => Val,
			"nil" => Nil,
			"int" => Int,
			"flo" => Flo,
			"num" => Num,
			"char" => Char,
			"bool" => Bool,
			"sym" => Sym,
			"arr" => Arr,
			"str" => Str,
			"deque" => Deque,
			"tab" => Tab,
			"iter" => Iter,
			"iterable" => Iterable,
			"obj" => Obj,
			"class" => Class,
			"rfn" => RFn,
			"fn" => GFn,
			"callable" => Callable,
			"expander" => Expander,
			"coro" => Coro,
			"rdata" => RData,
			text => {
				if text.split('|').count() == 2 {
					let mut split = text.split('|');
					let first = split.next().unwrap();
					let second = split.next().unwrap();
					Or2(Box::new((ApiType::from_str(first), ApiType::from_str(second))))
				} else if text.split('|').count() == 3 {
					let mut split = text.split('|');
					let first = split.next().unwrap();
					let second = split.next().unwrap();
					let third = split.next().unwrap();
					Or3(Box::new((
						ApiType::from_str(first),
						ApiType::from_str(second),
						ApiType::from_str(third)
					)))
				} else {
					panic!("unrecognized type {:?}", text)
				}
			}
		}
	}
}

enum ArgReps {
	One,
	Optional(Option<String>),
	ZeroOrMore,
	OneOrMore,
	TwoOrMore
}

enum Boilerplate {
	Std,
	Home,
	Playground,
	Tcof
}

fn render_boilerplate(boilerplate: Boilerplate, title: &str, body: &str) -> String {
	(html! {
		(DOCTYPE)
		html lang="en" {
			head {
				meta charset="utf-8";
				title { (PreEscaped(title)) }

				@let prefix = match boilerplate {
					Boilerplate::Home => "",
					_ => "../"
				};

				link rel="stylesheet" href={(prefix) (STYLESHEET)} type="text/css";
				link rel="icon" href={(prefix) "favicon-16.png"} sizes="16x16" type="image/x-icon";
				link rel="icon" href={(prefix) "favicon-24.png"} sizes="24x24" type="image/x-icon";
				link rel="icon" href={(prefix) "favicon-32.png"} sizes="32x32" type="image/x-icon";
				link rel="icon" href={(prefix) "favicon-48.png"} sizes="48x48" type="image/x-icon";

				meta name="color-scheme" content="light dark";
				meta name="viewport" content="width=device-width, initial-scale=1";

				@match boilerplate {
					Boilerplate::Std => {
						script defer? src=(SEARCH_JS) type="text/javascript" { }
						script defer? src=(PRELOAD_JS) type="text/javascript" { }
					}
					Boilerplate::Playground => {
						script defer? src=(GLSP_PLAYGROUND_JS) type="text/javascript" { }
						script defer? src=(PLAYGROUND_JS) type="text/javascript" { }
					}
					_ => {}
				}
			}
			(PreEscaped(body))
		}
	}).into_string()
}

fn render_page_body(page: &Page) -> String {
	(html! {
		body#api-page {
			input#checkbox-hack type="checkbox";
			(PreEscaped(render_page_sidebar(page)))
			(PreEscaped(render_page_content(page)))
			(PreEscaped(render_page_nav(page)))
		}
	}).into_string()
}

fn render_page_sidebar(page: &Page) -> String {
	(html! {
		nav.layout-sidebar.horz-center {
			@let logo_href = if cfg!(feature = "suffix-paths") { "../index.html" } else { "../" };
			a.logo-anchor href=(logo_href) {
				.logo { 
					picture {
						source srcset="../logo-dark.png" media="(prefers-color-scheme: dark)";
						img src="../logo-light.png" alt="GameLisp"; 
					}
				}
			}
			.panel.search-field {
				input#api-search-field type="search" placeholder="Search...";
			}
			.panel#api-list {
				@let index_href = PageName::index().href;
				@let header_class = if page.filename == "index" {
					"panel-header current"
				} else {
					"panel-header"
				};
				a#api-list-header href=(index_href) class=(header_class) { 
					"Standard Library"
				}
				.link-list#api-link-list {
					@for link in &page.link_list {
						a class=(if link.current { "current" } else { "" }) href=(link.name.href) {
							@if link.level == 0 {
								@if link.name.is_code {
									.list-level0 { code { (&link.name.user_facing) } }
								} @else {
									.list-level0 { span { (&link.name.user_facing) } }
								}
							} @else {
								@if link.name.is_code {
									.list-level1 { code { (&link.name.user_facing) } }
								} @else {
									.list-level1 { span { (&link.name.user_facing) } }
								}
							}
						}
					}
					hr { }
					@let alph_href = PageName::alphabetic_index().href;
					@if page.filename == "alphabetic-index" {
						a class="current" href=(alph_href) { .list-level0 { "Alphabetic Index" }}
					} @else {
						a href=(alph_href) { .list-level0 { "Alphabetic Index" }}
					}
				}
			}
		}
	}).into_string()
}

fn render_page_content(page: &Page) -> String {
	match &page.content {
		Content::Api { name, args, returns, kinds, markdown_desc, pre_text } => (html! {
			main.layout-content.panel {
				.pre-scroller { .pre-padding {
					pre#api-spec {
						@if let Some(pre_text) = pre_text {
							(PreEscaped(render_pre_text(pre_text, name, &page.filename)))
						} @else {
							@if kinds.contains(&Kind::Var) {
								span.emphasised { (name) }
								"\n  " span.subtle { "a frozen global variable" }
								@if let Some(ref returns) = *returns {
									"\n  " span.subtle { "stores " (PreEscaped(returns.a_name())) }
								}
							} @else {
								"(" span.emphasised { (name) }

								@for arg in args {
									" " (PreEscaped(render_arg_brief(arg)))
								}

								")"

								@for arg in args {
									@if arg.to_describe {
										"\n  " span.subtle { (PreEscaped(render_arg_verbose(arg))) }
									}
								}

								@if let Some(ref returns) = *returns {
									"\n  " span.subtle { "returns " (PreEscaped(returns.a_name())) }
								}
							}
						}
					}

					span { }

					@for kind in kinds.iter() {
						(PreEscaped(render_kind(kind)))
					}
				}}
				.text-box#api-desc {
					(PreEscaped(render_markdown(markdown_desc)))
				}
			}
		}).into_string(),

		Content::CategoryPage { name, markdown_blurb, subcategories } => (html! {
			main.layout-content.panel {
				.panel-header { (name) }
				.text-box {
					(PreEscaped(render_markdown(markdown_blurb)))

					@for code_table in subcategories {
						(PreEscaped(render_code_table(code_table)))
					}
				}
			}
		}).into_string(),

		Content::Index { tables } => (html! {
			main.layout-content.panel {
				.panel-header { "Standard Library" }
				.text-box {
					@for table in tables {
						(PreEscaped(render_index_table(table)))
					}
				}
			}
		}).into_string(),

		Content::AlphabeticIndex { tables } => (html! {
			main.layout-content.panel {
				.panel-header { "Alphabetic Index" }
				.text-box {
					@for table in tables {
						(PreEscaped(render_code_table(table)))
					}
				}
			}
		}).into_string()
	}
}

fn render_pre_text(mut spec: &str, name: &str, mut filename: &str) -> String {
	if filename.ends_with("-abbrv") {
		filename = &filename[..filename.len() - 6];
	}

	//leave () and [] unaltered. if a substring matches the name or filename, emphasise it.
	//mark all other text as subtle.
	let mut builder = String::new();
	let mut subtle_accum = String::new();

	loop {
		if spec.starts_with("(") || spec.starts_with(")") || 
		   spec.starts_with("[") || spec.starts_with("]") ||
		   spec.starts_with(name) || spec.starts_with(filename) ||
		   spec.len() == 0 {

		   	if subtle_accum.len() > 0 {
		   		builder.push_str(&(html! {
					span.subtle { (subtle_accum) }
				}).into_string());

		   		subtle_accum.clear();
		   	}

		   	if spec.len() == 0 {
		   		break
		   	}

		   	if spec.starts_with(name) {
		   		builder.push_str(&(html! {
					span.emphasised { (name) }
				}).into_string());

				spec = &spec[name.len()..];
		   	} else if spec.starts_with(filename) {
		   		builder.push_str(&(html! {
					span.emphasised { (filename) }
				}).into_string());

				spec = &spec[filename.len()..];
		   	} else {
		   		builder.push_str(&spec[..1]);
				spec = &spec[1..];
		   	}

		   	continue
		}

		let ch = spec.chars().next().unwrap();
		subtle_accum.push(ch);
		spec = &spec[ch.len_utf8()..];
	}

	assert!(subtle_accum.is_empty());
	builder
}

fn render_code_table(table: &CodeTable) -> String {
	(html! {
		@if let Some(ref heading) = table.heading {
			@let heading_id = heading_to_id(heading);
			h2 id=(heading_id) { 
				a href={"#" (heading_id)} { (heading) }
			}
		}

		.code-table {
			@for (row_i, row) in table.rows.iter().enumerate() {
				a.row-link 
					href=(row.name.href)
					style={"grid-row: " ((row_i + 1))} {}
				.left-column style={"grid-row: " ((row_i + 1))} {
					@if row.show_filename {
						code { (row.name.filename) }
					} @else {
						code { (row.name.user_facing) }
					}
				}
				.right-column style={"grid-row: " ((row_i + 1))} {
					p {
						(PreEscaped(render_markdown(&row.markdown_first_line)))
					}
				}
			}
		}
	}).into_string()
}

fn render_index_table(table: &IndexTable) -> String {
	(html! {
		h2 id=(heading_to_id(&table.heading)) { 
			a href=(table.href) { (table.heading) }
		}

		.index-table {
			@for (row_i, row) in table.rows.iter().enumerate() {
				@if let Some(ref name) = row.name {
					.left-column style={"grid-row: " ((row_i + 1))} {
						@let subcat_id = heading_to_id(name);
						a href={(table.href) "#" (subcat_id)} {
							(name)
						}
					}
				}
				.right-column style={"grid-row: " ((row_i + 1))} {
					@for api_name in &row.apis {
						a href=(api_name.href) { code { (api_name.user_facing) }}
					}
				}
			}
		}
	}).into_string()
}

fn heading_to_id(heading: &str) -> String {
	let mut builder = String::new();
	for word in heading.split_whitespace() {
		if builder.len() > 0 {
			builder.push('-');
		}
		builder.push_str(&word.to_lowercase());
	}
	builder
}

fn render_kind(kind: &Kind) -> String {
	let (kind_class, kind_title, kind_text) = match *kind {
		Kind::Fn => ("fn-kind", "Function", "F"),
		Kind::Mac => ("mac-kind", "Macro", "M"),
		Kind::Var => ("var-kind", "Global Variable", "V"),
		Kind::Special => ("special-kind", "Special Form", "S"),
		Kind::Abbrv => ("abbrv-kind", "Abbreviation", "A"),
		Kind::Clause => ("clause-kind", "Class Clause", "C"),
	};

	(html! {
		.api-kind.(kind_class) title=(kind_title) { .api-kind-text { (kind_text) }  }
	}).into_string()
}

fn render_arg_brief(arg: &ApiArg) -> String {
	match &arg.reps {
		ArgReps::One => {
			(html! {
				span.subtle { (arg.name) }
			}).into_string()
		}
		ArgReps::Optional(None) => {
			(html! {
				"(" span.subtle { "? " (arg.name) } ")"
			}).into_string()
		}
		ArgReps::Optional(Some(default)) => {
			(html! {
				"(" span.subtle { "? " (arg.name) " " (default) } ")"
			}).into_string()
		}
		ArgReps::ZeroOrMore | ArgReps::OneOrMore | ArgReps::TwoOrMore => {
			(html! {
				span.subtle { ".." (arg.name) }
			}).into_string()
		}
	}
}

fn render_arg_verbose(arg: &ApiArg) -> String {
	match arg.reps {
		ArgReps::One => {
			format!("{}: {}", &arg.name, arg.ty.a_name())
		}
		ArgReps::Optional(_) => {
			format!("{}: {} (optional)", &arg.name, arg.ty.a_name())
		}
		ArgReps::ZeroOrMore => {
			format!("{}: zero or more {}", &arg.name, arg.ty.names())
		}
		ArgReps::OneOrMore => {
			format!("{}: one or more {}", &arg.name, arg.ty.names())
		}
		ArgReps::TwoOrMore => {
			format!("{}: two or more {}", &arg.name, arg.ty.names())
		}
	}
}

impl ApiType {
	fn href(&self) -> Option<&'static str> {
		use ApiType::*;

		match *self {
			Form => None,
			Pat => Some("patterns.html"),
			Place => Some("built-in-macros.html#assignment"),
			Val => None,
			Nil => Some("syntax-and-types.html#nil"),
			Int => Some("numbers.html"),
			Flo => Some("numbers.html"),
			Num => Some("numbers.html"),
			Char => Some("syntax-and-types.html#char"),
			Bool => Some("syntax-and-types.html#bool"),
			Sym => Some("syntax-and-types.html#sym"),
			Arr => Some("arrays.html"),
			Str => Some("strings-and-text.html"),
			Deque => Some("syntax-and-types.html#abstract-types"),
			Tab => Some("tables.html"),
			Iter => Some("iterators.html"),
			Iterable => Some("syntax-and-types.html#abstract-types"),
			Obj => Some("oop.html"),
			Class => Some("oop.html"),
			RFn => Some("rust-functions.html"),
			GFn => Some("evaluation.html#functions"),
			Callable => Some("syntax-and-types.html#abstract-types"),
			Expander => Some("syntax-and-types.html#abstract-types"),
			Coro => Some("coroutines.html"),
			RData => Some("rust-data.html"),
			Or2(_) => None,
			Or3(_) => None
		}
	}

	fn a_name(&self) -> String {
		use ApiType::*;

		let (prefix, linked_text) = match *self {
			Form => ("", "any form"),
			Pat => ("a ", "pattern"),
			Place => ("a ", "place"),
			Val => ("", "any type"),
			Nil => ("", "#n"),
			Int => ("an ", "int"),
			Flo => ("a ", "flo"),
			Num => ("a ", "num"),
			Char => ("a ", "char"),
			Bool => ("a ", "bool"),
			Sym => ("a ", "sym"),
			Arr => ("an ", "arr"),
			Str => ("a ", "str"),
			Deque => ("a ", "deque"),
			Tab => ("a ", "tab"),
			Iter => ("an ", "iter"),
			Iterable => ("an ", "iterable"),
			Obj => ("an ", "obj"),
			Class => ("a ", "class"),
			RFn => ("an ", "rfn"),
			GFn => ("a ", "fn"),
			Callable => ("a ", "callable"),
			Expander => ("an ", "expander"),
			Coro => ("a ", "coro"),
			RData => ("an ", "rdata"),
			Or2(ref tuple) => {
				return format!(
					"{} or {}",
					tuple.0.a_name(),
					tuple.1.a_name()
				)
			}
			Or3(ref tuple) => {
				return format!(
					"{}, {}, or {}",
					tuple.0.a_name(),
					tuple.1.a_name(),
					tuple.2.a_name()
				)
			}
		};

		(html! {
			@if let Some(href) = self.href() {
				(prefix) a href={"../reference/" (href)} { (linked_text) }
			} @else {
				(prefix) (linked_text)
			}
		}).into_string()
	}

	fn names(&self) -> String {
		use ApiType::*;

		let text = match *self {
			Form => "forms",
			Pat => "patterns",
			Place => "places",
			Val => "arguments (any type)",
			Nil => "#n",
			Int => "ints",
			Flo => "flos",
			Num => "nums",
			Char => "chars",
			Bool => "bools",
			Sym => "syms",
			Arr => "arrs",
			Str => "strs",
			Deque => "deques",
			Tab => "tabs",
			Iter => "iters",
			Iterable => "iterables",
			Obj => "objs",
			Class => "classes",
			RFn => "rfns",
			GFn => "fns",
			Callable => "callables",
			Expander => "expanders",
			Coro => "coros",
			RData => "rdatas",
			Or2(ref tuple) => {
				return format!(
					"{} or {}",
					tuple.0.names(),
					tuple.1.names()
				)
			}
			Or3(ref tuple) => {
				return format!(
					"{}, {}, or {}",
					tuple.0.names(),
					tuple.1.names(),
					tuple.2.names()
				)
			}
		};

		(html! {
			@if let Some(href) = self.href() {
				a href={"../reference/" (href)} { (text) }
			} @else {
				(text)
			}
		}).into_string()
	}
}

fn render_page_nav(page: &Page) -> String {
	let nav_class = if page.prev.is_some() || page.next.is_some() || page.parent_href.is_some() {
		"layout-nav prev-next-nav"
	} else {
		"layout-nav prev-next-nav no-links"
	};

	(html! {
		#api-nav class=(nav_class) {
			@if let Some(prev) = &page.prev {
				.prev-nav {
					a href=(prev.href) {
						.button.left-button#api-nav-prev {
							span#api-nav-prev-arrow { "←" }
							@if prev.is_code {
								code.ellipsis-text#api-nav-prev-text { (prev.user_facing) }
							} @else {
								span.ellipsis-text#api-nav-prev-text { (prev.user_facing) }
							}
						}
					}
				}
			}
			@if let Some(ref parent_href) = page.parent_href {
				a#api-nav-parent-link href=(parent_href) {
					.button#api-nav-parent { span#api-nav-parent-arrow { "⭡" }}
				}
			}
			label.button#api-nav-hamburger for="checkbox-hack" {
				span#api-nav-hamburger-icon { "☰" }
				span#api-nav-cancel-icon { "X" }
			}
			@if let Some(next) = &page.next {
				.next-nav {
					a href=(next.href) {
						.button.right-button#api-nav-next {
							@if next.is_code {
								code.ellipsis-text#api-nav-next-text { (next.user_facing) }
							} @else {
								span.ellipsis-text#api-nav-next-text { (next.user_facing) }
							}
							span#api-nav-next-arrow { "→" }
						}
					}
				}
			}
		}
	}).into_string()
}

fn render_markdown(text: &str) -> String {
	//if the input is all whitespace, break early
	if text.chars().all(char::is_whitespace) {
		return text.to_string()
	}

	//use pulldown_cmark to convert the markdown to html. 
	let mut html = String::new();
	push_html(&mut html, Parser::new(text));

	//an awkward post-processing hack to syntax-highlight code blocks. we also take the 
	//opportunity to wrap <pre> tags in some extra divs to make them scrollable with padding.
	let mut highlighted = String::new();
	let mut remaining = &html[..];
	while remaining.len() > 0 {
		let i = remaining.find("<pre").unwrap_or(remaining.len());
		highlighted.push_str(&remaining[..i]);
		remaining = &remaining[i..];

		if remaining.starts_with("<pre") {
			highlighted.push_str("<div class=\"pre-scroller\"><div class=\"pre-padding\">");

			let j = remaining.find("</pre>").unwrap();
			let code = &remaining[..j];
			remaining = &remaining[j + 6..];

			for (line_i, line) in code.lines().enumerate() {
				if line_i > 0 {
					highlighted.push('\n');
				}

				//the leading space is to avoid capturing, say, &lt;
				if line.starts_with(";") || line.contains(" ;") || line.contains(">;") {
					let k = if line.starts_with(";") {
						0
					} else {
						line.find(" ;").unwrap_or_else(|| line.find(">;").unwrap()) + 1
					};

					highlighted.push_str(&line[..k]);
					highlighted.push_str("<span class=\"subtle\">");
					highlighted.push_str(&line[k..]);
					highlighted.push_str("</span>");
				} else {
					highlighted.push_str(line);
				}
			}

			highlighted.push_str("</pre></div></div>");
		}
	}
	html = highlighted;

	//another post-processing hack: search for href="foo", with no URL/prefix/suffix/etc, and 
	//replace it with href="foo.html" for easier local debugging
	if cfg!(feature = "suffix-paths") {
		let mut src = &html[..];
		let mut replaced = String::new();

		while let Some(i) = src.find("href=\"") {
			let j = i + 6 + (&src[i + 6..]).find('"').unwrap();

			let href = &src[i + 6 .. j];

			//a rough heuristic...
			if (!href.contains('.')) && (!href.contains('/')) && (!href.contains(':')) {
				replaced.push_str(&src[..i + 6]);
				replaced.push_str(&href);
				replaced.push_str(".html");
				replaced.push('"');
			} else {
				replaced.push_str(&src[..j + 1]);
			}
			
			src = &src[j + 1..];
		}

		replaced.push_str(src);
		html = replaced;
	}

	html
}

fn markdown_get_first_line(text: &str) -> String {
	text.lines().next().unwrap_or("").to_string()
}


//-------------------------------------------------------------------------------------------------
// rendering non-stdlib pages
//-------------------------------------------------------------------------------------------------

fn render_homepage() -> String {
	let home_md = render_markdown(&fs::read_to_string("input/home.md").unwrap());

	let body = (html! {
		body#home-page {
			input#checkbox-hack type="checkbox";

			.layout-title {
				@let logo_href = if cfg!(feature = "suffix-paths") { "index.html" } else { "." };
				a.logo-anchor href=(logo_href) {
					.logo { 
						picture {
							source srcset="logo-dark.png" media="(prefers-color-scheme: dark)";
							img src="logo-light.png" alt="GameLisp"; 
						}
					}
				}

				label#home-hamburger for="checkbox-hack" {
					picture#home-hamburger-menu {
						source srcset="hamburger-menu-dark.png"
							media="(prefers-color-scheme: dark)";
						img src="hamburger-menu-light.png" alt = "Navigation Menu";
					}
					picture#home-hamburger-cancel {
						source srcset="hamburger-cancel-dark.png"
							media="(prefers-color-scheme: dark)";
						img src="hamburger-cancel-light.png" alt = "Navigation Menu";
					}
				}
			}

			nav.layout-sidebar.horz-center {
				.panel {
					.link-list {
						@if cfg!(feature = "suffix-paths") {
							a class="current" href="index.html" {
								.list-level0 { "Homepage" }
							}
							a href="./playground/index.html" {
								.list-level0 { "Playground" }
							}
							a href="./reference/index.html" {
								.list-level0 { "Reference Manual" }
							}
							a href="./std/index.html" {
								.list-level0 { "Standard Library" }
							}
						} @else {
							a class="current" href="." {
								.list-level0 { "Homepage" }
							}
							a href="./playground/" {
								.list-level0 { "Playground" }
							}
							a href="./reference/" {
								.list-level0 { "Reference Manual" }
							}
							a href="./std/" {
								.list-level0 { "Standard Library" }
							}
						}
						a href="https://docs.rs/glsp/" {
							.list-level0 { 
								"Rust API" 
								span.link-destination { "(docs.rs)" }
							}
						}
						a href="https://crates.io/crates/glsp/" {
							.list-level0 { 
								"Download" 
								span.link-destination { "(crates.io)" }
							}
						}
						a href="https://github.com/fleabitdev/glsp/" {
							.list-level0 { 
								"Develop" 
								span.link-destination { "(github.com)" }
							}
						}
					}
				}

				a href="https://patreon.com/fleabitdev" {
					.panel.patreon-button {
						picture {
							source srcset="patreon-button-dark.png" 
								media="(prefers-color-scheme: dark)";
							img src="patreon-button-light.png" alt="Become a Patron";
						}
					}
				}
			}

			main.layout-content {
				.panel {
					.text-box {
						(PreEscaped(home_md))
					}
				}
			}
		}
	}).into_string();

	render_boilerplate(Boilerplate::Home, "GameLisp", &body)
}


fn render_playground() -> String {
	let body = (html! {
		body#playground-page {
			@let logo_href = if cfg!(feature = "suffix-paths") { "../index.html" } else { ".." };
			a.logo-anchor href=(logo_href) {
				.logo { 
					picture {
						source srcset="../logo-narrow-dark.png"
						       media="(prefers-color-scheme: dark, max-width: 590px)";
						source srcset="../logo-narrow-light.png" media="(max-width: 590px)";
						source srcset="../logo-dark.png" media="(prefers-color-scheme: dark)";
						img src="../logo-light.png" alt="GameLisp"; 
					}
				}
			}
			#playground-button {
				picture#playground-play {
					source srcset="../playground-play-dark.png" 
					       media="(prefers-color-scheme: dark)";
					img src="../playground-play-light.png" alt="Play"; 
				}
				picture#playground-stop {
					source srcset="../playground-stop-dark.png" 
					       media="(prefers-color-scheme: dark)";
					img src="../playground-stop-light.png" alt="Stop"; 
				}
			}
			#playground-dropdown {
				select name="glsp-source" id="glsp-source-select" { 
					@for demo in DEMOS {
						option value=((&demo[..demo.len()-5]).to_string()) { (demo.to_string()) }
					}
				}
			}
			.layout-content {
				textarea name="glsp-code" id="glsp-code-textarea" 
				         autocapitalize="none"
				         autocomplete="off"
				         spellcheck="false"
				         autocorrect="off" {
		         	@let filename = format!("input/playground/{}", DEMOS[0]);
					(fs::read_to_string(&filename).unwrap())
				}
				#playground-screen {
					#playground-loading { "Downloading data..." }
					#playground-error-msg {
						#playground-error-title { "Error" }
						pre#playground-error-text { }
					}
					#playground-intro {
					}
					canvas style="display: none;" { }
				}
			}
		}
	}).into_string();

	render_boilerplate(Boilerplate::Playground, "GameLisp Playground", &body)
}

fn render_tcof_splash() -> String {
	let body = (html! {
		body#tcof-page {
			#tcof-splash {
				#tcof-box {
					#tcof-text {
						p {
							em { "The Castle on Fire" }
							" is a retro 2D"
							(PreEscaped("&nbsp;"))
							"sidescroller, currently under development using "
							a href="https://gamelisp.rs/" { "GameLisp" }
							" and "
							a href="https://rust-lang.org/" { "Rust" }
							"."
						}

						p {
							"I'm aiming to release a demo before the end of 2020. If you'd \
							like to be notified when that happens, you can follow my "
							a href="https://twitter.com/fleabitdev" { "Twitter" }
							" or support me on "
							a href="https://patreon.com/fleabitdev" { "Patreon" }
							"."
						}
					}
				}
			}
		}
	}).into_string();

	render_boilerplate(Boilerplate::Tcof, "The Castle on Fire", &body)
}


//-------------------------------------------------------------------------------------------------
// RawCategory
//-------------------------------------------------------------------------------------------------

//RawCategory is the raw data which is deserialized from one of the .toml files

#[derive(Deserialize)]
struct RawCategory {
	filename: String, //filename, e.g. "numbers"
	name: String,     //user-facing name, e.g. "Numbers"
	text: String,     //markdown text for the introduction blurb

	vars: Option<HashMap<String, String>>,

	apis: Vec<RawApi>
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
struct RawApi {
	filename: String,

	/*
	in some cases, the filename and url will differ from the user-facing name: e.g. the 
	filename "add" represents the user-facing name "+". relevant characters forbidden either 
	by the URI spec or by Windows are +, @, ?, *, /, <, >, & and %. in these cases, the `name` 
	field is the user-facing name.
	
	when the filename ends with '-p' and `name` is absent, it defaults to a '?' suffix.

	for consistency, we rename a number of apis which we don't necessarily need to, like "-".
	
	for apis with the kind "abbrv", this field is indexed as an alternate name: e.g. both
	"unquote" and "~" are present in the search index and alphabetic index.
	*/

	name: Option<String>, 

	starts_subcategory: Option<String>,

	/*
	`pre_text` is an override for the contents of an api's <pre> block. for example, this is useful
	for abbreviations: "~form\n(unquote form)". when `spec` is provided, `args` and `returns` are
	both ignored. text which matches the api's name and filename is emphasised; all other text, 
	except for "()[]", is de-emphasised.
	*/

	pre_text: Option<String>,

	kinds: Vec<Kind>,
	args: Vec<String>,
	returns: Option<String>,
	see_also: Option<Vec<String>>,
	text: String
}

#[derive(Copy, Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
enum Kind {
	Fn,
	Mac,
	Var,
	Special,
	Abbrv,
	Clause
}

//perform variable substitution and markdown-unindentation for a toml file. this is a simple 
//pre-processing step, so we lump it in with the toml parsing. the only complication is that the 
//var values need to be unindented, and any leading and trailing whitespace needs to be trimmed, 
//so that they don't create spurious markdown formatting when copied in.
impl RawCategory {
	fn preprocess(mut self) -> RawCategory {
		let raw_vars = self.vars.take().unwrap_or_else(|| HashMap::new());
		let mut vars = HashMap::<String, String>::new();

		for (raw_key, raw_value) in &raw_vars {
			//the syntax here is a little stupid. basically just replaces `name` with `{{name}}`.
			let key = format!("{{{{{}}}}}", raw_key);
			let value = unindent_markdown(raw_value);

			vars.insert(key, value);
		}

		let preprocess = |mut raw_text: &str| -> String {
			let mut builder = String::new();

			while raw_text.len() > 0 {
				match raw_text.find("{{") {
					Some(i) => {
						builder.push_str(&raw_text[..i]);
						raw_text = &raw_text[i..];

						let j = raw_text.find("}}").map(|j| j + 2).unwrap_or(raw_text.len());
						let key = &raw_text[..j];
						raw_text = &raw_text[j..];

						match vars.get(key) {
							Some(value) => builder.push_str(value),
							None => builder.push_str(key)
						}
					}
					None => {
						builder.push_str(raw_text);
						raw_text = &raw_text[raw_text.len()..];
					}
				}
			}

			unindent_markdown(&builder)
		};


		self.text = preprocess(&self.text);
		for api in &mut self.apis {
			api.text = preprocess(&api.text);
		}

		//one final preprocessing step: replacing -p suffixes in filenames with ?, and
		//-mut suffixes with !
		for api in &mut self.apis {
			if api.name.is_none() {
				if api.filename.ends_with("-p") {
					api.name = Some(format!("{}?", &api.filename[..api.filename.len() - 2]));
				} else if api.filename.ends_with("-mut") {
					api.name = Some(format!("{}!", &api.filename[..api.filename.len() - 4]));
				}
			}
		}

		self
	}
}

fn unindent_markdown(text: &str) -> String {
	//detect the first nonempty line, and remove its leading whitespace from itself and from 
	//all subsequent lines. note that this algorithm prevents a markdown block from starting
	//with a <pre>, but that wouldn't make much sense anyway.
	let mut indent: Option<&str> = None;
	let mut accum = String::new();
	for line in text.lines() {
		if line.trim().len() > 0 {
			match indent {
				Some(indent) => {
					if line.starts_with(indent) {
						accum.push_str(&line[indent.len()..]);
					} else {
						accum.push_str(&line);
					}
				}
				ref mut indent => {
					let i = line.find(|ch| !char::is_whitespace(ch)).unwrap();
					*indent = Some(&line[..i]);
					accum.push_str(&line[i..]);
				}
			}
		}
		accum.push('\n');
	}

	//we also trim leading and trailing whitespace
	accum.trim().to_string()
}


//-------------------------------------------------------------------------------------------------
// RawCategory -> Page and RawApi -> Page conversions
//-------------------------------------------------------------------------------------------------

//these are mostly straight one-to-one conversions. the main complication is generating the
//hierarchy information (for rendering the sidebar and nav) while we traverse the raw input.

fn convert(categories: &[RawCategory]) -> Vec<Page> {
	let mut pages = Vec::new();

	pages.push(convert_index(categories));
	pages.push(convert_alphabetic_index(categories));

	for cat_i in 0 .. categories.len() {
		pages.push(convert_category_page(categories, cat_i));

		for api_i in 0 .. categories[cat_i].apis.len() {
			pages.push(convert_api_page(categories, cat_i, api_i));
		}
	}

	pages
}

fn convert_index(categories: &[RawCategory]) -> Page {
	//generate the sidebar links
	let mut link_list = Vec::<SidebarLink>::new();

	for category in categories {
		link_list.push(SidebarLink {
			current: false,
			level: 0,
			name: PageName::category(category)
		});
	}

	//constructing the IndexTables is straightforward
	let mut tables = Vec::<IndexTable>::new();

	for category in categories {
		let heading = category.name.clone();
		let href = if cfg!(feature = "suffix-paths") {
			format!("{}.html", &category.filename)
		} else {
			category.filename.clone()
		};

		let rows = if category.apis.iter().any(|api| api.starts_subcategory.is_some()) {
			let mut rows = Vec::<IndexRow>::new();

			let mut subcategory_name = String::new();
			let mut apis_accum = Vec::<PageName>::new();

			for api in &category.apis {
				if let Some(ref starts_subcategory) = api.starts_subcategory {
					if apis_accum.len() > 0 {
						rows.push(IndexRow {
							name: Some(subcategory_name),
							apis: replace(&mut apis_accum, Vec::new())
						});
					}

					subcategory_name = starts_subcategory.to_string();
				}

				apis_accum.push(PageName::api(api));
			}

			if apis_accum.len() > 0 {
				rows.push(IndexRow {
					name: Some(subcategory_name),
					apis: apis_accum
				});
			}

			rows
		} else {
			vec![IndexRow {
				name: None,
				apis: category.apis.iter().map(PageName::api).collect()
			}]
		};

		tables.push(IndexTable {
			heading,
			href,
			rows
		});
	}

	//finished
	Page {
		filename: "index".to_string(),
		title: "GameLisp Standard Library".to_string(),
		parent_href: None,
		link_list,
		content: Content::Index {
			tables
		},

		prev: None,
		next: None
	}
}

fn convert_alphabetic_index(categories: &[RawCategory]) -> Page {
	//generate the sidebar links
	let mut link_list = Vec::<SidebarLink>::new();

	for category in categories {
		link_list.push(SidebarLink {
			current: false,
			level: 0,
			name: PageName::category(category)
		});
	}

	//collect a vec of all RawApis, including duplicates for abbreviations. partition each api
	//into a different bucket depending on its starting character, preserving the relative
	//ordering of non-alphabetic apis, or lexicographically sorting alphabetic ones. emit one 
	//code-table for each bucket.
	let mut buckets = vec![Vec::<&RawApi>::new(); 27];

	for category in categories {
		for api in &category.apis {
			let mut names = vec![api.name.clone().unwrap_or_else(|| api.filename.clone())];
			if api.kinds.contains(&Kind::Abbrv) {
				assert!(api.name.is_some());
				names.push(api.filename.clone());
			}

			for name in &names {
				let ch = name.chars().next().unwrap().to_ascii_uppercase();

				let bucket_i = if ch >= 'A' && ch <= 'Z' {
					1 + ch as usize - 'A' as usize
				} else {
					0
				};

				buckets[bucket_i].push(api);
			}
		}
	}

	let mut tables = Vec::<CodeTable>::with_capacity(buckets.len());

	for (i, bucket) in buckets.iter().enumerate() {
		let heading = if i == 0 { 
			"#".to_string()
		} else { 
			char::try_from('A' as u32 + (i - 1) as u32).unwrap().to_string()
		};

		let mut rows = Vec::new();
		for api in bucket {
			rows.push(CodeRow {
				name: PageName::api(api),
				show_filename: api.kinds.contains(&Kind::Abbrv) && i > 0,
				markdown_first_line: markdown_get_first_line(&api.text)
			});
		}

		if i > 0 {
			rows.sort_by_key(|row| {
				if row.show_filename {
					row.name.filename.clone()
				} else {
					row.name.user_facing.clone()
				}
			});
		}

		if rows.len() > 0 {
			tables.push(CodeTable {
				heading: Some(heading),
				rows
			});
		}
	}

	//finished
	Page {
		filename: "alphabetic-index".to_string(),
		title: "Alphabetic Index - GameLisp Standard Library".to_string(),
		parent_href: None,
		link_list,
		content: Content::AlphabeticIndex {
			tables
		},

		prev: None,
		next: None
	}
}

fn convert_category_page(categories: &[RawCategory], cat_i: usize) -> Page {
	let category = &categories[cat_i];

	//generate the sidebar links
	let mut link_list = Vec::<SidebarLink>::new();
	for i in 0 .. cat_i {
		link_list.push(SidebarLink {
			current: false,
			level: 0,
			name: PageName::category(&categories[i])
		});
	}

	link_list.push(SidebarLink {
		current: true,
		level: 0,
		name: PageName::category(category)
	});

	for api in &category.apis {
		link_list.push(SidebarLink {
			current: false,
			level: 1,
			name: PageName::api(api)
		});
	}

	for i in (cat_i + 1) .. categories.len() {
		link_list.push(SidebarLink {
			current: false,
			level: 0,
			name: PageName::category(&categories[i])
		});
	}

	//the prev and next pages
	let prev = if cat_i == 0 {
		None
	} else {
		Some(PageName::category(&categories[cat_i - 1]))
	};

	let next = if cat_i == categories.len() - 1 {
		None
	} else {
		Some(PageName::category(&categories[cat_i + 1]))
	};

	//the api tables
	let mut subcategories = Vec::new();

	let mut api_i = 0;
	while api_i < category.apis.len() {
		let subcat_name = category.apis[api_i].starts_subcategory.clone();
		let mut rows = Vec::new();

		while api_i < category.apis.len() {
			let api = &category.apis[api_i];
			rows.push(CodeRow {
				name: PageName::api(api),
				show_filename: false,
				markdown_first_line: markdown_get_first_line(&api.text)
			});

			api_i += 1;
			if api_i < category.apis.len() && category.apis[api_i].starts_subcategory.is_some() {
				break
			}
		}

		subcategories.push(CodeTable {
			heading: subcat_name,
			rows
		});
	}

	Page {
		filename: category.filename.clone(),
		title: format!("{} - GameLisp Standard Library", &category.name),
		parent_href: Some(PageName::index().href),
		link_list,
		content: Content::CategoryPage {
			name: category.name.clone(),
			markdown_blurb: category.text.clone(),
			subcategories
		},

		prev,
		next
	}
}

fn convert_api_page(categories: &[RawCategory], cat_i: usize, api_i: usize) -> Page {
	let category = &categories[cat_i];
	let api = &category.apis[api_i];

	let page_name = PageName::api(api);

	//generate the sidebar links
	let mut link_list = Vec::<SidebarLink>::new();
	for i in 0 .. cat_i {
		link_list.push(SidebarLink {
			current: false,
			level: 0,
			name: PageName::category(&categories[i])
		});
	}

	link_list.push(SidebarLink {
		current: true,
		level: 0,
		name: PageName::category(category)
	});

	for (i, api) in category.apis.iter().enumerate() {
		link_list.push(SidebarLink {
			current: i == api_i,
			level: 1,
			name: PageName::api(api)
		});
	}

	for i in (cat_i + 1) .. categories.len() {
		link_list.push(SidebarLink {
			current: false,
			level: 0,
			name: PageName::category(&categories[i])
		});
	}

	//the prev and next pages
	let prev = if api_i == 0 {
		None
	} else {
		Some(PageName::api(&category.apis[api_i - 1]))
	};

	let next = if api_i == category.apis.len() - 1 {
		None
	} else {
		Some(PageName::api(&category.apis[api_i + 1]))
	};

	//finally, translate the api-page-specific fields from RawApi. "returns" and "args" are
	//parsed from an encoded string format. "markdown_desc" has a "See also:" paragraph appended
	//to it if see-also is present and non-empty. "kinds" and "pre_text" are trivial.
	let mut args = Vec::<ApiArg>::new();
	for arg in &api.args {
		let split_count = arg.split(' ').count();
		assert!(split_count >= 1 && split_count <= 3, "invalid arg {:?}", arg);

		let mut split = arg.split(' ');
		let name_str = split.next().unwrap();
		let ty_str = split.next();
		let reps_str = split.next();

		args.push(ApiArg {
			name: name_str.to_string(),
			to_describe: ty_str.is_some(),
			ty: match ty_str {
				Some(ty_str) => ApiType::from_str(ty_str),
				None => ApiType::Val
			},
			reps: match reps_str {
				None => ArgReps::One,
				Some("?") => ArgReps::Optional(None),
				Some(st) if st.starts_with("?") => {
					ArgReps::Optional(Some(st[1..].to_string()))
				}
				Some("*") => ArgReps::ZeroOrMore,
				Some("+") => ArgReps::OneOrMore,
				Some("2+") => ArgReps::TwoOrMore,
				Some(_) => panic!("invalid arg {:?}", arg)
			}
		});
	}

	let returns = api.returns.as_ref().map(|st| ApiType::from_str(st));

	let markdown_desc = match &api.see_also {
		Some(see_also) if !see_also.is_empty() => {
			let mut builder = api.text.clone();
			builder.push_str("\n\nSee also: ");

			for (i, filename) in see_also.iter().enumerate() {
				let page_name = PageName::from_filename(categories, filename);

				if page_name.is_code {
					write!(&mut builder, "[`{}`]({})", 
					      page_name.user_facing, page_name.href).unwrap();
				} else {
					write!(&mut builder, "[{}]({})", 
					      page_name.user_facing, page_name.href).unwrap();
				}

				if i < see_also.len() - 1 {
					builder.push_str(", ");
				}
			}

			builder
		}
		_ => api.text.clone()
	};

	let kinds = api.kinds.clone();
	let pre_text = api.pre_text.clone();

	//bring it all together
	Page {
		filename: page_name.filename,
		title: format!("{} - GameLisp Standard Library", page_name.user_facing),
		parent_href: Some(PageName::category(category).href),
		link_list,
		content: Content::Api {
			name: page_name.user_facing,
			args,
			returns,
			kinds,
			markdown_desc,
			pre_text
		},

		prev,
		next
	}
}

impl PageName {
	#[allow(dead_code)]
	fn homepage() -> PageName {
		PageName {
			is_code: false,
			user_facing: "Homepage".to_string(),
			filename: "homepage".to_string(),
			href: if cfg!(feature = "suffix-paths") { 
				"../index.html".to_string()
			} else { 
				"../".to_string()
			}
		}
	}

	fn index() -> PageName {
		PageName {
			is_code: false,
			user_facing: "Index".to_string(),
			filename: "index".to_string(),
			href: if cfg!(feature = "suffix-paths") {
				"index.html".to_string()
			} else {
				".".to_string()
			}
		}
	}

	fn alphabetic_index() -> PageName {
		PageName {
			is_code: false,
			user_facing: "Alphabetic Index".to_string(),
			filename: "alphabetic-index".to_string(),
			href: if cfg!(feature = "suffix-paths") {
				"alphabetic-index.html".to_string()
			} else {
				"alphabetic-index".to_string()
			}
		}
	}

	fn category(category: &RawCategory) -> PageName {
		PageName {
			is_code: false,
			user_facing: category.name.clone(),
			filename: category.filename.clone(),
			href: if cfg!(feature = "suffix-paths") {
				format!("{}.html", category.filename)
			} else {
				category.filename.clone()
			}
		}
	}

	fn api(api: &RawApi) -> PageName {
		PageName {
			is_code: true,
			user_facing: api.name.as_ref().unwrap_or(&api.filename).clone(),
			filename: api.filename.clone(),
			href: if cfg!(feature = "suffix-paths") {
				format!("{}.html", api.filename)
			} else {
				api.filename.clone()
			}
		}
	}

	//this has O(n) complexity, but it saves us from needing to save and pass around a lookup table
	fn from_filename(categories: &[RawCategory], filename: &str) -> PageName {
		for category in categories {
			if category.filename == filename {
				return PageName::category(category)
			}

			for api in &category.apis {
				if api.filename == filename {
					return PageName::api(api)
				}
			}
		}

		panic!("unrecognized filename {}", filename)
	}
}


//-------------------------------------------------------------------------------------------------
// building the search index
//-------------------------------------------------------------------------------------------------

fn build_search_index(categories: &[RawCategory]) -> String {
	let mut search_index = "let searchIndex = [\n".to_string();

	for category in categories {
		let page_name = PageName::category(category);
		write!(
			&mut search_index, 
			"\t[{:?}, {}, {:?}],\n",
			page_name.user_facing,
			false,
			page_name.href
		).unwrap();

		for api in &category.apis {
			let page_name = PageName::api(api);
			write!(
				&mut search_index, 
				"\t[{:?}, {}, {:?}],\n",
				page_name.user_facing,
				true,
				page_name.href
			).unwrap();

			if api.kinds.contains(&Kind::Abbrv) {
				write!(
					&mut search_index, 
					"\t[{:?}, {}, {:?}],\n",
					page_name.filename,
					true,
					page_name.href
				).unwrap();
			}
		}
	}

	search_index.push_str("];\n\n");
	search_index
}


//-------------------------------------------------------------------------------------------------
// images
//-------------------------------------------------------------------------------------------------

//detect whether any images in "output" are older than images in "input". if so, delete and
//regenerate them and return true. otherwise, do nothing and return false.
fn maybe_transform_images() -> bool{
	fn convert(dir_entry: walkdir::Result<DirEntry>) -> Option<PathBuf> {
		let dir_entry = dir_entry.unwrap();
		let path = dir_entry.path();

		if path.starts_with("output/reference") || path.starts_with("output/playground") {
			return None
		}

		match path.extension() {
			Some(ext) if ext == "png" => Some(path.to_path_buf()),
			_ => None
		}
	}

	let input_paths: Vec::<PathBuf> = WalkDir::new("input").into_iter()
	                                                       .filter_map(convert).collect();
	let output_paths: Vec::<PathBuf> = WalkDir::new("output").into_iter()
	                                                       .filter_map(convert).collect();

	fn path_age(path: &PathBuf) -> SystemTime {
		fs::metadata(path).unwrap().modified().unwrap()
	}

	let youngest_input = input_paths.iter().map(path_age).max();
	let youngest_output = output_paths.iter().map(path_age).max();

	if youngest_input < youngest_output {
		false
	} else {
		for output_path in &output_paths {
			fs::remove_file(output_path).unwrap();
		}

		transform_images();
		true
	}
}

fn transform_images() {
	let favicon_16 = image::open("input/favicon-16.png").unwrap();
	let favicon_24 = image::open("input/favicon-24.png").unwrap();

	//for the logo and the hamburger button, we just enlarge them 8x. this is much simpler than
	//trying to avoid image artefacts when using "image-rendering: crisp-edges", and it doesn't
	//bloat the filesize too much. (we don't bother to convert them to indexed-colour, because 
	//that would be fairly complex and it only reduces the filesize by 25% or so.)

	//we also use "palette.png" to automatically generate a dark-mode version of each sprite.
	let mut palette = HashMap::<Rgba<u8>, Rgba<u8>>::new();
	let palette_image = image::open("input/palette.png").unwrap();
	assert!(palette_image.width() == 2);
	for y in 0 .. palette_image.height() {
		palette.insert(palette_image.get_pixel(0, y), palette_image.get_pixel(1, y));
	}

	make_sprites(
		"output/logo-light.png",
		"output/logo-dark.png", 
		"input/logo.png",
		&palette
	);
	make_sprites(
		"output/logo-narrow-light.png",
		"output/logo-narrow-dark.png", 
		"input/logo-narrow.png",
		&palette
	);
	make_sprites(
		"output/hamburger-menu-light.png",
		"output/hamburger-menu-dark.png",
		"input/hamburger-menu.png",
		&palette
	);
	make_sprites(
		"output/hamburger-cancel-light.png",
		"output/hamburger-cancel-dark.png",
		"input/hamburger-cancel.png",
		&palette
	);
	make_sprites(
		"output/playground-play-light.png",
		"output/playground-play-dark.png",
		"input/playground-play.png",
		&palette
	);
	make_sprites(
		"output/playground-stop-light.png",
		"output/playground-stop-dark.png",
		"input/playground-stop.png",
		&palette
	);

	//for the favicon, we emit exact copies and 2x-upscaled copies
	favicon_16.save("output/favicon-16.png").unwrap();
	favicon_24.save("output/favicon-24.png").unwrap();

	let favicon_32 = favicon_16.resize(32, 32, imageops::Nearest);
	favicon_32.save("output/favicon-32.png").unwrap();

	let favicon_48 = favicon_24.resize(48, 48, imageops::Nearest);
	favicon_48.save("output/favicon-48.png").unwrap(); 

	//the patreon button is copied over unchanged
	fs::copy("input/patreon-button-light.png", "output/patreon-button-light.png").unwrap();
	fs::copy("input/patreon-button-dark.png", "output/patreon-button-dark.png").unwrap();
}

fn make_sprites(
	dst_light: &str,
	dst_dark: &str,
	src: &str,
	palette: &HashMap<Rgba<u8>, Rgba<u8>>
) {
	let light = image::open(src).unwrap();

	let mut dark = DynamicImage::new_rgba8(light.width(), light.height());
	for ((_, _, src), dst) in light.pixels().zip(dark.as_mut_rgba8().unwrap().pixels_mut()) {
		if src[3] == 0 {
			*dst = Rgba([0u8, 0, 0, 0]);
		} else {
			*dst = palette[&src];
		}
	}

	const SCALE: u32 = 8;
	for &(dst_path, ref image) in &[(dst_light, light), (dst_dark, dark)] {
		let large = image.resize(
			image.width() * SCALE,
			image.height() * SCALE,
			imageops::Nearest
		);
		large.save(dst_path).unwrap();
	}
}


//-------------------------------------------------------------------------------------------------
// main
//-------------------------------------------------------------------------------------------------

fn main() -> Result<(), Box<dyn Error>> {
	//clean up the `output` directory, creating it if it doesn't exist
	let cleanup_start = Instant::now();

	if !Path::new("output").is_dir() {
		fs::create_dir("output")?;
	}

	for result in fs::read_dir("output")? {
		let dir_entry = result?;
		let path = dir_entry.path();
		if path.is_file() {
			match path.extension() {
				Some(ext) if ext == "png" => (),
				_ => fs::remove_file(path)?
			}
		} else if path.is_dir() && !path.ends_with("reference") {
			fs::remove_dir_all(path)?;
		}
	}

	for subdir in &["output/std", "output/tcof", "output/playground"] {
		if !Path::new(subdir).is_dir() {
			fs::create_dir(subdir)?;
		}
	}

	println!("cleaned up the output directory in {:?}", cleanup_start.elapsed());

	//use mdbook to regenerate the reference, if any of the files in the "input/reference" 
	//directory are newer than "output/reference/index.html"
	let mut to_rebuild_reference = false;

	let reference_index_path = PathBuf::from("output/reference/index.html");
	if reference_index_path.exists() && reference_index_path.is_file() {
		let last_built = fs::metadata(reference_index_path)?.modified()?;
		for result in WalkDir::new("input/reference") {
			let dir_entry = result?;
			let path = dir_entry.path();

			let modified = fs::metadata(path)?.modified()?;
			if modified > last_built {
				to_rebuild_reference = true;
				break
			}
		}
	} else {
		to_rebuild_reference = true;
	}

	if to_rebuild_reference {
		let mdbook_start = Instant::now();

		let mdbook = MDBook::load("input/reference")?;
		mdbook.build()?;

		println!("reference rebuilt in {:?}", mdbook_start.elapsed());
	} else {
		println!("output/reference/index.html is up-to-date");
	}

	//copy over a few files without changing them
	fs::copy(format!("input/{}", STYLESHEET), format!("output/{}", STYLESHEET))?;
	fs::copy(format!("input/{}", TCOF_SPLASH_IMAGE), format!("output/{}", TCOF_SPLASH_IMAGE))?;
	for name in &[GLSP_PLAYGROUND_JS, GLSP_PLAYGROUND_BG_WASM] {
		fs::copy(format!("input/playground/{}", name), format!("output/playground/{}", name))?;
	}
	fs::write("output/.nojekyll", "")?;
	fs::copy("input/playground/spritesheet.png", "output/playground/spritesheet.png")?;
	fs::copy("input/playground/spritesheet.json", "output/playground/spritesheet.json")?;

	//generate the images
	let images_start = Instant::now();
	if maybe_transform_images() {
		println!("generated images in {:?}", images_start.elapsed());
	} else {
		println!("images are up-to-date");
	}

	//parse the input .toml files for the stdlib
	let toml_start = Instant::now();

	let toml_files = &[
		"special-forms.toml",
		"abbreviations.toml",
		"macros.toml",
		"types.toml",
		"evaluation.toml",
		"numbers.toml",
		"collections.toml",
		"strings-and-text.toml",
		"iterators.toml",
		"objects-and-classes.toml",
		"miscellaneous.toml"
	];

	let mut categories = Vec::new();

	for filename in toml_files {
		let path = format!("input/std/{}", filename);
		let category: RawCategory = toml::from_str(&fs::read_to_string(&path)?)?;
		categories.push(category.preprocess());
	}

	println!("parsed .toml files in {:?}", toml_start.elapsed());

	//build the search index, prepend it to std-search.js, and copy that over
	let search_start = Instant::now();

	let std_search = fs::read_to_string(&format!("input/std/{}", SEARCH_JS))?;
	let prepended_search = format!("{}{}", build_search_index(&categories[..]), std_search);
	fs::write(&format!("output/std/{}", SEARCH_JS), prepended_search)?;

	println!("created search index in {:?}", search_start.elapsed());

	//prepend each demo .glsp file to playground.js, and copy it over
	let mut playground_js = String::new();
	playground_js.push_str("let demoSources = {\n");
	for demo_path in DEMOS {
		let demo_text = fs::read_to_string(&format!("input/playground/{}", demo_path))?;
		write!(&mut playground_js, "\t{:?}: {:?},\n", demo_path, demo_text).unwrap();
	}
	playground_js.push_str("};\n\n");

	let playground_js_text = fs::read_to_string(&format!("input/playground/{}", PLAYGROUND_JS))?;
	playground_js.push_str(&playground_js_text);

	fs::write(&format!("output/playground/{}", PLAYGROUND_JS), playground_js)?;

	//generate the Pages, and render each one into a .html document. also append each page's
	//<body> to a literal, prepend the finished literal onto std-prelude.js, and copy it over.
	let pages_start = Instant::now();

	let mut preload_bodies = "let preloadBodies = {\n".to_string();
	let pages = convert(&categories[..]);
	for (i, page) in pages.iter().enumerate() {
		let body = render_page_body(&page);
		write!(
			&mut preload_bodies,
			"\t{:?}: [{:?}, {:?}]{}\n",
			if cfg!(feature = "suffix-paths") { 
				format!("{}.html", page.filename)
			} else {
				page.filename.clone()
			},
			&page.title,
			&body,
			if i == pages.len() - 1 { "" } else { "," } //json forbids trailing commas
		).unwrap();

		let full = render_boilerplate(Boilerplate::Std, &page.title, &body);
		fs::write(format!("output/std/{}.html", page.filename), &full)?;
	}

	preload_bodies.push_str("};\n\n");
	let std_preload = fs::read_to_string(&format!("input/std/{}", PRELOAD_JS))?;
	let prepended_preload = format!("{}{}", &preload_bodies, &std_preload);
	fs::write(&format!("output/std/{}", PRELOAD_JS), prepended_preload)?;

	//render the homepage and store it in output/index.html
	fs::write("output/index.html", &render_homepage())?;

	//render the playground page and store it in output/playground/index.html
	fs::write("output/playground/index.html", &render_playground())?;

	//render the tcof splash page and store it in output/tcof/index.html
	fs::write("output/tcof/index.html", &render_tcof_splash())?;

	//render a short explanation to output/README.md, for the github repo
	fs::write(
		"output/README.md",
		"Static files for the gamelisp.rs website. Generated using a custom static site \
		generator, which can be found in the `website` subdirectory of the `glsp` repository."
	)?;

	println!("generated and rendered pages in {:?}", pages_start.elapsed());

	//crawl each .html document in "output" and emit it as a sitemap.xml. emit a robots.txt
	//which points to it.
	let sitemap_start = Instant::now();

	let mut sitemap = r#"<?xml version="1.0" encoding="UTF-8"?>"#.to_string();
	sitemap.push('\n');
	sitemap.push_str(r#"<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">"#);
	sitemap.push('\n');

	for result in WalkDir::new("output") {
		let dir_entry = result?;
		let path = dir_entry.path().strip_prefix("output")?;

		if path.extension() == Some(OsString::from("html").as_os_str()) {
			write!(&mut sitemap, "\t<url><loc>{}", SITEMAP_PREFIX).unwrap();

			if path.starts_with("reference") {
				for component in path.components() {
					sitemap.push('/');

					let os_str: &OsStr = component.as_ref();
					sitemap.push_str(&os_str.to_string_lossy());
				}
			} else {
				for component in path.components() {
					sitemap.push('/');

					let os_str: &OsStr = component.as_ref();
					let comp_str = os_str.to_string_lossy();

					if comp_str.ends_with(".html") {
						if comp_str != "index.html" {
							sitemap.push_str(comp_str.strip_suffix(".html").unwrap());
						}
					} else {
						sitemap.push_str(&comp_str);
					}
				}
			}

			sitemap.push_str("</loc></url>\n");
		}
	}

	sitemap.push_str("</urlset>");
	fs::write("output/sitemap.xml", &sitemap)?;

	fs::write("output/robots.txt", &format!("\
		User-agent: *\n\
		Allow: /\n\
		\n\
		Sitemap: {}/sitemap.xml\n\
	", SITEMAP_PREFIX))?;

	println!("generated sitemap.xml in {:?}", sitemap_start.elapsed());

	//finished
	Ok(())
}
