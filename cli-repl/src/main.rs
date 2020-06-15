// Author: Alberto González Palomo https://sentido-labs.com
// ©2019,2020 Alberto González Palomo https://sentido-labs.com
// Created: 2019-05-07 17:50
//
// This file shows how to:
// - create a GameLisp runtime instance with options (`sandboxed`, ...).
// - load source files into it.
// - parse and evaluate single expressions in that runtime.
// - bind Rust functions to GameLisp symbols (see `www(url)` below).

use glsp::prelude::*;

use gong::{ ItemClass, Item };
use linefeed::{ Interface, ReadResult, terminal::DefaultTerminal };

static OPTS: gong::options::OptionSet = gong::gong_option_set_fixed!(
    [
        gong::gong_longopt!("help"),
        gong::gong_longopt!("version"),
        gong::gong_longopt!("sandboxed"),
    ],
    [
        gong::gong_shortopt!('h'),
        gong::gong_shortopt!('v'),
        gong::gong_shortopt!('q'),
    ]
);

const HISTORY_FILE: &str = concat!(env!("CARGO_PKG_NAME"), "_history");

fn usage() {
    eprintln!(r#"Usage: glsp <options> [file.glsp file2.glsp ...]
  -h, -help
      Show this message.
  -v, --version
      Show the version number ({version}) and exit.
  -q
      Quit before starting the REPL, to use as a command in shell scripts.
  --sandboxed
      Disables filesystem access: load, include, and require.
      https://gamelisp.rs/reference/the-glsp-crate.html#sandboxing

  The command input history is stored in "./{history}"."#,
        version = env!("CARGO_PKG_VERSION"),
        history = HISTORY_FILE,
    );
}

fn main() {
    debug_assert!(OPTS.is_valid());

    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut files = Vec::new();
    let mut run_repl  = true;
    let mut sandboxed = false;
    let analysis = OPTS.process(&args[..]);
    for &o in analysis.items.iter() {
        match o {
            ItemClass::Ok(Item::Short(_, 'h')) |
            ItemClass::Ok(Item::Long(_, "help")) => {
                usage();
                return;
            },
            ItemClass::Ok(Item::Short(_, 'v')) |
            ItemClass::Ok(Item::Long(_, "version")) => {
                eprintln!("{}", env!("CARGO_PKG_VERSION"));
                return;
            },
            ItemClass::Ok(Item::Short(_, 'q')) => {
                run_repl = false;
            },
            ItemClass::Ok(Item::Long(_, "sandboxed")) => {
                sandboxed = true;
            },
            ItemClass::Ok(Item::NonOption(_, file_name)) => {
                files.push(file_name);
            },
            ItemClass::Ok(Item::Short(_, _)) => {},
            ItemClass::Ok(Item::Long (_, _)) => {},
            ItemClass::Ok(Item::LongWithData  {i:_, n:_, d:_, l:_}) => {},
            ItemClass::Ok(Item::ShortWithData {i:_, c:_, d:_, l:_}) => {},

            ItemClass::Ok(Item::EarlyTerminator(_)) => { /* -- found */ },

            ItemClass::Warn(item) => {
                eprintln!("WARNING: {:?}", item);
            },
            ItemClass::Err(item) => {
                eprintln!("ERROR: {:?}", item);
            }
        }
    }

    let runtime = RuntimeBuilder::new().sandboxed(sandboxed).build();
    runtime.run(|| {
        // These are the defaults, set here to show how to redirect output:
        // https://gamelisp.rs/reference/the-glsp-crate.html#output-streams
        glsp::set_pr_writer(Box::new(std::io::stdout()));
        glsp::set_epr_writer(Box::new(std::io::stderr()));

        glsp::bind_rfn("help", rfn!(help))?;
        if !sandboxed {
            glsp::bind_rfn("www", rfn!(www))?;
        }

        for file_name in files { prn!("{}", glsp::load(file_name)?); }

        Ok(())
    });

    if !run_repl { return; }

    eprintln!(
        r#";;; {name} {version}                                       {homepage}
;;; “help name” is short for “(www (help "name"))” and shows the documentation
;;   for “name” in a web browser.
;;   E.g.: rust, lisp, std, special-forms, abbreviations, if, ', @, #n, #t, #f
;;   Without argument, shows the reference manual.
;;; (www "url") loads “url” in a web browser, unless using --sandboxed."#,
        name     = env!("CARGO_PKG_NAME"),
        version  = env!("CARGO_PKG_VERSION"),
        homepage = env!("CARGO_PKG_HOMEPAGE"),
    );

    match Interface::new(env!("CARGO_PKG_NAME")) {
        Ok(cli) => {
            repl(runtime, sandboxed, cli);
        },
        Err(err) => {
            eprintln!(";;; Terminal is not fully functional: {}", err);
            degraded_repl(runtime, sandboxed);
        }
    };
}

fn repl(runtime: Runtime, sandboxed: bool, cli: Interface<DefaultTerminal>) {
    let cwd = std::env::current_dir().unwrap();

    eprintln!(
        r#";;; CTRL+D to exit, command input history stored in "./{history}"."#,
        history  = HISTORY_FILE,
    );

    if let Err(e) = cli.load_history(HISTORY_FILE) {
        if e.kind() != std::io::ErrorKind::NotFound {
            eprintln!("ERROR: failed to load REPL history from {} at {}, {:?}",
                      HISTORY_FILE, cwd.display(), e);
        }
    }

    if std::env::var("TERM").map_or(false, |s| { s != "dumb" }) {
        // These options need features not available in dumb terminals.
        cli.lock_reader().set_blink_matching_paren(true);
    }

    loop {
        // Make prompt work like in Interlisp-D:
        cli.set_prompt(&format!("{}← ", 1 + cli.history_len())).unwrap();
        match cli.read_line() {
            Ok(line) => {
                match line {
                    ReadResult::Input(line) => {
                        runtime.run(|| {
                            match eval_line(&line, sandboxed) {
                                Ok(result) => { prn!("{}", result); },
                                Err(err) => { eprn!("{}", err.val()); }
                            }
                            Ok(())
                        });
                        cli.add_history_unique(line);
                    },
                    ReadResult::Signal(signal) => {
                        eprintln!("Signal: {:?}", signal);
                        break;
                    },
                    ReadResult::Eof => { break; },
                }
            },
            Err(err) => {
                eprintln!("Error: failed to read command line: {:?}", err);
                eprintln!("Switching to degraded mode.");
                degraded_repl(runtime, sandboxed);
                break;
            }
        }
    }

    if let Err(e) = cli.save_history(HISTORY_FILE) {
        eprintln!("Error: failed to store command history in {} from {}, {:?}",
                  HISTORY_FILE, cwd.display(), e);
    }
}

use std::io::{BufRead};
fn degraded_repl(runtime: Runtime, sandboxed: bool) {
    eprint!("> ");
    for line in std::io::stdin().lock().lines() {
        match line {
            Ok(line) => {
                runtime.run(|| {
                    match eval_line(&line, sandboxed) {
                        Ok(result) => { prn!("{}", result); },
                        Err(err) => { eprn!("{}", err.val()); }
                    }
                    Ok(())
                });
            },
            Err(err) => {
                eprintln!("Error: failed to read command line: {:?}", err);
                break;
            }
        }
        eprint!("> ");
    }
}

fn eval_line(mut line: &str, sandboxed: bool) -> GResult<Val> {
    line = line.trim_start().trim_end();
    if line == "help" || line.starts_with("help ") {
        let url = help(Some(line[4..].trim_start()));
        if sandboxed {
            prn!("{}", url);
            url.to_val()
        } else {
            match www(Some(&url)) {
                Ok(url) => url.to_val(),
                Err(err) => Err(err)
            }
        }
    } else {
        match glsp::parse_all(line, None) {
            Ok(values) => {
                match glsp::eval_multi(&values, Some(EnvMode::Copied)) {
                    Ok(result) => Ok(result),
                    Err(err)   => Err(error!("Evaluation error: {}", err.val()))
                }
            },
            Err(err) => {
                Err(error!("Syntax error: {}", err.val()))
            }
        }
    }
}

fn help(symbol: Option<&str>) -> String {
    let mut symbol = (match symbol.unwrap_or("") {
        "="      => "set",
        "'"      => "quote-abbrv",
        "`"      => "backquote-abbrv",
        "~"      => "unquote-abbrv",
        ".."     => "splay-abbrv",
        "@"      => "atsign-abbrv",
        "."      => "meth-name-abbrv",
        "[]"     => "access-abbrv",
        "\"{}\"" => "template-str-abbrv",
        "cond==" => "cond-num-eq",
        "->"     => "arrow-first",
        "->>"    => "arrow-last",
        "+"      => "add",
        "-"      => "sub",
        "*"      => "mul",
        "/"      => "div",
        "%"      => "rem",
        "=="     => "num-eq",
        "<"      => "lt",
        "<="     => "lte",
        ">"      => "gt",
        ">="     => "gte",
        "name"       => "name-clause",
        "mixin"      => "mixin-clause",
        "field"      => "field-clause",
        "const"      => "const-clause",
        "meth"       => "meth-clause",
        "prop"       => "prop-clause",
        "init"       => "init-clause",
        "init-mixin" => "init-mixin-clause",
        "fini"       => "fini-clause",
        "fini-mixin" => "fini-mixin-clause",
        "wrap"       => "wrap-clause",
        "wrap-prop"  => "wrap-prop-clause",
        "state"      => "state-clause",
        "state*"     => "statex-clause",
        "fsm"        => "fsm-clause",
        "init-state" => "init-state-clause",
        "fini-state" => "fini-state-clause",
        symbol => symbol
    })  .replace("?",  "-p")
        .replace("!",  "-mut")
        .replace("@",  "atsign-")
        .replace("==", "num-eq-")
        .replace("->", "-to-");
    if symbol.ends_with("=") {
        symbol.insert_str(0, "set-");
        symbol.pop();
    }

    match symbol.as_str() {
        ""       => "https://gamelisp.rs/reference/",
        "rust"   => "https://docs.rs/glsp/",
        "lisp"|"car"|"cdr"|"cons"|"first"|"rest"|"nth"|"lambda"|"progn"|"t" =>
            "ref:introduction-for-lisp-programmers.html",
        ";"|","|"#;"|"#|"|"|#"|"#||#" =>
            "ref:syntax-and-types.html#whitespace",
        "#n"|"nil"|"()" =>
            "ref:syntax-and-types.html#nil",
        "#t"|"#f" =>
            "ref:syntax-and-types.html#bool",
        "bool"|"int"|"flo"|"char"|"sym"|"arr"|"str"|
        "tab"|"iter"|"obj"|"class"|"fn"|"coro"|"rfn"|"rdata" =>
            "ref:syntax-and-types.html#type-summary",
        "num"|"deque"|"callable"|"expander"|"iterable" =>
            "ref:syntax-and-types.html#abstract-types",
        "std" => "https://gamelisp.rs/std/",
        _     => {
            symbol.insert_str(0, "https://gamelisp.rs/std/");
            symbol.as_str()
        }
    }.replace("ref:", "https://gamelisp.rs/reference/")
}

fn www(url: Option<&str>) -> GResult<&str> {
    let url = url.unwrap_or("https://gamelisp.rs");
    match webbrowser::open(url) {
        Ok(_) => {
            Ok(url)
        },
        Err(err) => {
            Err(error!("Failed to open URL: {}", url).with_source(err))
        }
    }
}
