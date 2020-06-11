#![feature(test)]

use glsp::prelude::*;
use lazy_static::lazy_static;
use pyo3::prelude::*;
use rlua::{self, Lua};
use std::{collections::HashMap, fs, hint::black_box, time::Instant};

lazy_static! {
	static ref START_INSTANT: Instant = Instant::now();
}

fn main() {

	// Rust ---------------------------------------------------------------------------------------

	static BENCHMARKS: [(&str, fn()); 11] = [
		("primitive_inc", primitive_inc),
		("primitive_arith", primitive_arith),
		("primitive_call0", primitive_call0),
		("primitive_call3", primitive_call3),
    	("primitive_array", primitive_array),
    	("primitive_table", primitive_table),
    	("primitive_field", primitive_field),
    	("primitive_method", primitive_method),
    	("rects", rects),
    	("flood_fill", flood_fill),
    	("rotation", rotation)
    ];

    for &(name, benchmark) in &BENCHMARKS {
    	let start = Instant::now();
    	benchmark();
    	let elapsed = start.elapsed().as_secs_f64() * 1000.0;
    	println!("Rust {}: {:.1}ms", name, elapsed);
    }

    println!();


	// Lua ----------------------------------------------------------------------------------------
	
	let benchmarks_lua = fs::read_to_string("src/benchmarks.lua").unwrap();

	let lua = Lua::new();
	lua.context::<_, ()>(|lua| {
		//lua doesn't have an accurate timer, so we need to register one
		let globals = lua.globals();
		let precise_time = lua.create_function(|_, ()| {
			Ok(START_INSTANT.elapsed().as_secs_f64())
		}).unwrap();
		globals.set("precise_time", precise_time).unwrap();

		lua.load(&benchmarks_lua).exec().unwrap();
	});

	println!();


	// GameLisp -----------------------------------------------------------------------------------

	let glsp = Runtime::new();
	glsp.run(|| {
		glsp::load("src/benchmarks.glsp")?;
		Ok(())
	}).unwrap();

	println!();


	// Python -------------------------------------------------------------------------------------
	
	let benchmarks_py = fs::read_to_string("src/benchmarks.py").unwrap();

	let gil = Python::acquire_gil();
	let py = gil.python();

	match py.run(&benchmarks_py, None, None) {
		Ok(_) => (),
		Err(py_err) => py_err.print(py)
	}

	//without this line, errors won't be printed. alternatively, the PYTHONUNBUFFERED environment
	//variable can be set to 1.
	py.run("import sys; sys.stdout.flush(); sys.stderr.flush()", None, None).unwrap();	

	println!();
}

fn primitive_inc() {
	let mut x = 0;
	for i in 0 .. 1000_000 {
		black_box(i);

		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
		x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); x += 1; black_box(x); 
	}
	black_box(x);
}

fn primitive_arith() {
	let mut x = 0f64;
	for i in 0 .. 1000_000 {
		black_box(i);

		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
		x += 500.0; x -= 250.0; x *= 1.5; x /= 42.0; x %= 18.0; x = -x; black_box(x);
	}
	black_box(x);
}

fn primitive_call0() {
	#[inline(never)]
	fn a() -> i32 { 1 }

	let mut x = 0;
	for i in 0 .. 100_000 {
		black_box(i);

		for j in 0 .. 10 {
			black_box(j);
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x); 
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x); 
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x); 
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x); 
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x); 
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x); 
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x); 
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x); 
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x); 
			x = black_box(a()); black_box(x); x = black_box(a()); black_box(x);
		}
		black_box(x);
	}
	black_box(x);
}

fn primitive_call3() {
	#[inline(never)]
	fn a(b: i32, c: i32, d: i32) -> i32 {
		black_box((b, c, d));
		1
	}

	let mut x = 0;
	for i in 0 .. 100_000 {
		black_box(i);

		for j in 0 .. 10 {
			black_box(j);
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x); 
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x); 
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x); 
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x); 
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x); 
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x); 
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x); 
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x); 
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x); 
			x = black_box(a(1, 2, 3)); black_box(x); x = black_box(a(1, 2, 3)); black_box(x);
		}
		black_box(x);
	}
	black_box(x);
}

fn primitive_array() {
	let mut a = vec![1_i32, 2, 3, 4, 5, 6, 7, 8, 9, 10];

	let mut x = 0;
	for i in 0 .. 100_000 {
		black_box(i);

		for j in 0 .. 10 {
			black_box(j);

			x = black_box(a[0]); black_box((x, &mut a));
			x = black_box(a[1]); black_box((x, &mut a));
			x = black_box(a[2]); black_box((x, &mut a));
			x = black_box(a[3]); black_box((x, &mut a));
			x = black_box(a[4]); black_box((x, &mut a));
			x = black_box(a[5]); black_box((x, &mut a));
			x = black_box(a[6]); black_box((x, &mut a));
			x = black_box(a[7]); black_box((x, &mut a));
			x = black_box(a[8]); black_box((x, &mut a));
			x = black_box(a[9]); black_box((x, &mut a));
		}
		black_box(x);
	}
	black_box(x);
}

fn primitive_table() {
	let mut t = HashMap::<&'static str, i32>::new();
	t.insert("a", 0);
	t.insert("b", 1);
	t.insert("c", 2);
	t.insert("d", 3);
	t.insert("e", 4);
	t.insert("f", 5);
	t.insert("g", 6);
	t.insert("h", 7);
	t.insert("i", 8);
	t.insert("j", 9);

	let mut x = 0;
	for i in 0 .. 100_000 {
		black_box(i);

		for j in 0 .. 10 {
			black_box(j);

			x = black_box(t.insert("a", 0).unwrap()); black_box((x, &mut t));
			x = black_box(t.insert("b", 1).unwrap()); black_box((x, &mut t));
			x = black_box(t.insert("c", 2).unwrap()); black_box((x, &mut t));
			x = black_box(t.insert("d", 3).unwrap()); black_box((x, &mut t));
			x = black_box(t.insert("e", 4).unwrap()); black_box((x, &mut t));
			x = black_box(t.insert("f", 5).unwrap()); black_box((x, &mut t));
			x = black_box(t.insert("g", 6).unwrap()); black_box((x, &mut t));
			x = black_box(t.insert("h", 7).unwrap()); black_box((x, &mut t));
			x = black_box(t.insert("i", 8).unwrap()); black_box((x, &mut t));
			x = black_box(t.insert("j", 9).unwrap()); black_box((x, &mut t));
		}
		black_box(x);
	}
	black_box(x);
}

fn primitive_field() {
	struct S {
		a: i32, b: i32, c: i32, d: i32, e: i32, f: i32, g: i32, h: i32, i: i32, j: i32
	}

	let mut o = S { a: 0, b: 1, c: 2, d: 3, e: 4, f: 5, g: 6, h: 7, i: 8, j: 9 };

	let mut x = 0;
	for i in 0 .. 100_000 {
		black_box(i);

		for j in 0 .. 10 {
			black_box(j);

			x = black_box(o.a); black_box((x, &mut o));
			x = black_box(o.b); black_box((x, &mut o));
			x = black_box(o.c); black_box((x, &mut o));
			x = black_box(o.d); black_box((x, &mut o));
			x = black_box(o.e); black_box((x, &mut o));
			x = black_box(o.f); black_box((x, &mut o));
			x = black_box(o.g); black_box((x, &mut o));
			x = black_box(o.h); black_box((x, &mut o));
			x = black_box(o.i); black_box((x, &mut o));
			x = black_box(o.j); black_box((x, &mut o));
		}
		black_box(x);
	}
	black_box(x);
}

fn primitive_method() {
	struct S;
	impl S {
		#[inline(never)]
		fn a(&self) -> i32 { 1 }
	}

	let mut s = S;

	let mut x = 0;
	for i in 0 .. 100_000 {
		black_box(i);

		for j in 0 .. 10 {
			black_box(j);

			x = black_box(s.a()); black_box((x, &mut s));
			x = black_box(s.a()); black_box((x, &mut s));
			x = black_box(s.a()); black_box((x, &mut s));
			x = black_box(s.a()); black_box((x, &mut s));
			x = black_box(s.a()); black_box((x, &mut s));
			x = black_box(s.a()); black_box((x, &mut s));
			x = black_box(s.a()); black_box((x, &mut s));
			x = black_box(s.a()); black_box((x, &mut s));
			x = black_box(s.a()); black_box((x, &mut s));
			x = black_box(s.a()); black_box((x, &mut s));
		}
		black_box(x);
	}
	black_box(x);
}

fn rects() {
	struct Rect { x: i32, y: i32, w: i32, h: i32 }

	impl Rect {
		fn intersects(&self, other: &Rect) -> bool {
			self.x + self.w > other.x &&
			self.x < other.x + other.w &&
			self.y + self.h > other.y &&
			self.y < other.y + other.h
		}
	}

	let mut rng = 75_i32;
	let mut rng_gen = || {
		rng = (rng * 75) % 65537;
		rng
	};

	let mut rects = Vec::with_capacity(2000);
	for _ in 0 .. 2000 {
		rects.push(Rect {
			x: rng_gen(),
			y: rng_gen(),
			w: rng_gen(),
			h: rng_gen()
		});
	}

	let mut count = 0;
	for (i, rect) in rects.iter().enumerate() {
		for other in &rects[i+1..] {
			if rect.intersects(other) {
				count += 1;
			}
		}
	}

	black_box(count);
}

fn flood_fill() {
	const WIDTH: usize = 1000_000;
	let mut pixels = vec![0_u8; WIDTH];
	let mut to_check: Vec<usize> = vec![pixels.len() / 2];

	let mut painted: usize = 0;
	while let Some(x) = to_check.pop() {
		black_box(x);
		black_box(&mut pixels);

		pixels[x] = 1;
		painted += 1;

		if x < WIDTH - 1 && pixels[x + 1] == 0 {
			to_check.push(x + 1);
		}

		if x > 0 && pixels[x - 1] == 0 {
			to_check.push(x - 1);
		}
	}

	assert!(painted == WIDTH);
}

fn rotation() {
	let mut rng = 75_i32;
	let mut rng_gen = || {
		rng = (rng * 75) % 65537;
		rng
	};

	struct Point {
		x: f32,
		y: f32
	}

	impl Point {
		fn rotate(&mut self, diff: f32) {
			let distance = ((self.x * self.x) + (self.y * self.y)).sqrt();
			let angle = f32::atan2(self.y, self.x);
			let new_angle = angle + diff;
			self.x = distance * new_angle.cos();
			self.y = distance * new_angle.sin();
		}
	}

	let mut points = Vec::with_capacity(300);
	for _ in 0 .. 300 {
		points.push(Point {
			x: rng_gen() as f32,
			y: rng_gen() as f32
		});
	}

	const STEP: f32 = (0.1 * 3.14159) / 180.0;
	for _ in 0 .. 3600 {
		for point in &mut points {
			point.rotate(STEP);
		}
	}

	black_box(points);
}
