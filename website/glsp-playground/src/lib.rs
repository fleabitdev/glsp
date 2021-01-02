use glsp::prelude::*;
use std::{io, str};
use std::cell::{RefCell};
use wasm_bindgen::prelude::*;
use web_sys::{console};

#[wasm_bindgen]
extern "C" {
	#[wasm_bindgen(catch, js_name = playMouseX)]
	fn play_mouse_x() -> Result<i32, JsValue>;

	#[wasm_bindgen(catch, js_name = playMouseY)]
	fn play_mouse_y() -> Result<i32, JsValue>;

	#[wasm_bindgen(catch, js_name = playDownP)]
	fn play_down_p(input: String) -> Result<bool, JsValue>;

	#[wasm_bindgen(catch, js_name = playPressedP)]
	fn play_pressed_p(input: String) -> Result<bool, JsValue>;

	#[wasm_bindgen(catch, js_name = playReleasedP)]
	fn play_released_p(input: String) -> Result<bool, JsValue>;

	#[wasm_bindgen(catch, js_name = playFill)]
	fn play_fill(x: f32, y: f32, width: f32, height: f32, 
	             r: f32, g: f32, b: f32) -> Result<(), JsValue>;

	#[wasm_bindgen(catch, js_name = playDraw)]
	fn play_draw(sprite: String, x: f32, y: f32, 
	             hflip: bool, vflip: bool, frame: f32) -> Result<(), JsValue>;
}

fn play_mouse_x_wrapper() -> GResult<i32> {
	match play_mouse_x() {
		Ok(x) => Ok(x),
		Err(js_err) => bail!("{:?}", js_err)
	}
}

fn play_mouse_y_wrapper() -> GResult<i32> {
	match play_mouse_y() {
		Ok(y) => Ok(y),
		Err(js_err) => bail!("{:?}", js_err)
	}
}

fn play_down_p_wrapper(sym: Sym) -> GResult<bool> {
	match play_down_p(sym.to_string()) {
		Ok(b) => Ok(b),
		Err(js_err) => bail!("{:?}", js_err)
	}
}

fn play_pressed_p_wrapper(sym: Sym) -> GResult<bool> {
	match play_pressed_p(sym.to_string()) {
		Ok(b) => Ok(b),
		Err(js_err) => bail!("{:?}", js_err)
	}
}

fn play_released_p_wrapper(sym: Sym) -> GResult<bool> {
	match play_released_p(sym.to_string()) {
		Ok(b) => Ok(b),
		Err(js_err) => bail!("{:?}", js_err)
	}
}

fn play_fill_wrapper(x: Num, y: Num, width: Num, height: Num, 
                     r: Num, g: Num, b: Num) -> GResult<()> {
	match play_fill(x.into_f32(), y.into_f32(), width.into_f32(), height.into_f32(), 
	                r.into_f32(), g.into_f32(), b.into_f32()) {
		Ok(()) => Ok(()),
		Err(js_err) => bail!("{:?}", js_err)
	}
}

fn play_draw_wrapper(sprite: Sym, x: Num, y: Num, flags: Rest<Val>) -> GResult<()> {
	let mut hflip: Option<bool> = None;
	let mut vflip: Option<bool> = None;
	let mut frame: Option<f32> = None;

	let mut flags = &flags[..];
	while flags.len() > 0 {
		match flags {
			[Val::Sym(flag), ..] if &*flag.name() == "hflip" => {
				ensure!(hflip.is_none(), "duplicate 'hflip flag passed to play:draw");
				hflip = Some(true);
				flags = &flags[1..];
			}
			[Val::Sym(flag), ..] if &*flag.name() == "vflip" => {
				ensure!(vflip.is_none(), "duplicate 'vflip flag passed to play:draw");
				vflip = Some(true);
				flags = &flags[1..];
			}
			[Val::Sym(flag), Val::Int(frame_i), ..] if &*flag.name() == "frame" => {
				ensure!(frame.is_none(), "duplicate 'frame flag passed to play:draw");
				frame = Some(*frame_i as f32);
				flags = &flags[2..];
			}
			[Val::Sym(flag), Val::Flo(frame_f), ..] if &*flag.name() == "frame" => {
				ensure!(frame.is_none(), "duplicate 'frame flag passed to play:draw");
				frame = Some(*frame_f);
				flags = &flags[2..];
			}
			_ => bail!("invalid flags passed to play:draw: {:?}", flags)
		}
	}

	match play_draw(
		sprite.name().to_string(),
		x.into_f32(),
		y.into_f32(),
		hflip.unwrap_or(false),
		vflip.unwrap_or(false),
		frame.unwrap_or(0.0)
	) {
		Ok(()) => Ok(()),
		Err(js_err) => bail!("{:?}", js_err)
	}
}

struct Engine {
	runtime: Runtime,
	width: u32,
	height: u32,
	title: String,
	blurb: String
}

struct ConsoleWriter {
	buffer: Vec<u8>
}

impl ConsoleWriter {
	fn new() -> ConsoleWriter {
		ConsoleWriter {
			buffer: Vec::new()
		}
	}
}

impl std::io::Write for ConsoleWriter {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		self.buffer.extend_from_slice(buf);

		while let Some(newline_i) = self.buffer.iter().position(|byte| *byte == b'\n') {
			let valid_str = match str::from_utf8(&self.buffer[..newline_i]) {
				Ok(valid_str) => valid_str,
				Err(err) => str::from_utf8(&self.buffer[..err.valid_up_to()]).unwrap()
			};

			console::log_1(&JsValue::from_str(valid_str));

			self.buffer.drain(..newline_i+1);
		}

		Ok(buf.len())
	}

	fn flush(&mut self) -> io::Result<()> {
		Ok(())
	}
}

thread_local! {
	static ENGINE: RefCell<Option<Box<Engine>>> = RefCell::new(None);
}

fn with_engine<F: FnOnce(&Engine) -> Result<R, JsValue>, R>(f: F) -> Result<R, JsValue> {
	ENGINE.with(|engine| {
		let option = engine.borrow();
		
		if option.is_none() {
			Err(JsValue::from_str("GameLisp Engine not yet initialized"))
		} else {
			f(&*option.as_ref().unwrap())
		}
	})
}

#[wasm_bindgen(js_name = initEngine)]
pub fn init_engine(text: String, filename: String, rand_seed: f32) -> Result<(), JsValue> {
	console_error_panic_hook::set_once();

	ENGINE.with(|engine| {
		let mut option = engine.borrow_mut();
		*option = None;

		let mut width: u32 = 0;
		let mut height: u32 = 0;
		let mut title: String = String::new();
		let mut blurb: String = String::new();

		let runtime = Runtime::new();
		let result: Result<(), JsValue> = runtime.run(|| {
			glsp::set_pr_writer(Box::new(ConsoleWriter::new()));
			glsp::set_epr_writer(Box::new(ConsoleWriter::new()));

			glsp::rand_reseed(rand_seed.to_bits() as i32);

			glsp::bind_rfn("play:mouse-x", &play_mouse_x_wrapper).unwrap();
			glsp::bind_rfn("play:mouse-y", &play_mouse_y_wrapper).unwrap();
			glsp::bind_rfn("play:down?", &play_down_p_wrapper).unwrap();
			glsp::bind_rfn("play:pressed?", &play_pressed_p_wrapper).unwrap();
			glsp::bind_rfn("play:released?", &play_released_p_wrapper).unwrap();
			glsp::bind_rfn("play:fill", &play_fill_wrapper).unwrap();
			glsp::bind_rfn("play:draw", &play_draw_wrapper).unwrap();

			let vals = match glsp::parse_all(&text, Some(&filename)) {
				Ok(vals) => vals,
				Err(err) => return Ok(Err(JsValue::from_str(&err.to_string())))
			};

			let _: Val = match glsp::eval_multi(&vals, None) {
				Ok(val) => val,
				Err(err) => return Ok(Err(JsValue::from_str(&err.to_string())))
			};

			const MIN_SIZE: i32 = 40;
			const MAX_SIZE: i32 = 800;

			width = match glsp::global::<_, Val>("play:width") {
				Ok(Val::Int(width)) if width >= MIN_SIZE && width <= MAX_SIZE => {
					width as u32
				}
				Ok(val) => {
					let msg = format!("invalid play:width value {}\n(should be an integer \
					                   between {} and {})", val, MIN_SIZE, MAX_SIZE);
					return Ok(Err(JsValue::from_str(&msg)))
				}
				Err(_) => {
					return Ok(Err(JsValue::from_str("play:width is not defined")))
				}
			};

			height = match glsp::global::<_, Val>("play:height") {
				Ok(Val::Int(height)) if height >= MIN_SIZE && height <= MAX_SIZE => {
					height as u32
				}
				Ok(val) => {
					let msg = format!("invalid play:height value {}\n(should be an integer \
					                   between {} and {})", val, MIN_SIZE, MAX_SIZE);
					return Ok(Err(JsValue::from_str(&msg)))
				}
				Err(_) => {
					return Ok(Err(JsValue::from_str("play:height is not defined")))
				}
			};

			title = match glsp::global::<_, Val>("play:title") {
				Ok(Val::Str(title))=> title.to_string(),
				Ok(val) => {
					let msg = format!("invalid play:title value {}\n(should be a string)", val);
					return Ok(Err(JsValue::from_str(&msg)))
				}
				Err(_) => {
					return Ok(Err(JsValue::from_str("play:title is not defined")))
				}
			};

			blurb = match glsp::global::<_, Val>("play:blurb") {
				Ok(Val::Str(blurb))=> blurb.to_string(),
				Ok(val) => {
					let msg = format!("invalid play:blurb value {}\n(should be a string)", val);
					return Ok(Err(JsValue::from_str(&msg)))
				}
				Err(_) => {
					return Ok(Err(JsValue::from_str("play:blurb is not defined")))
				}
			};

			Ok(Ok(()))
		}).unwrap();

		match result {
			Ok(()) => (),
			Err(err) => return Err(err)
		}

		*option = Some(Box::new(Engine {
			runtime,
			width,
			height,
			title,
			blurb
		}));

		Ok(())
	})
}

#[wasm_bindgen]
pub fn width() -> Result<u32, JsValue> {
	with_engine(|engine| {
		Ok(engine.width)
	})
}

#[wasm_bindgen]
pub fn height() -> Result<u32, JsValue> {
	with_engine(|engine| {
		Ok(engine.height)
	})
}

#[wasm_bindgen]
pub fn title() -> Result<String, JsValue> {
	with_engine(|engine| {
		Ok(engine.title.clone())
	})
}

#[wasm_bindgen]
pub fn blurb() -> Result<String, JsValue> {
	with_engine(|engine| {
		Ok(engine.blurb.clone())
	})
}

#[wasm_bindgen]
pub fn update(dt: f64) -> Result<(), JsValue> {
	with_engine(|engine| {
		engine.runtime.run(|| {
			let update: Root<GFn> = match glsp::global::<_, Val>("play:update") {
				Ok(Val::GFn(update)) => update,
				Ok(val) => {
					let msg = format!("invalid play:update value {}", val);
					return Ok(Err(JsValue::from_str(&msg)))
				}
				Err(_) => return Ok(Err(JsValue::from_str("play:update is not defined")))
			};

			let _: Val = match glsp::call(&update, (dt,)) {
				Ok(val) => val,
				Err(glsp_err) => {
					return Ok(Err(JsValue::from_str(&glsp_err.to_string())))
				}
			};

			glsp::gc();

			Ok(Ok(()))
		}).unwrap()
	})
}
