use glsp::{bail, ensure, GResult, Num, Rest, Val};
use smallvec::SmallVec;
use std::cmp::Ordering;
use std::f32;

pub fn init(_sandboxed: bool) -> GResult<()> {
    glsp::bind_rfn("+", &add)?;
    glsp::bind_rfn("-", &sub)?;
    glsp::bind_rfn("*", &mul)?;
    glsp::bind_rfn("/", &div)?;
    glsp::bind_rfn("%", &rem)?;
    glsp::bind_rfn("div-euclid", &div_euclid)?;
    glsp::bind_rfn("rem-euclid", &rem_euclid)?;
    glsp::bind_rfn("abs", &abs)?;
    glsp::bind_rfn("sign", &sign)?;

    glsp::bind_rfn("even?", &evenp)?;
    glsp::bind_rfn("odd?", &oddp)?;
    glsp::bind_rfn("nat-int?", &nat_intp)?;
    glsp::bind_rfn("pos-int?", &pos_intp)?;
    glsp::bind_rfn("neg-int?", &neg_intp)?;
    glsp::bind_rfn("nan?", &nanp)?;
    glsp::bind_rfn("inf?", &infp)?;

    glsp::bind_rfn("==", &num_eq)?;
    glsp::bind_rfn("<", &lt)?;
    glsp::bind_rfn("<=", &lte)?;
    glsp::bind_rfn(">", &gt)?;
    glsp::bind_rfn(">=", &gte)?;
    glsp::bind_rfn("ord", &ord)?;
    glsp::bind_rfn("min", &min)?;
    glsp::bind_rfn("max", &max)?;
    glsp::bind_rfn("clamp", &clamp)?;

    glsp::bind_rfn("round", &round)?;
    glsp::bind_rfn("floor", &floor)?;
    glsp::bind_rfn("ceil", &ceil)?;
    glsp::bind_rfn("sqrt", &sqrt)?;
    glsp::bind_rfn("cbrt", &cbrt)?;
    glsp::bind_rfn("pow", &pow)?;
    glsp::bind_rfn("log", &log)?;
    glsp::bind_rfn("flo-sign", &flo_sign)?;
    glsp::bind_rfn("trunc", &trunc)?;
    glsp::bind_rfn("fract", &fract)?;
    glsp::bind_rfn("sin", &sin)?;
    glsp::bind_rfn("cos", &cos)?;
    glsp::bind_rfn("tan", &tan)?;
    glsp::bind_rfn("asin", &asin)?;
    glsp::bind_rfn("acos", &acos)?;
    glsp::bind_rfn("atan", &atan)?;

    glsp::bind_rfn("bitand", &bitand)?;
    glsp::bind_rfn("bitor", &bitor)?;
    glsp::bind_rfn("bitxor", &bitxor)?;
    glsp::bind_rfn("bitnot", &bitnot)?;
    glsp::bind_rfn("bitshl", &bitshl)?;
    glsp::bind_rfn("bitshr", &bitshr)?;
    glsp::bind_rfn("bitsar", &bitsar)?;

    glsp::bind_rfn("rand", &rand)?;
    glsp::bind_rfn("chance?", &chancep)?;
    glsp::bind_rfn("rand-pick", &rand_pick)?;
    glsp::bind_rfn("rand-weighted", &rand_weighted)?;
    glsp::bind_rfn("rand-reseed", &rand_reseed)?;

    glsp::bind_rfn("smoothstep", &smoothstep)?;
    glsp::bind_rfn("seek", &seek)?;
    glsp::bind_rfn("antiseek", &antiseek)?;

    Ok(())
}

fn add(args: Rest<Num>) -> Num {
    args.iter().fold(Num::Int(0), |accum, &arg| accum + arg)
}

fn sub(first: Num, rest: Rest<Num>) -> Num {
    if rest.len() == 0 {
        -first
    } else {
        rest.iter().fold(first, |accum, &arg| accum - arg)
    }
}

//it would give us a more intuitively correct answer if we were to set result's initial value to a
//Flo if there are any Flo arguments: for example, that would stop (* #xf0000 #xf0000 3.0) from
//overflowing when multiplying the first two arguments. however, this would be inconsistent
//with the arithmetic operators' behaviour in vm.rs and transform.rs.
fn mul(args: Rest<Num>) -> Num {
    args.iter().fold(Num::Int(1), |accum, &arg| accum * arg)
}

fn div(first: Num, rest: Rest<Num>) -> GResult<Num> {
    if rest.len() == 0 {
        ensure!(first != Num::Int(0), "divide-by-zero error");
        Ok(Num::Flo(1.0) / first)
    } else {
        rest.iter().try_fold(first, |accum, &arg| {
            ensure!(
                !(accum.is_int() && arg == Num::Int(0)),
                "divide-by-zero error"
            );
            Ok(accum / arg)
        })
    }
}

fn div_euclid(first: Num, rest: Rest<Num>) -> GResult<Num> {
    if rest.len() == 0 {
        ensure!(first != Num::Int(0), "divide-by-zero error");
        Ok(Num::Flo(1.0).div_euclid(first))
    } else {
        rest.iter().try_fold(first, |accum, &arg| {
            ensure!(
                !(accum.is_int() && arg == Num::Int(0)),
                "divide-by-zero error"
            );
            Ok(accum.wrapping_div_euclid(arg))
        })
    }
}

fn rem(numer: Num, denom: Num) -> GResult<Num> {
    ensure!(denom != Num::Int(0), "divide-by-zero error");
    Ok(numer % denom)
}

fn rem_euclid(numer: Num, denom: Num) -> GResult<Num> {
    ensure!(denom != Num::Int(0), "divide-by-zero error");
    Ok(numer.rem_euclid(denom))
}

fn abs(num: Num) -> Num {
    num.abs()
}

fn sign(num: Num) -> i32 {
    match num {
        Num::Int(i) => i.signum(),
        Num::Flo(f) => {
            if f == 0.0f32 || f.is_nan() {
                0
            } else {
                f.signum() as i32
            }
        }
    }
}

fn min(first: Val, rest: Rest<Val>) -> GResult<Val> {
    ensure!(
        rest.iter()
            .all(|val| val.is_int() || val.is_flo() || val.is_char()),
        "non-number passed to min"
    );
    Ok(rest.iter().fold(first, |accum, arg| {
        if arg.partial_cmp(&accum).unwrap() == Ordering::Less {
            arg.clone()
        } else {
            accum
        }
    }))
}

fn max(first: Val, rest: Rest<Val>) -> GResult<Val> {
    ensure!(
        rest.iter()
            .all(|val| val.is_int() || val.is_flo() || val.is_char()),
        "non-number passed to max"
    );
    Ok(rest.iter().fold(first, |accum, arg| {
        if arg.partial_cmp(&accum).unwrap() == Ordering::Greater {
            arg.clone()
        } else {
            accum
        }
    }))
}

fn clamp(n: Val, min: Val, max: Val) -> GResult<Val> {
    for val in &[&n, &min, &max] {
        ensure!(
            val.is_int() || val.is_flo() || val.is_char(),
            "non-number passed to clamp"
        );
    }

    ensure!(
        min.partial_cmp(&max).unwrap() != Ordering::Greater,
        "min value passed to (clamp) is larger than its max value"
    );

    if n.partial_cmp(&min).unwrap() != Ordering::Greater {
        Ok(min)
    } else if n.partial_cmp(&max).unwrap() != Ordering::Less {
        Ok(max)
    } else {
        Ok(n)
    }
}

fn evenp(i: i32) -> bool {
    i % 2 == 0
}

fn oddp(i: i32) -> bool {
    i % 2 != 0
}

fn nat_intp(val: Val) -> bool {
    matches!(val, Val::Int(i) if i >= 0)
}

fn pos_intp(val: Val) -> bool {
    matches!(val, Val::Int(i) if i > 0)
}

fn neg_intp(val: Val) -> bool {
    matches!(val, Val::Int(i) if i < 0)
}

//we arbitrarily divide our mathematical functions into "non-converting" ones like (pow) and (/),
//which perform wrapping/rounding arithmetic when the input is an int; and "converting" ones like
//(sqrt), which always return a flo even for an int argument.

//trying to be pragmatic here - don't want to promote ints to flos unnecessarily, but also don't
//want unexpected wrapping arithmetic. in practice, we make anything with a `wrapping_x` method
//in Rust non-converting, and everything else converting.

//this problem would solve itself if we were to get rid of ints and flos, and just have a single
//f64 number type (48 bits of exact int precision!)... but that might cause performance issues,
//be inconvenient in the FFI, or interfere with our plans for 64-bit packed Slots. more thought
//required.

//i'm reluctant to switch to checked int arithmetic, because it would promote some bugs from
//"create a wacky numeric value" to "crash the entire game".

//really not keen on bignums, for performance reasons.

fn round(num: Num) -> Num {
    match num {
        Num::Int(i) => Num::Int(i),
        Num::Flo(f) => Num::Flo(f.round()),
    }
}

fn floor(num: Num) -> Num {
    match num {
        Num::Int(i) => Num::Int(i),
        Num::Flo(f) => Num::Flo(f.floor()),
    }
}

fn ceil(num: Num) -> Num {
    match num {
        Num::Int(i) => Num::Int(i),
        Num::Flo(f) => Num::Flo(f.ceil()),
    }
}

fn sqrt(num: Num) -> Num {
    match num {
        Num::Int(i) => Num::Flo((i as f32).sqrt()),
        Num::Flo(f) => Num::Flo(f.sqrt()),
    }
}

fn cbrt(num: Num) -> Num {
    match num {
        Num::Int(i) => Num::Flo((i as f32).cbrt()),
        Num::Flo(f) => Num::Flo(f.cbrt()),
    }
}

fn pow(base: Num, exponent: Num) -> Num {
    match (base, exponent) {
        (Num::Int(base), Num::Int(exp)) if exp >= 0 => Num::Int(base.wrapping_pow(exp as u32)),
        _ => Num::Flo(base.into_f32().powf(exponent.into_f32())),
    }
}

fn log(x: Num, base: Option<Num>) -> f32 {
    if let Some(base) = base {
        if base.into_f32() == 2.0 {
            x.into_f32().log2()
        } else if base.into_f32() == 10.0 {
            x.into_f32().log10()
        } else {
            x.into_f32().log(base.into_f32())
        }
    } else {
        x.into_f32().ln()
    }
}

fn flo_sign(f: f32) -> f32 {
    f.signum()
}

fn trunc(f: f32) -> f32 {
    f.trunc()
}

fn fract(f: f32) -> f32 {
    f.fract()
}

fn sin(f: f32) -> f32 {
    f.sin()
}

fn asin(f: f32) -> f32 {
    f.asin()
}

fn cos(f: f32) -> f32 {
    f.cos()
}

fn acos(f: f32) -> f32 {
    f.acos()
}

fn tan(f: f32) -> f32 {
    f.tan()
}

fn atan(y: f32, x: Option<f32>) -> f32 {
    if let Some(x) = x {
        y.atan2(x)
    } else {
        y.atan()
    }
}

fn nanp(f: f32) -> bool {
    f.is_nan()
}

fn infp(f: f32) -> bool {
    f.is_infinite()
}

fn bitand(args: Rest<i32>) -> i32 {
    args.iter().fold(-1, |accum, &arg| accum & arg)
}

fn bitor(args: Rest<i32>) -> i32 {
    args.iter().fold(0, |accum, &arg| accum | arg)
}

fn bitxor(args: Rest<i32>) -> i32 {
    args.iter().fold(0, |accum, &arg| accum ^ arg)
}

fn bitnot(arg: i32) -> i32 {
    !arg
}

//just like the integer arithmetic functions, the bitshifting functions are designed to behave in
//an identical way to rust-with-overflow-checks-disabled.
fn bitshl(arg: i32, shift: u32) -> i32 {
    arg.overflowing_shl(shift).0
}

fn bitshr(arg: i32, shift: u32) -> i32 {
    //as far as i can tell, rust doesn't currently perform any bounds checks for `my_i32 as u32`
    (arg as u32).overflowing_shr(shift).0 as i32
}

fn bitsar(arg: i32, shift: u32) -> i32 {
    arg.overflowing_shr(shift).0
}

fn num_eq(args: Rest<Val>) -> GResult<bool> {
    ensure!(
        args.len() >= 2,
        "expected at least 2 args, but received {}",
        args.len()
    );
    ensure!(
        args.iter()
            .all(|val| val.is_int() || val.is_flo() || val.is_char()),
        "non-number passed to =="
    );
    Ok((0..args.len() - 1).all(|i| args[i].partial_cmp(&args[i + 1]).unwrap() == Ordering::Equal))
}

fn lt(args: Rest<Val>) -> GResult<bool> {
    ensure!(
        args.len() >= 2,
        "expected at least 2 args, but received {}",
        args.len()
    );
    ensure!(
        args.iter()
            .all(|val| val.is_int() || val.is_flo() || val.is_char()),
        "non-number passed to <"
    );
    Ok((0..args.len() - 1).all(|i| args[i].partial_cmp(&args[i + 1]).unwrap() == Ordering::Less))
}

fn lte(args: Rest<Val>) -> GResult<bool> {
    ensure!(
        args.len() >= 2,
        "expected at least 2 args, but received {}",
        args.len()
    );
    ensure!(
        args.iter()
            .all(|val| val.is_int() || val.is_flo() || val.is_char()),
        "non-number passed to <="
    );
    Ok(
        (0..args.len() - 1)
            .all(|i| args[i].partial_cmp(&args[i + 1]).unwrap() != Ordering::Greater),
    )
}

fn gt(args: Rest<Val>) -> GResult<bool> {
    ensure!(
        args.len() >= 2,
        "expected at least 2 args, but received {}",
        args.len()
    );
    ensure!(
        args.iter()
            .all(|val| val.is_int() || val.is_flo() || val.is_char()),
        "non-number passed to >"
    );
    Ok(
        (0..args.len() - 1)
            .all(|i| args[i].partial_cmp(&args[i + 1]).unwrap() == Ordering::Greater),
    )
}

fn gte(args: Rest<Val>) -> GResult<bool> {
    ensure!(
        args.len() >= 2,
        "expected at least 2 args, but received {}",
        args.len()
    );
    ensure!(
        args.iter()
            .all(|val| val.is_int() || val.is_flo() || val.is_char()),
        "non-number passed to >="
    );
    Ok((0..args.len() - 1).all(|i| args[i].partial_cmp(&args[i + 1]).unwrap() != Ordering::Less))
}

fn ord(arg0: Val, arg1: Val) -> GResult<Ordering> {
    match arg0.partial_cmp(&arg1) {
        Some(ordering) => Ok(ordering),
        None => bail!(
            "invalid arguments passed to ord: {} and {}",
            arg0.a_type_name(),
            arg1.a_type_name()
        ),
    }
}

fn rand(arg0: Num, arg1: Option<Num>) -> Num {
    let (limit0, limit1) = match arg1 {
        Some(arg1) => (arg0, arg1),
        None => (Num::Int(0), arg0),
    };

    let (f0, f1) = match (limit0, limit1) {
        (Num::Int(i0), Num::Int(i1)) => {
            if i0 < i1 {
                return Num::Int(i0 + super::rand_i32().abs() % (i1 - i0));
            } else if i0 > i1 {
                return Num::Int(i0 - super::rand_i32().abs() % (i0 - i1));
            } else {
                assert!(i0 == i1);
                return Num::Int(i0);
            }
        }
        (Num::Int(i0), Num::Flo(f1)) => (i0 as f32, f1),
        (Num::Flo(f0), Num::Int(i1)) => (f0, i1 as f32),
        (Num::Flo(f0), Num::Flo(f1)) => (f0, f1),
    };

    if f0 == f1 {
        Num::Flo(f0)
    } else {
        Num::Flo(f0 + super::rand_f32() * (f1 - f0))
    }
}

fn chancep(chance: f32) -> bool {
    if chance <= 0.0 {
        false
    } else if chance >= 1.0 {
        true
    } else {
        super::rand_f32() < chance
    }
}

fn rand_pick(args: Rest<Val>) -> GResult<Val> {
    ensure!(args.len() > 0, "expected at least one argument");

    let i = (super::rand_i32().abs() as usize) % args.len();
    Ok(args[i].clone())
}

fn rand_weighted(args: Rest<Val>) -> GResult<Val> {
    ensure!(args.len() > 0, "expected at least one argument");
    ensure!(args.len() % 2 == 0, "expected an even number of arguments");

    let mut choices = SmallVec::<[Val; 32]>::with_capacity(args.len() / 2);
    let mut weights = SmallVec::<[f32; 32]>::with_capacity(args.len() / 2);
    let mut total_weight = 0.0_f32;
    for pair in args.chunks_exact(2) {
        let weight = match pair[1] {
            Val::Int(i) if i >= 0 => i as f32,
            Val::Flo(f) if f >= 0.0 => f,
            _ => bail!("each weight must be a non-negative int or a non-negative flo"),
        };

        choices.push(pair[0].clone());
        weights.push(weight);
        total_weight += weight;
    }

    ensure!(total_weight != 0.0, "the total weight must not be 0");

    let selection = super::rand_f32() * total_weight;

    let mut accum = 0.0;
    for i in 0..weights.len() {
        if accum >= selection && weights[i] > 0.0 {
            return Ok(choices[i].clone());
        }
        accum += weights[i];
    }

    //this should be unreachable, but just in case...
    Ok(choices.last().unwrap().clone())
}

fn rand_reseed(seed: i32) {
    super::rand_reseed(seed)
}

fn smoothstep(f: f32) -> f32 {
    if f <= 0.0 {
        0.0
    } else if f >= 1.0 {
        1.0
    } else {
        3.0 * f * f - 2.0 * f * f * f
    }
}

fn seek(orig: Num, target: Num, step_by: Option<Num>) -> GResult<Num> {
    let step_by = step_by.unwrap_or(Num::Int(1));
    ensure!(step_by >= Num::Int(0), "negative step {}", step_by);

    if orig == target {
        Ok(target)
    } else if orig < target {
        let new = orig + step_by;
        Ok(if new >= target { target } else { new })
    } else {
        assert!(orig > target);
        let new = orig - step_by;
        Ok(if new <= target { target } else { new })
    }
}

fn antiseek(orig: Num, target: Num, step_by: Option<Num>) -> GResult<Num> {
    let step_by = step_by.unwrap_or(Num::Int(1));
    ensure!(step_by >= Num::Int(0), "negative step {}", step_by);

    if orig >= target {
        Ok(orig + step_by)
    } else {
        Ok(orig - step_by)
    }
}
