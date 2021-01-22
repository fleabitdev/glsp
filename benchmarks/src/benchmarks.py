import math, sys, time

def primitive_inc():
    x = 0
    for i in range(0, 1_000_000):
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
        x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; x += 1; 
    return x

def primitive_arith():
    x = 0
    for i in range(0, 1_000_000):
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
        x += 500; x -= 250; x *= 1.5; x /= 42.0; x %= 18.0; x = -x;
    return x

def primitive_call0():
    def a():
        return None

    for i in range(0, 100_000):
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a(); 
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a(); 
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a(); 
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a(); 
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a(); 
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a(); 
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a(); 
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a(); 
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a(); 
        a(); a(); a(); a(); a(); a(); a(); a(); a(); a();

def primitive_call3():
    def a(b, c, d):
        return None

    for i in range(0, 100_000):
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);
        a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3); a(1, 2, 3);

def primitive_array():
    a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    x = None;
    for i in range(0, 100_000):
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];
        x = a[0]; x = a[1]; x = a[2]; x = a[3]; x =  a[4];
        x = a[5]; x = a[6]; x = a[7]; x = a[8]; x = a[9];

def primitive_table():
    t = {'a': 0, 'b': 1, 'c': 2, 'd': 3, 'e': 4, 'f': 5, 'g': 6, 'h': 7, 'i': 8, 'j': 9};

    x = None
    for i in range(0, 100_000):
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];
        x = t['a']; x = t['b']; x = t['c']; x = t['d']; x = t['e'];
        x = t['f']; x = t['g']; x = t['h']; x = t['i']; x = t['j'];

def primitive_field():
    class C:
        def __init__(self):
            self.a = None; self.b = None; self.c = None; self.d = None; self.e = None;
            self.f = None; self.g = None; self.h = None; self.i = None; self.j = None;

    o = C();

    x = None
    for i in range(0, 100_000):
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;
        x = o.a; x = o.b; x = o.c; x = o.d; x = o.e; x = o.f; x = o.g; x = o.h; x = o.i; x = o.j;

def primitive_method():
    class C:
        def a(self):
            return None

    o = C();

    for i in range(0, 100_000):
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 
        o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); o.a(); 

def rects():
    class Rect:
        def __init__(self, x, y, w, h):
            self.x = x
            self.y = y
            self.w = w
            self.h = h

        def intersects(self, other):
            return (self.x + self.w > other.x) and \
                   (self.x < other.x + other.w) and \
                   (self.y + self.h > other.y) and \
                   (self.y < other.y + other.h)

    global rng
    rng = 75
    def rng_gen():
        global rng
        rng = (rng * 75) % 65537
        return rng

    rects = []
    for i in range(0, 2000):
        rects.append(Rect(rng_gen(), rng_gen(), rng_gen(), rng_gen()))

    count = 0
    for i in range(0, len(rects)):
        for j in range(i + 1, len(rects)):
            if rects[i].intersects(rects[j]):
                count += 1

def flood_fill():
    width = 1_000_000
    pixels = [0] * width
    to_check = [len(pixels) // 2]

    painted = 0
    while len(to_check) > 0:
        x = to_check.pop()

        pixels[x] = 1
        painted += 1

        if x < width - 1 and pixels[x + 1] == 0:
            to_check.append(x + 1)

        if x > 0 and pixels[x - 1] == 0:
            to_check.append(x - 1)

    assert(painted == width)

def rotation():
    global rng
    rng = 75
    def rng_gen():
        global rng
        rng = (rng * 75) % 65537
        return rng

    class Point:
        def __init__(self, x, y):
            self.x = x
            self.y = y

        def rotate(self, diff):
            distance = math.sqrt((self.x * self.x) + (self.y * self.y))
            angle = math.atan2(self.y, self.x)
            new_angle = angle + diff
            self.x = distance * math.cos(new_angle)
            self.y = distance * math.sin(new_angle)

    points = []
    for _ in range(0, 300):
        points.append(Point(float(rng_gen()), float(rng_gen())))

    step = (0.1 * 3.14159) / 180.0
    for _ in range(0, 3600):
        for point in points:
            point.rotate(step)

def bench(names):
    for name in names:
        start = time.time_ns()
        globals()[name]()
        elapsed = time.time_ns() - start
        elapsed_str = "{:.1f}".format(elapsed / 1_000_000)
        print(f"Python {name}: {elapsed_str}ms")
        sys.stdout.flush()

bench(["primitive_inc", "primitive_arith", "primitive_call0", "primitive_call3",
       "primitive_array", "primitive_table", "primitive_field", "primitive_method",
       "rects", "flood_fill", "rotation"])
