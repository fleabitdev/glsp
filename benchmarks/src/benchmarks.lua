function primitive_inc()
    local x = 0
    for i = 0, 1000 * 1000 do
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
        x = x + 1; x = x + 1; x = x + 1; x = x + 1; x = x + 1;
    end
end

function primitive_arith()
    local x = 0
    for i = 0, 1000 * 1000 do
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
        x = x + 500; x = x - 250; x = x * 1.5; x = x / 42.0; x = x % 18.0; x = -x;
    end
end

function primitive_call0()
    local function a()
        return nil
    end

    for i = 0, 100 * 1000 do
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
    end
end

function primitive_call3()
    local function a(b, c, d)
        return nil
    end

    for i = 0, 100 * 1000 do
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
    end
end

function primitive_array()
    local a = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

    -- lua doesn't allow us to just write a[0];, so we assign each result to a local variable
    local x
    for i = 0, 100 * 1000 do
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
    end
end

function primitive_table()
    local t = {
        a = 0, b = 1, c = 2, d = 3, e = 4,
        f = 5, g = 6, h = 7, i = 8, j = 9
    }

    -- lua doesn't allow us to just write t.a;, so we assign each result to a local variable
    local x
    for i = 0, 100 * 1000 do
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
        x = t.a; x = t.b; x = t.c; x = t.d; x = t.e;
        x = t.f; x = t.g; x = t.h; x = t.i; x = t.j;
    end
end

function primitive_field()
    primitive_table()
end

function primitive_method()
    -- in Lua, methods would typically be stored on a prototype table
    local class = {
        a = function() return nil end
    }

    local o = {}
    setmetatable(o, {__index = class})

    for i = 0, 100 * 1000 do
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
    end
end

function rects()
    local function intersects(a, b)
        return (a.x + a.w > b.x) and 
               (a.x < b.x + b.w) and 
               (a.y + a.h > b.y) and
               (a.y < b.y + b.h)
    end

    local rng = 75
    local function rng_gen()
        rng = (rng * 75) % 65537
        return rng
    end

    local rects = {}
    for i = 1, 2000 do
        rects[i] = { x = rng_gen(), y = rng_gen(), w = rng_gen(), h = rng_gen() }
    end

    local count = 0
    for i = 1, #rects do
        for j = i + 1, #rects do
            if intersects(rects[i], rects[j]) then
                count = count + 1
            end
        end
    end
end

function flood_fill()
    local width = 1000 * 1000;
    local pixels = {}
    for i = 1, width do
        pixels[i] = 0
    end
    local to_check = {#pixels / 2}

    local painted = 0
    while #to_check > 0 do
        local x = table.remove(to_check)

        pixels[x] = 1
        painted = painted + 1

        if x < width and pixels[x + 1] == 0 then
            table.insert(to_check, x + 1)
        end

        if x > 1 and pixels[x - 1] == 0 then
            table.insert(to_check, x - 1)
        end
    end

    assert(painted == width)
end

function rotation()
    local rng = 75
    local function rng_gen()
        rng = (rng * 75) % 65537
        return rng
    end

    local function rotate(p, diff)
        local distance = math.sqrt((p.x * p.x) + (p.y * p.y))
        local angle = math.atan(p.y, p.x)
        local new_angle = angle + diff
        p.x = distance * math.cos(new_angle)
        p.y = distance * math.sin(new_angle)
    end

    local points = {}
    for i = 1, 300 do
        points[i] = { x = rng_gen(), y = rng_gen() }
    end

    local step = (0.1 * 3.14159) / 180.0
    for _ = 1, 3600 do
        for _, point in ipairs(points) do
            rotate(point, step)
        end
    end
end

function bench(names)
    for _, name in ipairs(names) do
        local start = precise_time()
        _G[name]()
        local elapsed = precise_time() - start
        print(string.format("Lua %s: %.1fms", name, elapsed * 1000))
    end
end

bench({"primitive_inc", "primitive_arith", "primitive_call0", "primitive_call3",
       "primitive_array", "primitive_table", "primitive_field", "primitive_method",
       "rects", "flood_fill", "rotation"})
