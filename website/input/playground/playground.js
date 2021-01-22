/*
the static site generator prefixes this file with `let demoSources = { ... }`, where each
value is the full contents of a .glsp source file as a string, and each key is its full filename,
e.g. "minefinder.glsp"
*/

//-------------------------------------------------------------------------------------------------
// elements
//-------------------------------------------------------------------------------------------------

let playStopButtons = document.getElementById("playground-button");
let playButton = document.getElementById("playground-play");
let stopButton = document.getElementById("playground-stop");
let screen = document.getElementById("playground-screen");
let loading = document.getElementById("playground-loading");
let errorMsg = document.getElementById("playground-error-msg");
let errorTitle = document.getElementById("playground-error-title");
let errorText = document.getElementById("playground-error-text");
let intro = document.getElementById("playground-intro");
let canvas = document.querySelector("#playground-screen canvas");
let textarea = document.getElementById("glsp-code-textarea");
let select = document.querySelector("#playground-dropdown select");

let ctx = canvas.getContext("2d", {
    alpha: false,
    desynchronized: true
});


//-------------------------------------------------------------------------------------------------
// the code editor
//-------------------------------------------------------------------------------------------------

/*
don't want to waste too much effort on this, so we currently do the bare minimum to make the
textarea usable for *basic* code editing: disable tab-to-change-focus, enable tab input
(including indenting/unindenting a multi-line selection), and enable basic autoindentation 
(when the return key is pressed, we insert the same whitespace as the previous line).
*/

textarea.addEventListener("keydown", ev => {

    //intercept Tab and shift+Tab keypresses
    if (ev.key == "Tab" && ev.ctrlKey == false && ev.altKey == false) {

        //takes a line of text and returns that same line, reduced by one indentation level
        let unindentLine = (lineStr) => {
            let trimmed = lineStr.trimStart();
            let whitespace = lineStr.slice(0, lineStr.length - trimmed.length);

            if (whitespace.startsWith('\t')) {
                return whitespace.slice(1) + trimmed;
            }

            if (whitespace.startsWith(' ')) {
                if (whitespace.startsWith('  ')) {
                    return whitespace.slice(2) + trimmed;
                } else {
                    return whitespace.slice(1) + trimmed;
                }
            }

            return whitespace + trimmed;
        };

        let selection = textarea.value.slice(textarea.selectionStart, textarea.selectionEnd);
        if (selection.includes('\n')) {

            //the selection spans multiple lines. unindent each line
            while (true) {
                let ch = textarea.value.charAt(textarea.selectionStart - 1);
                if (ch && ch != '\n') {
                    textarea.selectionStart -= 1;
                } else {
                    break;
                }
            }

            while (true) {
                let ch = textarea.value.charAt(textarea.selectionEnd);
                if (ch && ch != '\n') {
                    textarea.selectionEnd += 1;
                } else {
                    break;
                }
            }

            let selection = textarea.value.slice(textarea.selectionStart, textarea.selectionEnd);
            let inLines = selection.split('\n');
            let outLines = [];
            for (inLine of inLines) {
                if (ev.shiftKey) {
                    outLines.push(unindentLine(inLine));
                } else {
                    outLines.push('\t' + inLine);
                }
            }

            let output = outLines.join('\n');
            textarea.setRangeText(output);

            textareaChanged();
            ev.preventDefault();
            return;
        } else {

            //the selection does not span multiple lines. 
            if (ev.shiftKey) {

                //shift+tab. iff there is '\t' or ' ' just before the selection, delete it.
                if (textarea.value.charAt(textarea.selectionStart - 1) == '\t') {
                    textarea.selectionStart -= 1;
                    textarea.selectionEnd = textarea.selectionStart + 1;
                    textarea.setRangeText("");
                } else if (textarea.value.charAt(textarea.selectionStart - 1) == ' ' &&
                           textarea.value.charAt(textarea.selectionStart - 2) == ' ') {
                    textarea.selectionStart -= 2;
                    textarea.selectionEnd = textarea.selectionStart + 2;
                    textarea.setRangeText("");
                }

                textareaChanged();
                ev.preventDefault();
            } else {

                //just type a tab character
                textarea.setRangeText("\t", textarea.selectionStart, textarea.selectionEnd, "end");
                
                textareaChanged();
                ev.preventDefault();
            }
        }
    }

    //intercept Enter keypresses
    if (ev.key == "Enter" && ev.ctrlKey == false && ev.altKey == false) {
        //html5 spec says that linebreaks are normalized to '\n', so we don't need to handle '\r\n'
        textarea.setRangeText(
            '\n',
            textarea.selectionStart,
            textarea.selectionEnd,
            "end"
        );

        //if '\n' is not found, this sets lineStart to 0
        let lineStart = textarea.value.lastIndexOf('\n', textarea.selectionStart - 2) + 1;

        let textStart = lineStart;
        while (true) {
            let ch = textarea.value.charAt(textStart);
            if (ch == undefined || !(ch == ' ' || ch == '\t')) {
                break;
            }

            textStart += 1;
        }

        textarea.setRangeText(
            textarea.value.slice(lineStart, textStart),
            textarea.selectionStart,
            textarea.selectionEnd,
            "end"
        );

        textareaChanged();
        ev.preventDefault();
    }
});


//-------------------------------------------------------------------------------------------------
// demo source code
//-------------------------------------------------------------------------------------------------

//surprisingly straightforward. we just store the current source in demoSources and keep it in 
//sync with the textarea.
function textareaChanged() {
    demoSources[select.selectedOptions[0].textContent] = textarea.value;
}

textarea.addEventListener("input", textareaChanged);

function selectChanged() {
    textarea.value = demoSources[select.selectedOptions[0].textContent];
    textarea.scrollTop = 0;
}

select.addEventListener("input", selectChanged);

//handling the url #fragment
function processHash() {
    if (window.location.hash) {
        let fragment = window.location.hash.slice(1);

        for (option of select.options) {
            if (fragment == option.value || fragment == option.label) {
                select.selectedIndex = option.index;
                selectChanged();
            }
        }
    }
}

processHash();

window.addEventListener("hashchange", processHash);


//-------------------------------------------------------------------------------------------------
// states
//-------------------------------------------------------------------------------------------------

/*
this toplevel state machine only really controls the visibility of html elements. there's quite a 
lot of implicit state related to loading and rendering - see below.

possible states are "editing". "preLoading", "loading", "errorMsg", "intro" or "rendering". 
clicking the "stop" button unconditionally changes the state back to "editing", which cancels 
any ongoing pre-loading/loading/rendering.

"preLoading" is a brief pause, during which time the text editor is still displayed, to avoid
a distracting flicker of "Loading..." text on fast connections.
*/

let state = "editing";

function showElementsForState(newState) {
    //hide/disable everything
    playButton.style.display = "none";
    textarea.style.display = "none";
    screen.style.display = "none";
    loading.style.display = "none";
    errorMsg.style.display = "none";
    intro.style.display = "none";
    canvas.style.display = "none";
    select.disabled = true;

    //selectively re-show/re-enable only what the new state needs
    switch (newState) {
        case "editing":
            playButton.style.display = "block";
            textarea.style.display = "block";
            select.disabled = false;
            break;
        
        case "preLoading":
            textarea.style.display = "block";
            break;

        case "loading":
            screen.style.display = "flex";
            loading.style.display = "block";
            break;

        case "errorMsg":
            screen.style.display = "flex";
            errorMsg.style.display = "flex";
            break;

        case "intro":
            screen.style.display = "flex";
            intro.style.display = "flex";
            break;

        case "rendering":
            screen.style.display = "flex";
            canvas.style.display = "block";
            break;

        default:
            throw new Error("unrecognized state " + newState)
    }

    //update the `state` global
    state = newState
}

playStopButtons.addEventListener("click", (ev) => {
    switch (state) {
        case "editing":
            showElementsForState("preLoading");
            startPreLoading();
            break;

        //cancel ongoing operations from the current state...
        case "preLoading":
            showElementsForState("editing");
            stopPreLoading();
            break;

        case "loading":
            showElementsForState("editing");
            stopLoading();
            break;

        case "errorMsg":
            showElementsForState("editing");
            break;

        case "intro":
            showElementsForState("editing");
            break;

        case "rendering":
            showElementsForState("editing");
            stopRendering();
            break;

        default:
            showElementsForState("editing");
            break;
    }
});

screen.addEventListener("click", (ev) => {
    if (state == "intro") {
        showElementsForState("rendering");
        startRendering();
    }
});

screen.addEventListener("contextmenu", (ev) => {
    ev.preventDefault();
});

document.addEventListener("keydown", (ev) => {
    if (state == "intro") {
        showElementsForState("rendering");
        startRendering();
    }
});


//-------------------------------------------------------------------------------------------------
// the "preLoading" state
//-------------------------------------------------------------------------------------------------

//think of this state as a child of the "loading" state. it calls startLoading when it's
//enabled and stopLoading when it's interrupted.

let preLoadingTimeout = null;

function startPreLoading() {
    startLoading();

    if (!loaded) {
        if (preLoadingTimeout !== null) {
            throw new Error("preLoading state enabled twice simultaneously")
        }

        preLoadingTimeout = window.setTimeout(preLoad, 150);
    }
}

function isPreLoading() {
    return (preLoadingTimeout !== null);
}

function preLoad() {
    preLoadingTimeout = null;
    showElementsForState("loading");
}

function stopPreLoading() {
    window.clearTimeout(preLoadingTimeout);
    preLoadingTimeout = null;

    stopLoading();
}


//-------------------------------------------------------------------------------------------------
// the "loading" state
//-------------------------------------------------------------------------------------------------

//the actual data is only loaded once. if `loaded` is true, then the data has already been loaded, 
//and startLoading() can immediately cancel itself (and "preLoading") and enable the
//rendering state instead.

let loaded = false;
let bitmap = null;
let sprites = null;

//important never to load the .wasm file twice, even if some other part of loading failed
let wasmLoaded = false;

function startLoading() {
    loadCancelled = false;

    if (loaded) {
        onLoaded();
    } else {
        let bitmapPromise = new Promise((resolve, reject) => {
            bitmap = new Image();
            bitmap.addEventListener("load", () => {
                resolve();
            });
            bitmap.addEventListener("error", (err) => {
                reject("unable to load spritesheet bitmap: ", err.type);
            });
            bitmap.src = "spritesheet.png";
        });

        let spritesPromise = new Promise((resolve, reject) => {
            window.fetch("spritesheet.json").then(
                response => {
                    if (!response.ok) {
                        throw Error(response.statusText);
                    }

                    response.json().then(
                        json => {
                            sprites = spritesFromJSON(json);
                            resolve();
                        },
                        err => reject(err)
                    );
                },
                err => reject(err)
            )
        });

        let wasmPromise = new Promise((resolve, reject) => {
            window.fetch("glsp_playground_bg.wasm").then(
                response => {
                    if (!response.ok) {
                        throw Error(response.statusText);
                    }
                    
                    response.arrayBuffer().then(
                        arrayBuffer => {
                            if (wasmLoaded) {
                                resolve();
                            } else {
                                wasm_bindgen(arrayBuffer).then(
                                    () => {
                                        wasmLoaded = true;
                                        resolve();
                                    },
                                    (err) => reject(err)
                                );
                            }
                        },
                        err => reject(err)
                    );
                },
                err => reject(err)
            );
        });

        Promise.all([bitmapPromise, spritesPromise, wasmPromise])
            .then(onLoaded)
            .catch(onLoadFailed);
    }
}

let loadCancelled = false;

function onLoaded() {
    if (loadCancelled) {
        return;
    }

    //book-keeping
    loaded = true;
    console.assert(bitmap !== null && sprites !== null && wasmLoaded);

    if (isPreLoading()) {
        stopPreLoading();
        loadCancelled = false;
    }

    //load the glsp code, then show the "intro" screen
    try {
        wasm_bindgen.initEngine(
            textarea.value,
            select.selectedOptions[0].textContent,
            Math.random()
        );
    } catch(err) {
        showErrorMsg(err);
        return;
    }

    intro.innerHTML =
        "<h2>" + wasm_bindgen.title() + "</h2>" + wasm_bindgen.blurb() +
        "<p>To begin, click the play area or press any key." +
        "<div class=\"strut\"></div>";

    showElementsForState("intro");
}

function onLoadFailed(err) {
    loaded = false;
    bitmap = null;
    sprites = null;

    if (isPreLoading()) {
        stopPreLoading();
        loadCancelled = false;
    }

    showErrorMsg(err);
}

function stopLoading() {
    //there actually isn't any way to cancel a Promise in ES6... but the worst-case scenario
    //is that `sprites` and `bitmap` get loaded twice, which isn't the end of the world.

    //we just set the `loadCancelled` flag to prevent onLoaded from being executed.
    loadCancelled = true;
}

function spritesFromJSON(json) {
    let sprites = {};

    for (slice of json.meta.slices) {
        let rect = Object.assign({}, slice.keys[0].bounds);

        if (slice.name.includes(".")) {
            let splits = slice.name.split(".");
            let frameCount = parseInt(splits[1]);
            let frameWidth = Math.floor(rect.w / frameCount);

            let frames = [];
            for (let f=0; f<frameCount; f++) {
                frames.push({
                    x: rect.x + (f * frameWidth),
                    y: rect.y,
                    w: frameWidth,
                    h: rect.h
                });
            }

            sprites[splits[0]] = frames;
        } else {
            sprites[slice.name] = [rect];
        }
    }

    return sprites;
}


//-------------------------------------------------------------------------------------------------
// the "errorMsg" state
//-------------------------------------------------------------------------------------------------

function showErrorMsg(err) {
    errorText.innerText = String(err);

    showElementsForState("errorMsg")
}


//-------------------------------------------------------------------------------------------------
// the "rendering" state
//-------------------------------------------------------------------------------------------------

let animationFrameHandle = null;
let bufWidth = canvas.width;
let bufHeight = canvas.height;
let screenWidth = bufWidth;
let screenHeight = bufHeight;

function resizeCanvas() {
    //bufWidth and bufHeight store the nominal size of the "back buffer", as specified by
    //the glsp code using the play:width and play:height globals.

    //this function applies a width, height and image-rendering strategy for the <canvas> element,
    //based on the size of the rendering area in device pixels. if it can be upscaled to an
    //integer ratio of its nominal size, while still occuping at least two-thirds of the
    //rendering area in at least one dimension, we perform that integer upscaling using
    //image-rendering: crisp-edges. otherwise we upscale to the next-lowest integer ratio using
    //2d transforms, and upscale or downscale the rest of the way using html.

    let padding = 16 * window.devicePixelRatio;
    screenWidth = screen.clientWidth * window.devicePixelRatio;
    screenHeight = screen.clientHeight * window.devicePixelRatio;

    let maxWidth = screenWidth - padding*2;
    let maxHeight = screenHeight - padding*2;

    let floatRatio = Math.min(maxWidth / bufWidth, maxHeight / bufHeight);
    let intRatio = Math.floor(floatRatio);

    let drawScale;

    if (intRatio >= 1 && intRatio/floatRatio >= 0.667) {
        //image-rendering: pixelated is pretty broken on Chrome right now, so we fall back to 
        //just using imageSmoothingEnabled and enlarging the canvas
        let isChrome = !CSS.supports("image-rendering", "crisp-edges");

        if (isChrome) {
            drawScale = Math.max(1, intRatio);

            canvas.width = bufWidth * drawScale;
            canvas.height = bufHeight * drawScale;

            canvas.style.imageRendering = "auto";
        } else {
            drawScale = 1;

            canvas.width = bufWidth;
            canvas.height = bufHeight;

            canvas.style.imageRendering = "crisp-edges";
        }

        canvas.style.width = (bufWidth * intRatio / window.devicePixelRatio).toFixed(2) + "px";
        canvas.style.height = (bufHeight * intRatio / window.devicePixelRatio).toFixed(2) + "px";
    } else {
        drawScale = Math.max(1, intRatio);

        canvas.width = bufWidth * drawScale;
        canvas.height = bufHeight * drawScale;

        canvas.style.width = (bufWidth * floatRatio / window.devicePixelRatio).toFixed(2) + "px";
        canvas.style.height = (bufHeight * floatRatio / window.devicePixelRatio).toFixed(2) + "px";
        canvas.style.imageRendering = "auto";
    }

    //drawing configuration is reset when the canvas' backing width/height changes, so we
    //need to assign any long-lasting settings here
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.scale(drawScale, drawScale);
    ctx.imageSmoothingEnabled = false;
}

function startRendering() {
    showElementsForState("rendering");

    bufWidth = wasm_bindgen.width();
    bufHeight = wasm_bindgen.height();
    resizeCanvas();

    //start the animation loop
    animationFrameHandle = window.requestAnimationFrame(onFirstFrame);
}

function stopRendering() {
    if (animationFrameHandle !== null) {
        window.cancelAnimationFrame(animationFrameHandle);
        animationFrameHandle = null;
    }
}

//the input map. each key is "LMB", "RMB", "MMB" or a KeyboardEvent.key string. each value is
//a table with keys `down`, `pressed` and `released` bound to booleans. we assume that if a
//particular key hasn't been seen, all three values are false.
let inputState = {};

let mouseClientX = 0;
let mouseClientY = 0;

document.addEventListener("mousemove", (ev) => {
    mouseClientX = ev.clientX;
    mouseClientY = ev.clientY;
});

//returned coordinates are relative to the interior of the <canvas> element, and measured in 
//back-buffer pixels, as in bufWidth and bufHeight. this function shouldn't be called when
//the canvas is hidden.
function mouseCoords() {
    let rect = canvas.getBoundingClientRect();

    return {
        x: Math.floor((mouseClientX - rect.left) * (bufWidth / rect.width)),
        y: Math.floor((mouseClientY - rect.top) * (bufHeight / rect.height))
    }
}

function inputEntry(key) {
    //we normalize alphabetic characters to uppercase
    if (key.length == 1 && key >= "a" && key <= "z") {
        key = key.toUpperCase();
    }

    if (!(key in inputState)) {
        inputState[key] = { down: false, pressed: false, released: false };
    }

    return inputState[key];
}

function stepInputState() {
    for (key of Object.keys(inputState)) {
        inputState[key].pressed = false;
        inputState[key].released = false;
    }
}

document.addEventListener("keydown", (ev) => {
    if (ev.key != "Undefined" && !ev.repeat) {
        let entry = inputEntry(ev.key);
        entry.down = true;
        entry.pressed = true;
    }
});

document.addEventListener("keyup", (ev) => {
    if (ev.key != "Undefined") {
        let entry = inputEntry(ev.key);
        entry.down = false;
        entry.released = true;
    }
});

document.addEventListener("mousedown", (ev) => {
    let buttonName = ["LMB", "MMB", "RMB"][ev.button];
    if (!buttonName) {
        return;
    }

    let entry = inputEntry(buttonName);
    entry.down = true;
    entry.pressed = true;
});

document.addEventListener("mouseup", (ev) => {
    let buttonName = ["LMB", "MMB", "RMB"][ev.button];
    if (!buttonName) {
        return;
    }

    let entry = inputEntry(buttonName);
    entry.down = false;
    entry.released = true;
});

let lastTimeStamp = 0;

//to ensure that the `dt` argument is always accurate, we discard the first frame. this also
//helps to ensure that (play:pressed?) and (play:released?) are false during the first frame.
function onFirstFrame(timeStamp) {
    stepInputState();
    lastTimeStamp = timeStamp;
    animationFrameHandle = window.requestAnimationFrame(onAnimationFrame);
}

function onAnimationFrame(timeStamp) {

    //detect when the viewport has been resized, and resize the canvas if so
    let curScreenWidth = screen.clientWidth * window.devicePixelRatio;
    let curScreenHeight = screen.clientHeight * window.devicePixelRatio;
    if (screenWidth != curScreenWidth || screenHeight != curScreenHeight) {
        resizeCanvas();
    }

    //yield to glsp
    try {
        wasm_bindgen.update((timeStamp - lastTimeStamp)/1000);
    } catch(err) {
        stopRendering();
        showErrorMsg(err);
        return null;
    }

    //continue the main loop
    stepInputState();
    lastTimeStamp = timeStamp;
    animationFrameHandle = window.requestAnimationFrame(onAnimationFrame);
}


//-------------------------------------------------------------------------------------------------
// `play:` callbacks for glsp
//-------------------------------------------------------------------------------------------------

function playMouseX() {
    return mouseCoords().x;
}

function playMouseY() {
    return mouseCoords().y;
}

//to keep things minimalist, we only support querying the following symbols: lmb, mmb, rmb, 
//a through z, up, down, left, right, space, enter.
let knownInputs = {
    up: "ArrowUp",
    down: "ArrowDown",
    left: "ArrowLeft",
    right: "ArrowRight",
    space: " ",
    enter: "Enter",
    lmb: "LMB",
    mmb: "MMB",
    rmb: "RMB"
};

let simpleInputs = [
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
    "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
];

for (simple of simpleInputs) {
    knownInputs[simple] = simple.toUpperCase();
}

function playDownP(input) {
    if (!(input in knownInputs)) {
        throw "unrecognized input " + input + " passed to play:down?"
    }

    return inputEntry(knownInputs[input]).down;
}

function playPressedP(input) {
    if (!(input in knownInputs)) {
        throw "unrecognized input " + input + " passed to play:pressed?"
    }

    return inputEntry(knownInputs[input]).pressed;
}

function playReleasedP(input) {
    if (!(input in knownInputs)) {
        throw "unrecognized input " + input + " passed to play:released?"
    }

    return inputEntry(knownInputs[input]).released;
}

function playFill(x, y, width, height, r, g, b) {
    x = Math.round(x);
    y = Math.round(y);
    width = Math.round(width);
    height = Math.round(height);
    r = Math.max(0, Math.min(255, Math.round(r)));
    g = Math.max(0, Math.min(255, Math.round(g)));
    b = Math.max(0, Math.min(255, Math.round(b)));

    ctx.fillStyle = "rgb(" + r + "," + g + "," + b + ")";
    ctx.fillRect(x, y, width, height);
}

function playDraw(spriteName, dx, dy, hflip, vflip, frame_i) {
    dx = Math.round(dx);
    dy = Math.round(dy);
    frame_i = Math.round(frame_i);

    if (!(spriteName in sprites)) {
        throw "unrecognized sprite " + spriteName + " passed to play:draw";
    }

    let frames = sprites[spriteName];
    let { x, y, w, h } = frames[((frame_i % frames.length) + frames.length) % frames.length];
    
    if (hflip || vflip) {
        let xScale = 1;
        let yScale = 1;

        if (hflip) {
            xScale = -1;
            dx += w;
        }

        if (vflip) {
            yScale = -1;
            dy += h;
        }

        ctx.save();
        ctx.translate(dx, dy);
        ctx.scale(xScale, yScale);
        ctx.drawImage(bitmap, x, y, w, h, 0, 0, w, h);
        ctx.restore();
    } else {
        ctx.drawImage(bitmap, x, y, w, h, dx, dy, w, h);
    }
}
