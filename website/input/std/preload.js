//the static site generator prefixes this file with a literal table bound to the local variable
//preloadBodies. the keys are hrefs, like "alphabetic-index" or "add.html", and the values are
//2-tuples, where the first field is the page's title, and the second field is HTML text 
//representing a <body> element for that page.

//this is a very large file, but because it's so redundant it gzips down to a tiny fraction of
//its actual size (currently 3mb -> 120kb). it shouldn't use up too much bandwidth.

//once this file has been loaded, we intercept any `click` events involving <a> elements with a
//non-empty href (note that we don't intercept middle-click, right-click, or left-click with
//a modifier key). if the filename is present in the preload database, then rather than truly
//navigating to that page, we instead replace the contents of `document.body`, update the title, 
//and push a new history entry.

//we also push a new history entry onto the stack when the hamburger menu is opened or closed.

//each history entry stores the page href, scroll position, and the current contents of the
//search field. these are constantly updated as they change, using replaceState.

function stateSnapshot() {
    //on the first page we visit, we don't have a canonical name for the page - all we have is
    //the address bar contents, which the user could have typed in manually. "/std/", "/std/index"
    //and "/std/index.html" are all synonyms.

    //we generate a canonical name by taking the last path segment from the address bar, changing
    //it to "index" if it's empty, and stripping a ".html" suffix if one is present. then, if
    //preloadBodies uses the ".html" suffix, we add it back.
    let curHref = window.location.pathname.split("/").pop();

    if (curHref.length == 0) {
        curHref = "index";
    }

    let hasSuffix = curHref.endsWith(".html");
    let needsSuffix = "index.html" in preloadBodies;

    if (!hasSuffix && needsSuffix) {
        curHref += ".html";
    }

    if (hasSuffix && !needsSuffix) {
        curHref = curHref.substring(0, curHref.length - 5);
    }

    console.assert(curHref in preloadBodies);

    //before messing with the history, make sure that we've scrolled to the specified #fragment
    if (window.location.hash.length > 0) {
        let element = document.getElementById(window.location.hash.substring(1));
        if (element) {
            element.scrollIntoView();
        }
    }

    //generate the state table
    return {
        href: curHref,
        scrollX: window.scrollX,
        scrollY: window.scrollY,
        checked: document.getElementById("checkbox-hack").checked
    };
}

function loadPage(href) {
    //parse the html stored in preloadBodies and assign it as the new <body> element
    let dummyDocument = `
        <!DOCTYPE html>
        <html lang="en">
            <head>
                <meta charset="utf-8">
                <title></title>
            </head>
            ${preloadBodies[href][1]}
        </html>
    `;
    document.body = (new DOMParser()).parseFromString(dummyDocument, "text/html").body;
    registerHandlers();

    //update the title string
    document.title = preloadBodies[href][0];

    //re-register search.js
    initSearch();
}

function registerHandlers() {
    document.body.addEventListener("click", clickHandler);
    document.getElementById("checkbox-hack").addEventListener("change", checkedHandler);
}

function popStateHandler(ev) {
    if (ev.state) {
        loadPage(ev.state.href);

        window.scrollTo(ev.state.scrollX, ev.state.scrollY);

        document.getElementById("checkbox-hack").checked = ev.state.checked;
    }
}

function scrollHandler(ev) {
    let cur = Object.assign({}, history.state);
    cur.scrollX = window.scrollX;
    cur.scrollY = window.scrollY;
    history.replaceState(cur, "");
}

function checkedHandler(ev) {
    let cur = Object.assign({}, history.state);
    cur.checked = ev.target.checked;

    history.pushState(cur, "");
}

function clickHandler(ev) {
    //detect whether to ignore this event and return `true` so that it will be handled as normal
    if (ev.button != 0 || ev.defaultPrevented || 
        ev.altKey || ev.ctrlKey || ev.metaKey || ev.shiftKey) {
        return true;
    }

    //detect whether the target, or any of its parents, are an <a> with a nonempty href which
    //is present in preloadBodies, or a #fragment href.
    let iter = ev.target;
    let href;
    while (iter) {
        if (iter.tagName == "A") {
            let attr = iter.getAttribute("href");
            if (attr && attr.length > 0 && (attr in preloadBodies || attr.startsWith('#'))) {
                href = attr;
                break
            }
        }

        iter = iter.parentElement;
    }

    if (!href) {
        return true;
    }

    //if href is a #fragment, just scroll to the element with that id. otherwise, "navigate"
    //to the new page and scroll to the top of the page.
    let fragment;
    if (href.startsWith('#')) {
        let fragment = href.substring(1);

        let element = document.getElementById(fragment);
        if (element) {
            element.scrollIntoView();

            //store a new history entry
            let historyState = {
                href: history.state.href,
                scrollX: window.scrollX,
                scrollY: window.scrollY,
                checked: history.state.checked
            };
            history.pushState(historyState, "", history.state.href + href);
        } else {
            return true;
        }
    } else {
        loadPage(href);
        window.scrollTo(0, 0);

        //store a new history entry
        let historyState = {
            href: href,
            scrollX: 0,
            scrollY: 0,
            checked: false
        };
        history.pushState(historyState, "", href);
    }

    //we call preventDefault last because, if any of the above fails, we want to navigate to the
    //destination page as a fallback
    ev.preventDefault();
    return false;
}

registerHandlers();
window.addEventListener("popstate", popStateHandler);
window.addEventListener("scroll", scrollHandler);

history.scrollRestoration = "manual";
history.replaceState(stateSnapshot(), "");
