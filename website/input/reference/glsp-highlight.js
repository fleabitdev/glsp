//a tiny syntax-highlighter for our glsp code blocks
for (code of document.querySelectorAll("pre .hljs:not(.language-rust)")) {
	code.innerHTML = code.innerHTML.replace(
		/^(;.*)$/mg,
		"<span class=\"hljs-comment\">$1</span>"
	);

	code.innerHTML = code.innerHTML.replace(
		/(\s;.*)$/mg,
		"<span class=\"hljs-comment\">$1</span>"
	);

	console.log(code.innerHTML);

	code.innerHTML = code.innerHTML.replace(
		/(#\|(.|\s)*?\|#)/g,
		"<span class=\"hljs-comment\">$1</span>"
	);
}

//we also hide the "play" and "show hidden lines" buttons - i find them distracting
for (button of document.querySelectorAll(".fa-play, .fa-expand")) {
	button.style.display = "none";
}

//finally, we set all <code> elements within the text to have non-breaking whitespace
for (code of document.querySelectorAll("p > code")) {
	code.style.whiteSpace = "nowrap";
}
