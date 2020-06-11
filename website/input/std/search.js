//the static site generator prefixes this file with a literal array bound to the local variable
//searchIndex. each field is a 3-tuple: [name, is_code, href]

let apiSearchField;

//we cache the starting layout for the "APIs by Category" sidebar so that it can be restored if
//the search-field is cleared
let originalHeaderText;
let originalApiLinkList;

//search("text") replaces the "APIs by Category" sidebar with a list of `searchIndex` which
//case-insensitively match the query string, starting with exact matches, then prefix matches, 
//then all other matches. the original relative ordering is preserved. search("") restores the 
//original "APIs by Category" sidebar.
function search(rawSearchText) {
	let searchText = rawSearchText.toLowerCase();

	let apiList = document.getElementById("api-list");
	let apiListHeader = document.getElementById("api-list-header");

	if (searchText.length == 0) {
		if (apiList.lastElementChild != originalApiLinkList) {
			if (apiList.lastElementChild.id == "api-link-list") {
				apiList.removeChild(apiList.lastElementChild);
			}

			apiList.appendChild(originalApiLinkList);
			apiListHeader.innerText = originalHeaderText;
		}	
	} else {
		if (apiList.lastElementChild == originalApiLinkList) {
			apiListHeader.innerText = "Search Results";

			apiList.removeChild(apiList.lastElementChild);

			let linkList = document.createElement("div");
			linkList.id = "api-link-list";
			linkList.className = "link-list";

			apiList.appendChild(linkList);
		}

		let linkList = apiList.lastElementChild;

		while (linkList.lastElementChild) {
			linkList.removeChild(linkList.lastElementChild);
		}

		let prefixResults = []
		let otherResults = [];
		for (searchItem of searchIndex) {
			let candidate = searchItem[0].toLowerCase();

			if (candidate == searchText) {
				prefixResults.unshift(searchItem);
			} else {
				let findIndex = candidate.indexOf(searchText);
				if (findIndex == 0) {
					prefixResults.push(searchItem);
				} else if (findIndex != -1) {
					otherResults.push(searchItem);
				}
			}
		}
		let results = prefixResults.concat(otherResults);

		if (results.length == 0) {
			let emptyItem = document.createElement("div");
			emptyItem.className = "list-level0 empty-message";
			emptyItem.appendChild(document.createTextNode("No results found"));
			linkList.appendChild(emptyItem);
		} else {
			for (result of results) {
				let userFacingName = result[0];
				let isCode = result[1];
				let href = result[2];

				let a = document.createElement("a");
				console.assert(encodeURIComponent(href) == href);
				a.href = href;
				linkList.appendChild(a);

				let listItem = document.createElement("div");
				listItem.className = "list-level0";
				a.appendChild(listItem);

				let span;
				if (isCode) {
					span = document.createElement("code");
				} else {
					span = document.createElement("span");
				}
				span.appendChild(document.createTextNode(userFacingName));
				listItem.appendChild(span);
			}
		}
	}
}

//invoked when return is pressed with the search-field focused. if there is at least one item
//in the search results list, emulates a click on the topmost item (to navigate to that page).
function navigateToFirstResult() {
	let apiList = document.getElementById("api-list");

	let apiLinkList = apiList.lastElementChild;
	if (apiLinkList != originalApiLinkList) {
		let target = apiLinkList.firstElementChild;
		if (target.tagName == "A") {
			target.click();
		}
	}
}

function initSearch() {
	//look up elements
	apiSearchField = document.getElementById("api-search-field");
	originalHeaderText = document.getElementById("api-list-header").innerText;
	originalApiLinkList = document.getElementById("api-link-list");

	//by default, the search-field is hidden so that it doesn't show up when js is disabled
	apiSearchField.parentElement.style.display = "block";

	//if the search-field text is changed or submitted, invoke search()
	apiSearchField.addEventListener("input", (ev) => {
		search(ev.target.value);
	});

	apiSearchField.addEventListener("change", (ev) => {
		ev.target.blur();
		search(ev.target.value);
	});

	//if the return key is pressed, unfocus the search field and perhaps invoke navigateToFirstResult.
	//if the esc key is pressed, clear and unfocus the search field.
	apiSearchField.addEventListener("keydown", (ev) => {
		if (ev.key == "Enter") {
			ev.target.blur();
			if (ev.target.value.length > 0) {
				navigateToFirstResult();
			}
		}

		if (ev.key == "Escape") {
			ev.target.blur();
			ev.target.value = "";
			search("");
		}
	});

	//when the hamburger menu is opened or closed, clear the search field.
	document.getElementById("checkbox-hack").addEventListener("input", (ev) => {
		let apiSearchField = document.getElementById("api-search-field");
		apiSearchField.blur();
		apiSearchField.value = "";
		search("");
	});

	//when the search-field is not focused and the S or / key is pressed, focus it.
	document.body.addEventListener("keydown", (ev) => {
		let apiSearchField = document.getElementById("api-search-field");
		if (!(document.activeElement == apiSearchField && document.hasFocus())) {
			if (ev.key == "/" || ev.key == "s" || ev.key == "S") {
				apiSearchField.focus();
				ev.preventDefault();
			}
		}
	});
}

initSearch();
