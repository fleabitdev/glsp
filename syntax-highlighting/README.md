# Syntax Highlighting

The file [`GameLisp.tmLanguage.json`](./GameLisp.tmlanguage.json) is the canonical 
syntax-highlighting grammar for GameLisp. It should be easy enough to install it into
any text editor which accepts `.tmLanguage.json` files (most of them!).

Instructions for specific text editors are provided below.


## Sublime Text 3

Open your local packages folder using `Preferences/Browse Packages...`. Copy the 
[`sublime-text-3/GameLisp`](.sublime-text-3/GameLisp) directory into that folder.

Also, consider using `Preferences/Settings - Syntax Specific` to adjust the following settings:

```json
{
	"tab_size": 2,
	"rulers": [100],
	"word_separators": "\"'`~,()[]",
	"match_brackets": true
}
```

By default, Sublime Text 3 will only match brackets which are a short distance apart. This
can be troublesome when editing Lisp code. To mitigate the problem, consider installing the
[`BracketHighlighter` plugin](https://packagecontrol.io/packages/BracketHighlighter).


## Visual Studio Code

Simply copy the [`visual-studio-code/glsp`](./visual-studio-code/glsp) directory to your
extensions directory:

- On Windows, `%USERPROFILE%\.vscode\extensions`, for example,
  `C:\Users\YourName\.vscode\extensions`.

- On Mac or Linux, `~/.vscode/extensions`.
