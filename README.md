[![MELPA](https://melpa.org/packages/gorepl-mode-badge.svg)](https://melpa.org/#/gorepl-mode)

# gorepl-mode
A minor emacs mode for Go REPL.


## Synopsis
**gorepl-mode** is a Go REPL interaction library for Emacs. It's built on top of
[gore](https://github.com/motemen/gore).


Gorepl packs some features (in no particular order):

* Powerful REPL - thanks to gore
* Interactive code evaluation
* Evaluate expressions selected
* Launch a repl with loaded file in a context

## Installation

`gorepl-mode` is available on [MELPA](http://melpa.org)

You can install `gorepl-mode` with the following command:

<kbd>M-x package-install [RET] gorepl-mode [RET]</kbd>


### Prerequisite

Install [gore](https://github.com/motemen/gore).

### Basic configuration

* Enable `gorepl-mode` in `go-mode` buffers:

```el
(add-hook 'go-mode-hook #'gorepl-mode)
```

## Keyboard shortcuts

* <kbd>M-x gorepl-run</kbd>: Launch an instance of gore REPL client.

* <kbd>M-x gorepl-run-load-current-file</kbd>: Launch an instance of gore REPL client with the current file loaded.

### gorepl-mode

Keyboard shortcut                    | Description
-------------------------------------|-------------------------------
<kbd>C-C C-g</kbd>                   | Launch an instance of gore REPL client
<kbd>C-C C-l</kbd>                   | Launch an instance of gore REPL client - with the current file loaded
<kbd>C-c C-e</kbd>                   | Evaluate the region selected
<kbd>C-c C-r</kbd>                   | Evaluate the current line

### Issues

The performance for gore is not very fast, in fact when a gorepl is running using `gorepl-run-load-current-file` it can be slow at first, especially with large files, because it's doing `go run `to all file.


### TODO

* Autocompletion
* go-mode in gorepl-mode
