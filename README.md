# README

## Description
My `.emacs.d/init.el` is based/inspired off of [Gavin Freeborn](https://github.com/Gavinok/emacs.d/blob/main/init.el) and [adventuresinwhy.com/post/eglot](https://www.adventuresinwhy.com/post/eglot/). I have yet to master all the functionality he includes but I like how he has chosen modern emacs packages that is fast. My previous `init.el` had very slow code completion and weird auto-indenting at times.

## Tree-Sitter mode
If you want "tree-sitter" support for individual language, clone this [repo](https://github.com/casouri/tree-sitter-module) and:
```bash
$ ./build.sh python
$ mkdir ~/.emacs.d/tree-sitter/
$ mv dist/libtree-sitter-python.dylib ~/.emacs.d/tree-sitter
```


## Packages included (so far)
* quelpa
* async
* savehist
* repeat
* undo-fu
* align
* vertico
* orderless
* marginalia
* consult
* ibuffer
* isearch
* ffap
* apheleia
* lsp
* highlight-indent-guides
