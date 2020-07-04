# To hack on Glow

## First, install Glow

Before you attempt to hack the Glow compiler,
please make sure you have [installed it](INSTALL.md) properly and it passes all tests.

## Programming Environment

### Gerbil and Gambit

Glow is implemented in [Gerbil Scheme](https://cons.io/),
itself a layer on top of [Gambit Scheme](http://gambitscheme.org/).

While hacking on Glow, you'll find that the lower-level primitives you use are defined by
[Gambit](https://www.iro.umontreal.ca/~gambit/doc/gambit.html),
while the higher-level functions and macros are defined by
[Gerbil](https://cons.io/reference/).

You can find the community for both these layers of language on Gitter:
[Gerbil Scheme gitter](https://gitter.im/gerbil-scheme/community) and
[Gambit Scheme gitter](https://gitter.im/gambit/gambit).

## Configuring Your Editor

### Debugging with Emacs gerbil-mode

You can setup [Gerbil Development with Emacs](https://cons.io/guide/emacs.html).
In addition to the regular setup, you can arrange for `gxi` to come
with all the relevant Glow modules already loaded at startup,
by configure your Emacs with something like:

```
(setq gerbil-program-name "/path/to/glow/ggxi")
(setq scheme-program-name gerbil-program-name)
```

After you rebuild some modules, you can reload them with:

```
(reload "path/to/module.ss")
```

### Debugging with Emacs SLIME

As an alternative to `gerbil-mode`, but still in Emacs,
you can use [SLIME](https://common-lisp.net/project/slime/).
Make sure a recent SLIME is installed in Emacs, and use the following,
replacing the path in it by the place where you checked out the glow source code:

```
(defun gerbil-scheme-start-swank (file encoding)
  (format "%S\n%S\n%S\n%S\n"
          '(include "/home/fare/src/fare/glow/all-glow.ss")
          '(import :drewc/gerbil-swank/swank)
          '(import :drewc/r7rs/gerbil-swank)
          `(start-swank ,file)))

(setq slime-lisp-implementations
      (cons `(gerbil-scheme ("gxi" "-:d-")
                            :init gerbil-scheme-start-swank)
            slime-lisp-implementations))
```

### Syntax Highlighting for Glow

We do not have a Glow mode for Emacs yet, but you can use `javascript-mode` for the syntax highlighting:

```
(add-to-list 'auto-mode-alist '("\\.glow$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.sexp$" . gerbil-mode))
```

## Hacking the Glow compiler

### Writing tests

After you have installed our dependencies,

Tests are in subdirectories named `t/` of the directories with the files they test.
Inside those test directories, files named ending in `-test.ss` are tests,
and in a file `foo-test.ss` the main test entry point shall be exported as symbol `foo-test`.
E.g. `compiler/t/multipass-test.ss` exports a test suite called `multipass-test`.

The top-level script `./unit-tests.ss` will automatically find and run those tests,
and will skip tests that do not follow this convention.
It prints a test summary that shall let you see that your test did (or didn't) pass;
if not you failed to follow the convention and your test didn't run.

Please ensure that regression tests always pass and never push or merge into master
any code that breaks them. When Gitlab, the CI system shall help you with it.

Note that regular code outside of a `t/` directory must not depend
on test code in `t/` directories.
As an exception, and as a practical tool for debugging,
`t/common` is included in the interactive image.
