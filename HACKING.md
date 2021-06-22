# To hack on Glow

## First, install Glow

Before you attempt to hack the Glow compiler,
please make sure you have [installed it](INSTALL.md) properly and it passes all tests.

## Programming Environment

### Gerbil and Gambit

[Glow](https://glow-lang.org/) is implemented in [Gerbil Scheme](https://cons.io/),
itself a layer on top of [Gambit Scheme](http://gambitscheme.org/).

While hacking on Glow, you'll find that the lower-level primitives you use are defined by
[Gambit](https://www.iro.umontreal.ca/~gambit/doc/gambit.html),
while the higher-level functions and macros are defined by
[Gerbil](https://cons.io/reference/).

You can find the community for both these layers of language on Gitter:
[Gerbil Scheme gitter](https://gitter.im/gerbil-scheme/community) and
[Gambit Scheme gitter](https://gitter.im/gambit/gambit).

The biggest drawback of Gerbil is that it is insufficiently documented,
and that the documentation that exists is split in many layers:

  - The R5RS and/or R7RS for the base Scheme language.
  - Gambit documentation for Gambit (and for some internals the Gambit sources).
  - Gerbil documentation for Gerbil and its standard library (and for some things, again, its sources).
  - The SRFI documents for some of the somewhat standard libraries provided by Gambit or Gerbil.
  - The sources of various libraries we use (and often develop) on top of Gerbil,
    such as Gerbil-Utils, Gerbil-POO, etc.

Don't hesitate to ask around for help.

Also, the `apropos` function can help you find functions that have already been
loaded in your environment, if you only remember part of their name, as in
`(apropos "hash")` to discover the name of a function relating to hash-tables.

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

### Running tests

Commands to get you started.

Help for test-suite CLI:
```
./unit-tests.ss help # general help for top-level commands
./unit-tests.ss help <command> # help for a command
```

Running tests:
```
./unit-tests.ss # Run full unit-test suite
./unit-tests.ss integration # Run full integration-test suite
./unit-tests.ss test <file> # Run a single test single-file
./unit-tests.ss pass <pass> <file> # Run compiler pass up to <pass> for <file>
```

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

## Managing dependencies

Glow is being actively developed, alongside with its dependencies.
The following sections provide instructions on updating our nixpkgs snapshot, cachix, Docker and CI. 

### Setup

Ensure you have `nix-prefetch-git` installed:
```sh
nix-env -iA nixpkgs.nix-prefetch-git
```

Checkout the `nixpkgs` package snapshot locally: https://github.com/MuKnIO/nixpkgs/commits/devel. 
```sh
git clone git@github.com:MuKnIO/nixpkgs.git
git worktree add ../devel
export nixpkgs=../devel
```

Note: `$nixpkgs` is a `git worktree` of a checkout,
so that when included in the docker image it doesn't pull the entire git history with it.

Add the following to your shell, updating `/path/to` with the actual path in your filesystem:
```sh
gerbsup () {
  local gu=/path/to/gerbil-utils 
  GERBIL_LOADPATH=$gu $gu/scripts/update-gerbil-nix-recipe.ss $@
}

nixglow () {
  b=/path/to/gerbil-utils/build.ss 
  time $b nixpkgs -f "$nixpkgs" && time $b publish -f "$nixpkgs" && time $b docker ${1:-all}
}
```

For updating `cachix`, request access token, then do:
```
cachix authtoken $token
```

Use our nixpkgs cache:
```
cachix use mukn
```

### Before updating

**Ensure that the current `master` branch of glow and dependencies are compatible.**

Sometimes you may need to merge although CI fails,
due to breaking changes etc... if that's the case,
request help from project owners / maintainers.

### Updating

Update nixpkgs first. Ensure the updated nix expressions can be built locally.
```sh
gerbsup

# You may have to pin various packages depending on their compatibility. See `gerbsup -h`.
# For example, to pin gambit:
# gerbsup -g feeley/gambit@c0753ff127baefebd00193a135f48f18c59c496a
```

Commit the changes above, one per package,
following the [nixpkgs convention for commit messages](https://github.com/NixOS/nixpkgs/blob/master/.github/CONTRIBUTING.md),
to the `devel` branch.
If necessary, rebase before release so there's only one commit per package.
Push your changes to the `devel` branch (force push if you're rebased).

Update cachix and Docker:
```sh
nixglow
```

Update dependencies on CI:
```sh
# Just gerbil packages
nix-thunk update dep/gerbil-* dep/nixpkgs
```

### After Updating

- Ensure CI pipeline passes.
- Ensure dependencies from `nixpkgs` can be installed.
