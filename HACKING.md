# To hack on Glow

## Install dependencies

If you know you won't hack on any of our dependencies, just install them with `gxpkg`:
```
DEPS=(github.com/drewc/js-syntax github.com/fare/gerbil-utils github.com/fare-patches/r7rs-swank)
for i in ${DEPS[@]} ; do gxpkg install $i ; done
```

Note that `github.com/fare-patches/r7rs-swank` is
a patch to `github.com/ecraven/r7rs-swank` that still hasn't been merged yet
(see https://github.com/ecraven/r7rs-swank/pull/10).

If at some point you need to hack some or all of our dependencies, you can reinstall them this way:
```
DEPS=(github.com/drewc/js-syntax github.com/fare/gerbil-utils github.com/fare-patches/r7rs-swank)
(cd .. &&
for i in ${DEPS[@]} ; do
  (git clone https://$i &&
   gxpkg uninstall $i > /dev/null 2>&1 &&
   cd $(basename $i) &&
   gxpkg link $i $PWD &&
   gxpkg build $i $PWD)
done)
```

### Configuring SLIME

Make sure a recent SLIME is installed in Emacs, and use the following,
replacing the path in it by the place where you checked out the glow source code:
```
(defun gerbil-scheme-start-swank (file encoding)
  (format "%S\n%S\n%S\n"
          '(include "/home/fare/src/fare/glow/all-glow.ss")
          '(import :ecraven/gerbil-swank)
          `(start-swank ,file)))

(setq slime-lisp-implementations
      (cons `(gerbil-scheme ("gxi" "-:d-")
                            :init gerbil-scheme-start-swank)
            slime-lisp-implementations))
```

### Writing tests

Tests are in subdirectories named `t/` of the directories with the files they test.
Inside those test directories, files named ending in `-test.ss` are tests,
and in a file `foo-test.ss` the main test entry point shall be exported as symbol `foo-test`.
E.g. `compiler/t/multipass-test.ss` exports a test suite called `multipass-test`.

The top-level script `./unit-tests.ss` will automatically find and run those tests,
and will skip tests that do not follow this convention.
It prints a test summary that shall let you see that your test did (or didn't) pass;
if not you failed to follow the convention and your test didn't run.
