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
