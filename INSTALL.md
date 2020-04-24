# Installing Glow

Instructions for compiling and installing Glow

## Install Gerbil Scheme

First, install the latest version of the [Gerbil Scheme](https://cons.io/) compiler,
if you haven't installed it already.

To install the very same version that we're using for development,
use [NixOS](https://nixos.org/), or [Nix](https://nixos.org/nix/) in userland
(on top of any Linux distribution, also on macOS, on some day on WSL):

    nix-env -f http://github.com/fare-patches/nixpkgs/archive/fare.tar.gz -iA gerbil-unstable

Gerbil Scheme is also available on macOS on [homebrew](https://brew.sh/),
but you'll need at least version 0.16 of Gerbil, when it's available:

    brew install gerbil-scheme

If your system doesn't provide Gerbil Scheme,
hopefully it provides [Gambit Scheme](http://gambitscheme.org/),
and you can [build Gerbil from source](https://cons.io/guide/#source-code).

## Install Gerbil libraries

If you know you won't hack on any of our dependencies, just install them with `gxpkg`:

```
DEPS=(github.com/fare/gerbil-utils
      github.com/drewc/js-syntax
      github.com/drewc/gerbil-swank
      github.com/drewc/drewc-r7rs-swank) ;
for i in ${DEPS[@]} ; do
  gxpkg install $i &&
  gxpkg build $i
done
```

Note that [drewc-r7rs-swank](https://github.com/drewc/drewc-r7rs-swank) is
a gerbil-friendlier fork of [r7rs-swank](https://github.com/ecraven/r7rs-swank),
the latter containing still-unmerged patches
(see also [r7rs-swank PR #10](https://github.com/ecraven/r7rs-swank/pull/10)
for a similar patch to it, from [my fork](github.com/fare-patches/r7rs-swank)).

If at some point you need to hack some or all of our dependencies,
you can reinstall them this way,
after possibly editing the script to adjust the `SRCDIR` shell variable
to adjust where you'll install those dependencies:

```
DEPS=(github.com/fare/gerbil-utils
      github.com/drewc/js-syntax
      github.com/drewc/gerbil-swank
      github.com/drewc/drewc-r7rs-swank) ;
SRCDIR=.. ;
(cd ${SRCDIR} &&
for i in ${DEPS[@]} ; do
  (gxpkg uninstall $i > /dev/null 2>&1 ;
   git clone https://$i &&
   cd $(basename $i) &&
   gxpkg link $i $PWD &&
   gxpkg build $i $PWD)
done)
```

## Build it

After you have installed dependencies as above, you should be all ready to go.
You can build a binary with:

    ./build.ss

## Test it

After you have installed dependencies, you should be all ready to go.
You can run all our unit tests with:

    ./unit-tests.ss

Note that you don't need to build Glow as above to test it.
However, you do need the dependencies to be built.

## Run it

Once you built the binary with `./build.ss`, you can run it with `./glow`.

## Install it

You can copy the `./glow` binary anywhere on your machine, HOWEVER,
note that at the time being, the binary relies
not only on your Gambit and Gerbil installations,
but also on the compiled file in `~/.gerbil/lib/`
and so only works for the with Unix user.

Now, you can export the `GERBIL_PATH` environment variable
on the user who compiles Glow and/or on the other users who use it
to specify where the compiled files should be stored.
You may need to either copy the compiled files or re-build them
for the dependencies as well as for Glow itself, if you change the `GERBIL_PATH`.
