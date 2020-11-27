# Installing Glow

Instructions for compiling and installing Glow, testing and running it.

Long story short, to try Glow, you can just use the following one-liner,
and skip reading the rest of this file:

    curl -L https://glow-lang.org/install/glow-user-install | sh

## Warnings and Prerequisites

1. At this time Glow only runs on Linux or macOS (on any supported architecture).
   On these platforms, we use the [Nix](https://nixos.org/nix/) package manager
   for its deterministically reproducible builds, and its support for distributed computing:
   If it builds for us, it should build identically for you.

2. Installing Glow for the first time may require recompiling a lot of code,
   which may take an hour.

3. Installing Glow, or any software, requires that you trust the authors and their infrastructure.
   To minimize the need for trust, and minimize the opportunity for damage
   from applications that do breach your trust,
   we recommend you use isolated virtual machines for each application.
   For this and for extra security, you might want to start using
   @a[href: "https://www.qubes-os.org/"]{Qubes OS}
   before you start using your laptop to manipulate real assets.

*In the future*, we will improve the installation as follows:

4. We will support JavaScript as a target platform,
   and distribute pre-compiled portable "binaries" for that platform.
   This will allow Glow to run on Windows, iOS, Android, and on any web browser.

5. We will also provide some [cachix](https://cachix.org/) server for our Nix packages,
   so you can install without recompilation.

But for now, be prepared for the initial installation to take a lot of time,
so run it in the background and do something else while your machine compiles a lot of code.

## Simplified Installation for Users

If you mean to use Glow (as a developer or end-user),
but not modify the language implementation itself (compiler, runtime, libraries),
then the following command will do everything,
and you don't need the further sections of this file:

    curl -L https://glow-lang.org/install/glow-user-install | sh

From a checkout of this repository, you can just use:

    sh scripts/glow-user-install

## Simplified Installation for Implementers

If you mean not just to use Glow, but also
to modify its implementation (compiler, runtime, libraries),
then the following command will do everything:

    curl -L https://glow-lang.org/install/glow-implementer-install | sh

From a checkout of this repository, you can just use the below,
then `rm -rf glow` to remove the second redundant checkout of `glow` it will download:

    sh scripts/glow-implementer-install

Once you have thus installed all the prerequisites,
you can `cd glow` to enter the downloaded git checkout,
at which point you can build the software with:

    nix-build

Or you can use `nix-shell` to enter an environment in which to build and hack by hand.

You don't need the further sections in this file, but
you need to read the [HACKING.md](HACKING.md) file for how to hack.

## Manual installation via Nix

You can look at the above-mentioned installation scripts, and reproduce their steps manually.

## Trying it out with Docker

When we have a stable release, we'll directly provide an image `mukn/glow:stable` on
[Docker](https://hub.docker.com/repository/docker/mukn/glow).

In the meantime, you can build your own with:

    docker build -t mukn/glow -f scripts/Dockerfile .

## Installing Glow the Hard Way

If you don't use either Linux or macOS, and can't use Docker,
or if you insist on not using Nix, then you will have to build and install Glow the hard way:
installing GCC, on top of it Gambit Scheme, on top of it Gerbil Scheme, on top of it Glow.

You might be able to use [homebrew](https://brew.sh/) on macOS,
or your Linux distribution's package management tools
to skip a few steps, if it supports a recent enough version of Gambit or Gerbil.

### First, install GCC

I'll assume you know how this works.
GCC probably comes with your Operating System or software distribution.
On Windows, you may have to use WSL or [MinGW](http://mingw.org/).

Note that the LLVM's Clang won't do: we use Gambit Scheme,
which compiles ten times (10x) slower with Clang than with GCC,
producing code that runs three times (3x) slower.

### Second, install Gambit Scheme

The [Gambit Scheme](http://gambitscheme.org/) compiler
is already available on a lot of operating systems and Linux distributions.
Go grab a binary package if you can.
If you can't, get its source code and compile it according to its instructions.
You will need release 4.9.3 or later, or a recent checkout from git.

### Third, install Gerbil Scheme

The [Gerbil Scheme](https://cons.io/) compiler
is only available on a few platforms, in which case you can go get a binary package,
but could also use Nix as above.
In other cases, get its source code and compile it
according to [its instructions](https://cons.io/guide/#source-code).
You will need release 0.16 or later, possibly a even more recent development version.

For instance, on macOS using [homebrew](https://brew.sh/), you could use:

    brew install gerbil-scheme

On Nix (recommended, but could skip to the last step as above), you could have used:

    nix-env -f http://github.com/fare-patches/nixpkgs/archive/fare.tar.gz -iA gerbil-unstable

Note that to avoid conflict with other versions of Gerbil, you may have to make sure that

  1. The Nix version of Gerbil appears first in your `$PATH`,
     which you can check with `which gxi`, though if you just changed your PATH,
     you may also need to tell your shell with e.g. `hash -r` for BASH.

  2. You don't have variables such as `GERBIL_HOME` or `GERBIL_GSI` defined
     that interfere with Nix's Gerbil installation.

In both cases, you may have to both edit your shell configuration
(such as `~/.bash_profile`, `~/.profile`, `~/.bashrc` and/or
 `~/.zsh_profile`, `~/.zshenv`, `~/.zshrc`, etc.) to add or remove the desired variable definitions,
but also `export` or `unset` the variables in the current shell.

### Fourth, install Gerbil libraries

Since you build outside Nix, you should now use `gxpkg`
to install the Gerbil libraries we use.

Assuming for now you won't be modifying their source code,
just install them with `gxpkg install` and `gxpkg build`.

```
DEPS=(github.com/fare/gerbil-utils
      github.com/fare/gerbil-crypto
      github.com/fare/gerbil-poo
      github.com/fare/gerbil-persist
      github.com/fare/gerbil-ethereum
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
you can reinstall them the following way,
after possibly editing the script to adjust the `SRCDIR` shell variable
to adjust where you'll install those dependencies:

```
DEPS=(github.com/fare/gerbil-utils
      github.com/fare/gerbil-crypto
      github.com/fare/gerbil-poo
      github.com/fare/gerbil-persist
      github.com/fare/gerbil-ethereum
      github.com/drewc/js-syntax
      github.com/drewc/gerbil-swank
      github.com/drewc/drewc-r7rs-swank) ;
SRCDIR=.. ;
(cd ${SRCDIR} &&
for i in ${DEPS[@]} ; do
  (gxpkg unlink $i > /dev/null 2>&1 ;
   git clone https://$i &&
   cd $(basename $i) &&
   gxpkg link $i $PWD &&
   gxpkg build $i $PWD)
done)
```

### Last but not least, build Glow

After you have installed dependencies as above, you should be all ready to go.
You can build a binary with:

    ./build.ss

To compile faster, you can disable optimizations
with option `--O` to command `compile`,
and on supported platforms (e.g. Linux amd64)
use [TCC](https://en.wikipedia.org/wiki/Tiny_C_Compiler) with option `-t`.
You might also or instead use option `-g` use extra debugging information.

    ./build.ss compile -t --O

## Testing Glow

Once you have built and installed Glow, you should be all ready to go.
You can run all our unit tests with:

    ./unit-tests.ss

And you can run our integration tests with:

    ./unit-tests.ss integration

However, mind that integration tests may require you
to have a local private `geth` running
(typically using the `script/run-ethereum-test-net.ss`
from [gerbil-ethereum](https://github.com/fare/gerbil-ethereum))
and may maintain state in a `run` directory
that you might have to wipe with `rm -rf run`
each time you restart `geth`, shortly *before* said restart
(since `geth` may also be storing its data there).

Note that assuming all dependencies are present and built,
you shouldn't need to build Glow as above to test it.

## Run it

Once you built the software with `./build.ss`, you can run it with `./glow`.

## Install it

If you use Nix, it will have already installed Glow.

Otherwise, you can copy the `./glow` binary anywhere on your machine.
HOWEVER, note that at the time being, the binary relies
not only on your Gambit and Gerbil installations,
but also on the compiled file in `~/.gerbil/lib/`
and so only works for the with Unix user.

Now, you can export the `GERBIL_PATH` environment variable
on the user who compiles Glow and/or on the other users who use it
to specify where the compiled files should be stored.
You may need to either copy the compiled files or re-build them
for the dependencies as well as for Glow itself, if you change the `GERBIL_PATH`.
