# Installing Glow

Instructions for installing, testing and running *Glow*.

## Easy Install

Long story short, you can install the latest stable release of *Glow*
with just use the following one-liner, and
skip reading the rest of this file (except the warnings and prerequisites):

    curl -L https://glow-lang.org/install/glow-install | sh

Alternatively, if you're rather use Docker than Nix,
you can `docker pull mukn/glow:stable` then `docker run -it mukn/glow:stable`.

## Warnings and Prerequisites

1. Currently, installing *Glow* requires about 4GB of RAM and 2GB of disk.
   You will need at least 8GB of RAM and 8GB of swap if you need to recompile it or its dependencies.
   This includes the case where you will modify *Glow* itself,
   but also the case where you run it on an architecture that doesn't have precompiled binaries.

2. We use the [Nix](https://nixos.org/nix/) package manager
   for its deterministically reproducible builds:
   if it builds and run for us, it should build and run identically for you.
   *Glow* should work on all supported architectures supported by Nix.

3. The first step of our script above makes sure that Nix is installed.
   If that wasn't the case yet, any shell started before that step was complete
   (including the one running the installation, and any previous one)
   will have the wrong `$PATH` variable.
   To use nix and our software, you will need to either re-start new shells
   or have them re-read their configuration, so they have the new `PATH`
   that includes `~/.nix-profile/bin`.
   For instance, you may close and re-open your terminal windows,
   or type the command `exec $SHELL`, or `. ~/.profile`, or
   `PATH="$HOME/.nix-profile/bin:$PATH"`, etc.

3. Our developers so far have are testing *Glow* on Linux and macOS on `x86_64`.
   Hopefully it should all work on the Window Subsystem for Linux (WSL),
   and on other architectures (such as `arm64`). But we haven't been testing it yet
   (try [this recipe](https://nathan.gs/2019/04/12/nix-on-windows/)
   if you have trouble with Nix on WSL).

4. We are using [cachix](https://cachix.org/) to distribute pre-compiled binaries
   for all our packages. But this only works if you are using one of the architectures
   our developers use, which are `x86_64-linux` and `x86_64-darwin`.
   This *should* also work on WSL on `x86_64`, but we haven't tried.

5. If you're not using one of the above architectures, then the installation will build
   not only *Glow*, but also, at least the first around, a lot of its dependencies.
   This may take a long time (hours?) and consume a lot of memory.
   Make sure you have at least 16GB of RAM+swap. Also, if you have a lot of cores,
   you might reduce the memory pressure somewhat by passing the argument `--cores 1` or such
   to `nix-env` in the second step of the script:

        nix-env --cores 1 -f https://github.com/muknio/nixpkgs/archive/devel.tar.gz \
            -iA glow-lang gerbil-unstable go-ethereum solc

6. Installing *Glow*, or any software, requires that you trust the authors and their infrastructure.
   To minimize the need for trust, and minimize the opportunity for damage
   from applications that do breach your trust,
   we recommend you use isolated virtual machines for each application.
   For this and for extra security, you might want to start using
   [Qubes OS](https://www.qubes-os.org/)
   before you start using your laptop to manipulate real assets.

*In the future*, we will improve the installation as follows:

1. We will support JavaScript as a target platform,
   and distribute pre-compiled portable "binaries" for that platform.
   This will allow *Glow* to run on Windows, iOS, Android, and on any web browser.

But for now, be prepared for the initial installation to take a lot of time,
so run it in the background and do something else while your machine compiles a lot of code.

## Simplified Installation for Users

If you mean to use *Glow* (as a developer or end-user),
but not modify the language implementation itself (compiler, runtime, libraries),
then the following command will do everything,
and you don't need the further sections of this file:

    curl -L https://glow-lang.org/install/glow-install | sh

From a checkout of this repository, you can just use:

    sh scripts/glow-install

Note that if Nix wasn't installed yet, you might have
to restart your shell and/or to logout and login,
so that the Nix-enabled `$PATH` be defined, and other environment variables with it.

## Simplified Installation for Implementers

First, use the installation script as above, or, if you're distrusting (as you should be),
audit it, trace the steps one by one, execute them manually, all in a sandbox, etc.

Then, you can checkout the code of *Glow* (if you haven't already) with

    git clone https://gitlab.com/mukn/glow

Now you can `cd glow` and build the software deterministically with:

    nix-build

Or you can use `nix-shell` to enter an environment in which to build and hack by hand.

You don't need the further sections in this file, but
you need to read the [HACKING.md](HACKING.md) file for how to hack.

## Manual installation via Nix

You can look at the above-mentioned installation scripts, and reproduce their steps manually.

## Trying it out with Docker

We have a Docker image [`mukn/glow`](https://hub.docker.com/repository/docker/mukn/glow)
with *Glow* and its associated libraries precompiled.

For the the most recent stable release (currently alpha quality), use:

    docker pull mukn/glow:stable
    docker run -it mukn/glow:stable bash

For the latest development code (has most features but can be unstable), use:

    docker pull mukn/glow:latest
    docker run -it mukn/glow:latest bash

You can build your own using `scripts/make-docker-image.ss` from
[gerbil-utils](https://github.com/fare/gerbil-utils).

## Installing *Glow* the Hard Way

If you are going to be modifying the implementation of *Glow*,
you may want to install things the hard way as below
before you look at the file [HACKING](HACKING.md) for development guidance.

If you don't use either Linux or macOS, and can't use Docker,
or if you insist on not using Nix, or on reproducing every step manually for audit purposes,
then you will have to build and install *Glow* the Hard Way:
installing GCC, on top of it Gambit Scheme, on top of it Gerbil Scheme,
on top of it a bunch of libraries, and finally, *Glow*.

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

    nix-env -f http://github.com/muknio/nixpkgs/archive/devel.tar.gz -iA gerbil-unstable

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
      github.com/fare/gerbil-poo
      github.com/fare/gerbil-crypto
      github.com/fare/gerbil-persist
      github.com/fare/gerbil-ethereum
      github.com/drewc/gerbil-swank
      github.com/drewc/drewc-r7rs-swank
      github.com/drewc/smug-gerbil
      github.com/vyzo/gerbil-libp2p) ;
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
      github.com/fare/gerbil-poo
      github.com/fare/gerbil-crypto
      github.com/fare/gerbil-persist
      github.com/fare/gerbil-ethereum
      github.com/drewc/gerbil-swank
      github.com/drewc/drewc-r7rs-swank
      github.com/drewc/smug-gerbil
      github.com/vyzo/gerbil-libp2p) ;
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

### Last but not least, build *Glow*

After you have installed dependencies as above, you should be all ready to go.
You can build a binary with:

    ./build.ss

To compile faster, you can disable optimizations
with option `--O` to command `compile`,
and on supported platforms (e.g. Linux amd64)
use [TCC](https://en.wikipedia.org/wiki/Tiny_C_Compiler) with option `-t`.
You might also or instead use option `-g` use extra debugging information.

    ./build.ss compile -t --O

## Testing *Glow*

Once you have built and installed *Glow*, you should be all ready to go.
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
you shouldn't need to build *Glow* as above to test it.

## Run it

Once you built the software with `./build.ss`, you can run it with `./glow`.

## Install it

If you use Nix, it will have already installed *Glow*.

Otherwise, you can copy the `./glow` binary anywhere on your machine.
HOWEVER, note that at the time being, the binary relies
not only on your Gambit and Gerbil installations,
but also on the compiled file in `~/.gerbil/lib/`
and so only works for the Unix user who compiled it.

Now, you can export the `GERBIL_PATH` environment variable
to point to some place other than your `~/.gerbil`
for the user who compiles *Glow*.
You can also point the `GERBIL_LOADPATH` environment variable
at a colon-separated list of source and compiled directories.
Thus, if you want to develop code using software pre-compiled by nix,
plus some source overrides from `$MYSRC/gerbil-ethereum`, you may use:
```
export GERBIL_LOADPATH=$MYSRC/gerbil-ethereum:$HOME/.nix-profile/gerbil/lib
```

To see how *Glow* is installed by its Nix package, see
[glow-lang.nix](https://github.com/MuKnIO/nixpkgs/blob/devel/pkgs/development/compilers/gerbil/glow-lang.nix)
in the `devel` branch of our [nixpkgs repo](https://github.com/MuKnIO/nixpkgs/).
