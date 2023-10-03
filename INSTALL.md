# Installing Glow

Instructions for installing, testing and running *Glow*.

## Easy Install

### One-Liner on Linux, macOS, WSL

You can install *Glow* with this one-liner:

    curl -L https://glow-lang.org/install/glow-install | sh

This will make the command `glow` available to you, though you may have
to restart your shell, or copy/paste a configuration command that will
be suggested. The installation script requires `curl`, `sudo`, and `xz`.

### Alternative one-liner using Docker

If you'd rather use Docker than Nix, you can run:

    docker pull mukn/glow:latest

The command `glow` is available in that image,
but see [below](#Trying-it-out-with-Docker) on how to invoke it.

## Warnings and Prerequisites

1. Installing *Glow* currently requires about 4GB of RAM and about
   2GB of free disk space.

2. We use the [Nix](https://nixos.org/nix/) package manager for its
   deterministic, reproducible builds. If it builds and runs for us,
   it should build and run identically for you.

3. Nix installs its packages under `/nix`. If you are on a machine with
   a small root partition, *Glow* and its dependencies may be too big.
   You can get around this on Linux by using a bind-mount, e.g.:

        sudo mount --bind /big/partition/nix /nix

   where `/big/partition/nix` is some directory on a filesystem with
   enough free space.

4. The first step of our script makes sure that Nix is installed.
   If it wasn't already, any shell started before that step was complete
   (including the one running the installation, and any previous ones)
   will have the wrong value for the `$PATH` variable. To use nix and
   our software, you will need to either re-start your shells or have
   them re-read their configuration, so they have a new `PATH` that
   includes `~/.nix-profile/bin`. For instance, you may close and re-open
   your terminal windows, or run any of the commands `exec $SHELL`,
   or `. ~/.profile`, or `PATH="$HOME/.nix-profile/bin:$PATH"`, etc.

5. We use [cachix](https://cachix.org/) to distribute pre-compiled binary
   packages, but currently only for Linux and macOS on `x86_64`.

6. If you're not using one of those architectures, then the installation
   script will build not only *Glow*, but also many of its dependencies.
   This may take a long time (hours) and consume a lot of memory;
   make sure you have at least 16GB of RAM+swap. If you have a lot of
   cores, you might pass the argument `--cores 4` or such to `nix-env`
   in the second step of the script:

        nixpkgs=https://github.com/muknio/nixpkgs/archive/devel.tar.gz
        nix-env --cores 4 -f $nixpkgs -iA glow-lang gerbil-unstable go-ethereum solc

   The build process *should* work on architectures like WSL on `x86_64`
   and Linux on `arm64`, but we haven't tried them; please let us know
   if you do!

## Simplified Installation for Users

If you mean to use *Glow* as a developer or end-user
but not modify the language implementation itself (compiler, runtime, libraries),
then the following command will do everything,
and you don't need the further sections of this file:

    curl -L https://glow-lang.org/install/glow-install | sh

From a checkout of this repository, you can use:

    sh scripts/glow-install

Note that if Nix wasn't installed yet, you might have to restart your shell
and/or to logout and login, so that the Nix-enabled `$PATH` can be defined,
and other environment variables with it.

## Simplified Installation for Implementers

First, use the installation script as above, or, if you're distrusting (as you should be),
audit it, trace the steps one by one, execute them manually, all in a sandbox, etc.

Then, you can checkout the code of *Glow* (if you haven't already) with

    git clone https://github.com/Glow-lang/glow

Now you can `cd glow` and build the software deterministically with:

    nix-build

Or you can use `nix-shell` to enter an environment in which to build and hack by hand.

You don't need the further sections in this file, but
you need to read the [HACKING.md](HACKING.md) file for how to hack.

## Manual installation via Nix

You can look at the above-mentioned installation scripts, and reproduce their steps manually.

To install our latest development version `devel`, instead of the `alpha` version,
you can point the `nixpkgs` near the beginning of the file to this alternate location:

    nixpkgs=https://github.com/muknio/nixpkgs/archive/devel.tar.gz

Or, once Nix is installed, you can define this `nixpkgs` variable, then run

    nix-env -f $nixpkgs -iA glow

In the future, we will also have a `stable` version.

<a name="Trying-it-out-with-Docker" />
## Trying it out with Docker

We have a Docker image [`mukn/glow`](https://hub.docker.com/repository/docker/mukn/glow)
with *Glow* and its associated libraries precompiled.

For the the most recent stable release (currently alpha quality), use:

    docker pull mukn/glow:latest
    docker run -it --mount type=volume,src=glow-home,dst=/root mukn/glow:latest bash

For the latest development code (which may have more features, but
is better suited to work on *Glow* itself and can be unstable), use:

    docker pull mukn/glow:devel
    docker run -it --mount type=volume,src=glow-home,dst=/root mukn/glow:devel bash

You can build your own using `scripts/make-docker-image.ss` from
[gerbil-utils](https://github.com/mighty-gerbils/gerbil-utils).

Note that the `--mount` option is there to help you persist your working state
between sessions of running *Glow* inside docker.
Without such a mount, you'd lose your keys, etc., between sessions.
Alternatively, you could use a `bind` mount to put those files directly in your filesystem
(consult the Docker documentation for details),
but beware what it would do to file permissions considering that
the processes under `docker` run as `root` by default.
You can layer your own modifications on top of the docker image to suit your needs.

## Installing *Glow* the Hard Way

If you are going to be modifying the implementation of *Glow*,
you may want to install things the hard way as below.
This installs the same dependencies as those in [the easy way](INSTALL.md#easy-install),
but also installs source code you can modify.

After that you can look at the file [HACKING](HACKING.md) for development guidance.

If you don't use either Linux or macOS, and can't use Docker,
or if you insist on not using Nix, or on reproducing every step manually for audit purposes,
then you will have to build and install *Glow* the Hard Way:
installing GCC, on top of it Gambit Scheme, on top of it Gerbil Scheme,
on top of it a bunch of libraries, and finally, *Glow*.

Beware that the version of Gambit and/or Gerbil that you might install
from your Linux distribution's package manager or from [homebrew](https://brew.sh/) on macOS
might *not* be recent enough version to build *Glow*.

### First, install GCC

I'll assume you know how this works.
GCC probably comes with your Operating System or software distribution.
On Windows, you may have to use WSL or [MinGW](http://mingw.org/).

Note that the LLVM's Clang won't do: we use Gambit Scheme,
which reportedly compiles ten times (10x) slower with Clang than with GCC,
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
You will need a recent development version; release 0.16 is not enough.

We recommend you use Nix with our own repo and branch of nixpkgs,
so you get a known-working recent version:

    nixpkgs=https://github.com/muknio/nixpkgs/archive/devel.tar.gz
    nix-env -f $nixpkgs -iA gerbil-unstable

You could try to install Gerbil via your Linux distribution's package manager
or via macOS's [homebrew](https://brew.sh/), but it is likely not to provide
a recent enough version of Gerbil to build *Glow*, at which point you will have
to build from source.

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
DEPS=(github.com/mighty-gerbils/gerbil-leveldb
      github.com/mighty-gerbils/gerbil-utils
      github.com/mighty-gerbils/gerbil-poo
      github.com/mighty-gerbils/gerbil-crypto
      github.com/mighty-gerbils/gerbil-persist
      github.com/mighty-gerbils/gerbil-ethereum
      github.com/drewc/gerbil-swank
      github.com/drewc/drewc-r7rs-swank
      github.com/drewc/smug-gerbil
      github.com/drewc/ftw
      github.com/vyzo/gerbil-libp2p
      ) ;
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
DEPS=(github.com/mighty-gerbils/gerbil-leveldb
      github.com/mighty-gerbils/gerbil-utils
      github.com/mighty-gerbils/gerbil-poo
      github.com/mighty-gerbils/gerbil-crypto
      github.com/mighty-gerbils/gerbil-persist
      github.com/mighty-gerbils/gerbil-ethereum
      github.com/drewc/gerbil-swank
      github.com/drewc/drewc-r7rs-swank
      github.com/drewc/smug-gerbil
      github.com/drewc/ftw
      github.com/vyzo/gerbil-libp2p
      ) ;
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

### Fifth, install Go-Ethereum

[`geth`](https://geth.ethereum.org/) is necessary to run integration tests.

For Nix users (recommended):
```sh
nixpkgs=https://github.com/muknio/nixpkgs/archive/devel.tar.gz
nix-env -f $nixpkgs -iA go-ethereum
```

The `nixpkgs` archive above ensures you have a `geth` matching the one used in development.

For non-Nix users,
other installation steps can be found [here](https://geth.ethereum.org/downloads/).

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

However, mind that integration tests require you
to have a local private [`geth`](https://geth.ethereum.org/) running
(typically using the [`script/run-ethereum-test-net.ss`](https://github.com/mighty-gerbils/gerbil-ethereum/blob/master/scripts/run-ethereum-test-net.ss)
from [gerbil-ethereum](https://github.com/mighty-gerbils/gerbil-ethereum))
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
