# Installing Glow

Instructions for compiling and installing Glow, testing and running it.

## Simplified Installation via Nix

Assuming you are using Linux or macOS,
the recommended way to install Glow is with [Nix](https://nixos.org/nix/).
Nix will build and install Glow, deterministically, based on your nixpkgs.

If you're not using Nix yet, go [Get Nix](https://nixos.org/nix/),
then configure it to use the [same repo as we do](https://github.com/fare-patches/nixpkgs),
and the same branch `fare` (note that these will change when we have stable releases):

    export NIX_PATH=nixpkgs=http://github.com/fare-patches/nixpkgs/archive/fare.tar.gz

Once you have configured Nix, you can build and install Glow
together with a coherent set of its dependencies, from source, yet with a binary cache,
by running this command from the current directory:

    nix-build

## Trying it out with Docker

When we have a stable release, we'll directly provide an image `mukn/glow:stable` on
[Docker](https://hub.docker.com/repository/docker/mukn/glow).

In the meantime, you can build your own with:

    docker build -t mukn/glow -f deploy/Dockerfile .

## Install it the hard way

If you don't use either Linux or macOS, and can't use Docker,
you will have to build and install Glow the hard way:
installing GCC, on top of it Gambit Scheme, on top of it Gerbil Scheme, on top of it Glow.

On some supported platforms, e.g. [Nix](https://nixos.org/nix/) on Linux and macOS,
or [homebrew](https://brew.sh/) on macOS, you can skip directly to the third step.
On other platforms, you'll have to start at square one.

### First, install GCC

I'll assume you know how this works.
GCC probably comes with your Operating System or software distribution.
On Windows, you may have to use [MinGW](http://mingw.org/).

Note that the LLVM's Clang won't do: we use Gambit Scheme,
which compiles ten times (10x) slower with Clang than with GCC,
producing code that runs three times (3x) slower.

### Second, install Gambit Scheme

The [Gambit Scheme](http://gambitscheme.org/) compiler
is already available on a lot of operating systems and Linux distributions.
Go grab a binary package if you can.
If you can't, get its source code and compile it according to its instructions.
You will need release 4.9.3 or later.

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

### Fourth, install Gerbil libraries

You can now install the Gerbil libraries we use.
Assuming for now you won't be modifying their source code,
just install them with `gxpkg`:

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

### Last but not least, build Glow

After you have installed dependencies as above, you should be all ready to go.
You can build a binary with:

    ./build.ss

## Testing Glow

Once you have built and installed Glow, you should be all ready to go.
You can run all our unit tests with:

    ./unit-tests.ss

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
