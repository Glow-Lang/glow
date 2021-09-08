# Glow

MuKn Glow is a language to write safe Decentralized Applications (DApps)
interacting with cryptocurrency blockchains.
It is designed to minimize the effort required to trust
that a DApp is indeed safe for people to use.

### Copyright and License

Copyright 2019 Mutual Knowledge Systems, Inc. All rights reserved.
Glow is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).

### Installation instructions

To install the latest stable build, you can use our install script:

    curl -L https://glow-lang.org/install/glow-install | sh

Some caveats you should take note of:
- The install script uses [`nix`](https://nixos.org/) (and installs it if it is missing).
- We distribute precompiled binaries via cachix for `x86_64-linux` and `x86_64-darwin`.
  This *should* also work on WSL on `x86_64`, but we haven't tried.
- If you aren't using one of the above architectures,
  run the installation in the background,
  as it will take a while to compile Glow and its dependencies.
  
See [INSTALL.md](INSTALL.md) for greater detail on warnings and prerequisites, as well as alternative installation steps.

### Getting started with Glow

After installing Glow,
you can get started with a tutorial in our [`Standard Library`](./dapps/README.md).

Alternatively, you can dive into the `glow-cli`:
``` sh
glow help # View all available commands
glow help <command> # View help for a command
```

The full, in-depth tutorial may be found [here](https://glow-lang.org/docs/Glow_Tutorial.html).

### Being worked on

You can watch on our [gitlab repository](https://gitlab.com/mukn/glow)
what we are currently working on.

### More information

For more information on Glow, see our web site
[glow-lang.org](https://glow-lang.org), including
[our documentation](https://glow-lang.org/docs).

If you are a developer, you may also be interested in
[our wiki](https://gitlab.com/mukn/glow/-/wikis/home), including
[our Roadmap](https://gitlab.com/mukn/glow/-/wikis/Roadmap).
