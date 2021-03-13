#lang scribble/manual

@(require "glow-code.rkt"
          "glow-docs.rkt"
          (for-label glow))

@title{Glow How-To}

@section{How to Install @(Glow)}

Currently, @(Glow) is available for installation on Linux and macOS.
To install, open a terminal and type or copy/paste the following command line:

@racketblock{
    curl -L https://glow-lang.org/install/glow-install | sh
}

This command will download about 2GB of programs and data using the
@hyperlink["https://nixos.org"]{Nix} package manager.
On Intel @racket[x86_64] it should download pre-compiled binary packages.
On other architectures, it may take quite some time to compile everything.
It may or may not work on Windows Subsystem for Linux (WSL).

In the future, we will compile @(Glow) fully to a single JavaScript file
that you can open in your browser.
