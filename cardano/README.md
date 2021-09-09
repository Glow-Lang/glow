# Gerbil-Cardano

Gerbil-Cardano is a package to interact
with the [Cardano network](https://cardano.org/)
from [Gerbil Scheme](https://cons.io).
It is a layer on top of Plutus, providing various abstractions notably used by the Glow language.
It is not at all a reimplementation any part of the Cardano protocol,
and relies on an existing node to implement the Cardano protocol.

### Copyright and License

Copyright 2020 Mutual Knowledge Systems, Inc. All rights reserved.
Gerbil-Cardano is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).

### What does Gerbil Cardano do?

Gerbil-Cardano provides an interface to a Cardano node, but
with Gerbil Scheme as the underlying language platform, rather than Haskell.
It provides runtime infrastructure both for DApp clients and for contracts written in Plutus Core,
that is usable as a backend for the Glow language.

The current version of Gerbil Cardano uses the Plutus Application Framework.


### What does Gerbil Cardano NOT do yet?

Gerbil-Cardano is doing its best to demo the semantics of Glow on top of Plutus,
but to fully implement a robust DApp platform, we would need the following APIs from Cardano,
that are not yet available:

  - Preparing a transaction without posting it, so we can persist it
    before we post it. This is slightly less important if we have a 100%
    deterministic way to prepare the transaction (e.g. guaranteed no
    random number while generating signatures and/or we can provide the
    PRNG seed and/or all the random numbers). This is also less important
    if Plutus Application Framework already deals with robust replicated persistence.

  - Posting a set of transactions — multiple ones

  - Watching a set of transactions — see which ones are at which stage
    of acceptance or rejection.

  - Watching all transactions on a set of UTXOs

  - Compiling code to Plutus Core

  - Applying a compiled plutus program to a set of parameters

  - Decoding in a transaction which parameters were given to a compiled
    Plutus program.

  - The ability to generate some linear non-fungible token to
    unambiguously track state.


### Why use Gerbil Cardano?

* *Better Language, Simpler Code*: Gerbil Cardano can achieve functionality similar
  to that of libraries in other languages at a fraction of the complexity,
  because the underlying language is more *expressive*.
  First-class *continuations* and threads enable you to directly express in Scheme
  massively concurrent computations that require jumping through many hoops in other languages,
  such as async functions, monads, trampolines, thread pools, factory factories, etc.
  Scheme's unique ability for *syntactic abstraction* not only makes the code much more succint,
  but also much safer, by protecting the users from errors introduced by developers when they manually
  follow "design patterns", or fail to follow them, especially as they subsequently evolve their code.
  Using *prototype object orientation* for type descriptors,
  we can leverage both ad-hoc and parametric polymorphism to factor our code into
  small pieces of incremental functionality that nicely build on each other,
  rather than large monolithic pieces of code that constantly repeat each other.
  In the end, that means not only much less code to write, but
  even more importantly for cryptocurrency software, much less code to *audit*.

* *Safer Programming Model*: with Gerbil Cardano, we are creating a programming model
  for building decentralized applications that are safe by construction.
  Our current code base does not yet offer builtin protection against all the attacks
  that we are thinking of preventing, yet the programming model it offers is already
  significantly more robust than that offered by Plutus alone.
  For instance, we take persist-before-messaging discipline seriously,
  with persistent activities, proper transactionality, and in the future distributed replication.
  Like Cardano itself, we build and test our code with
  [deterministic build tools](https://www.nixos.org/nix/)
  that ensure that if it works for us, it will work for everyone.

* *Potential Portability*: Gerbil-Cardano is part of a family of libraries written in Gerbil
  to support all cryptocurrencies. If you only plan to support Cardano, and don't need the
  abstractions we are building, then you can use Plutus directly, in Haskell. But if you want
  to write software that is portable to many cryptocurrencies, Gerbil-Cardano makes sense.
  What more, Gerbil-Cardano also potentially brings portability to more deployment platforms:
  Gerbil itself is built on top of Gambit, that has backends to any platform that matters,
  and could be made to target any future platform that would.
  In addition to the C platform, it can target JavaScript, Java, PHP, Python, Ruby, and
  various popular microprocessor architectures. While we haven't yet taken advantage of
  this portability with Gerbil Cardano, your odds at building fully audited cryptocurrency software
  that runs on top of any of these platforms, including JavaScript, are higher
  if you use a few hundreds of lines of Gambit Scheme to retarget Gerbil Cardano
  than if you use any JavaScript platform and start auditing millions of lines of JavaScript,
  or some similar situation with another "blub" language.
  
### Running the Demo interaction: Buy Signature

1. Run the PAB backend

  ```
  cd haskell
  nix-shell
  cabal run glow-contract
  ```
  
2. Run the Glow interaction

  From **project root** run:
  ``` sh
  ./cardano/script/execute-contract.ss
  ```

### Developing

Run `nix-shell` to enter a shell with the following executables:
- `run-testnet-node` for starting a local node running on the Cardano test network
- `run-wallet` for starting a local wallet connected to a local node
- `nix-thunk` for unpacking and unpacking dependencies in the dep folder (e.g., `nix-thunk unpack dep/cardano-node`)

A faucet for the test network is available at: https://testnets.cardano.org/en/cardano/tools/faucet/

### Bibliography

- Plutus platform: https://hydra.iohk.io/build/3793507/download/1/plutus.pdf
- Plutus Core specification: https://hydra.iohk.io/build/3877005/download/1/plutus-core-specification.pdf
- Extended UTXO model: https://hydra.iohk.io/build/3793505/download/1/extended-utxo-specification.pdf
- Cardano node specification: https://hydra.iohk.io/job/Cardano/ouroboros-network/native.docs.x86_64-linux/latest
