# Bibliography about other Relevant Technology

This document has plenty of pointers to other technology outside of Glow.
Mostly useful for people implementing or designing Glow,
or who want to understand the wider context.

### Types in Lisp

* [Coalton](https://github.com/stylewarning/coalton), a dialect of ML embedded in Common Lisp
* [Shen](http://shenlanguage.org/), a Lisp dialect in which you write your own type system with sequent calculus
* [Cur](https://docs.racket-lang.org/cur/index.html), a Racket language with dependent types
* [Typed Racket](https://docs.racket-lang.org/ts-reference/index.html), a Racket language with types
* [Hackett](https://lexi-lambda.github.io/hackett/), a Racket language like Haskell with macros
* [LispNYC presentation on dependent types in 100 lines of Racket by Gershom Bazerman](https://www.meetup.com/LispNYC/events/217730802/)
* [Stuck Macros, by Samuel Gélineau](https://www.youtube.com/watch?v=nUvKoG_V_U0)
* [Rosette](https://docs.racket-lang.org/rosette-guide/), solver-aided programming for Racket

Other non-Lisp types of interest?
* [Cedille](https://cedille.github.io/), with its "extrinsic" (Curry-style) types as annotations
  to untyped terms (vs intrinsic / Church style as parts of the term).
* LEAN, Idris, Coq, Agda, F*, ATS, NuPRL, Isabelle, HOL, Epigram, Cayenne.

### Compilers

* [Jay McCarthy's course on compiler construction](https://jeapostrophe.github.io/courses/2019/spring/406/course/)
* [Nanopass compilers](https://nanopass.org/documentation.html), and [this example](https://github.com/akeep/scheme-to-c/blob/master/c.ss)
* [Urlang](https://github.com/soegaard/urlang), compiling from SEXP syntax with macros to JavaScript as a Racket #lang using a nanopass compiler.
* Not to be confused with Chlipala's [Ur](http://www.impredicative.com/ur/) is a very different language, that supports metaprogramming using row types.
* Daniel Patterson on [proving a compiler fully abstract](https://dbp.io/essays/2018-04-19-how-to-prove-a-compiler-fully-abstract.html)

### Debugging

* Omniscient Debugging: [Pernosco](https://pernos.co/about/related-work/), [rr](https://rr-project.org), TTD, UndoDB, ODB, Chronon, TOD, Chronomancer, Qira, [Reven](https://www.tetrane.com/), SteamDrill.

### Persistence of Long-Running Activities

* [Sagas](https://dl.acm.org/doi/10.1145/38713.38742).
  Daniel Yokomizo says there are plenty of papers, blogs, videos, implementations, etc., about Sagas.

### Structured Interactive Development

* [Hazel](https://hazel.org/)
* [ProjecturEd](https://github.com/projectured/projectured)
* [Cirru](http://cirru.org)
* [Lamdu](https://lamdu.org)
* [Luna (DAGs)](https://luna-lang.org)
* [Pure Data (DAGs)](https://puredata.info)

### Deployment

* [Deploying container and application services with Disnix](https://sandervanderburg.blogspot.com/2020/04/deploying-container-and-application.html)
* [Arion: Docker compose with NixOS images](https://github.com/hercules-ci/arion)

### Game Theory

* [Jules Hedges's compositional game theory bibliography](https://julesh.com/compositional-game-theory-bibliography/), including [The game semantics of game theory](https://arxiv.org/abs/1904.11287).

### Help with Gerbil Scheme

* [GitHub Workflows CI for Gerbil](https://github.com/belmarca/gerbil-fwd/blob/master/.github/workflows/main.yml)
* For complex information propagation during type analysis, ekmett suggests Rete + propagators?

### Proofs for Blockchain

* Algorand proof of correctness and controversy:
  [announce by runtime verification](https://runtimeverification.com/blog/formally-verifying-algorand-reinforcing-a-chain-of-steel-modeling-and-safety/),
  [medium article](https://medium.com/algorand/formal-verification-of-the-algorand-protocol-bbde5a52b830).
  [Counter-example](https://arxiv.org/abs/1905.04463),
  [also this](https://arxiv.org/pdf/1907.05523.pdf).

### Proof Systems

* [Lean](https://leanprover.github.io/theorem_proving_in_lean/)

### Issues with Solidity

* [Consensys: known attacks](https://consensys.github.io/smart-contract-best-practices/known_attacks/)

### Non-blockchain Languages to learn from

* [Fugue](https://fugue.co), a proprietary alternative to Hashicorp's [Terraform](https://terraform.io), puts engineers in command of cloud security with tools for visualizing resources and changes over time.
* [dhall](https://dhall-lang.org), a strongly typed terminating programmable configuration language that you can think of as: JSON + functions + types + imports.
* [Hazel](https://hazel.org), a live functional language programming environment with typed holes.
* [Dark](https://darklang.com), a holistic programming language, editor, and infrastructure for building backends without accidental complexity.
* [Unison](https://unisonweb.org), an open source functional programming language based on a simple idea with big implications: code is content-addressed and immutable.
* [Pony](https://www.ponylang.io/), an open-source, object-oriented, actor-model, capabilities-secure, high-performance programming language.

### Complexity

* [How Complex Systems Fail](https://web.mit.edu/2.75/resources/random/How%20Complex%20Systems%20Fail.pdf)

### Smart Contract Vulnerabilities

* [Smart Contract Weakness Classification and Test Cases](https://swcregistry.io/)

## Blockchain Projects to Integrate With

### Kadena

* [Public Chain Interaction](https://kadena-io.github.io/kadena-docs/Public-Chain-Docs/)
* [wallet signing api](https://kadena-io.github.io/signing-api/)
* [nodes](https://github.com/kadena-io/chainweb-node)

### Trezor (Hard Wallet)

* [Trezor](https://wiki.trezor.io/Developers_guide)

