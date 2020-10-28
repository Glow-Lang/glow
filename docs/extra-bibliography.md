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
* [Nanopass compilers](https://nanopass.org/documentation.html), and [this example](https://github.com/akeep/scheme-to-c/blob/master/c.ss), [this course](https://iucompilercourse.github.io/IU-P423-P523-E313-E513-Fall-2020/) with [video lectures](https://iu.mediaspace.kaltura.com/media/Compiler+Course%2C+September+1%2C+2020/1_7o6702no)
* [Urlang](https://github.com/soegaard/urlang), compiling from SEXP syntax with macros to JavaScript as a Racket `#lang` using a nanopass compiler.
* Not to be confused with Chlipala's [Ur](http://www.impredicative.com/ur/) is a very different language, that supports metaprogramming using row types.
* Daniel Patterson on [proving a compiler fully abstract](https://dbp.io/essays/2018-04-19-how-to-prove-a-compiler-fully-abstract.html)
* Abstract Abstract Machines, Virtual Virtual Machines...
* [Language Oriented Programming in Racket, a cultural anthropology](https://gumroad.com/l/lop-in-racket-cultural-anthro)

### Debugging

* Omniscient Debugging: [Pernosco](https://pernos.co/about/related-work/), [rr](https://rr-project.org), TTD, UndoDB, ODB, Chronon, TOD, Chronomancer, Qira, [Reven](https://www.tetrane.com/), SteamDrill.

### Persistence of Long-Running Activities

* [Sagas](https://dl.acm.org/doi/10.1145/38713.38742).
  Daniel Yokomizo says there are plenty of papers, blogs, videos, implementations, etc., about Sagas.
* [Spores](https://infoscience.epfl.ch/record/191239)
* [nippy (de)serializing Clojure fns](https://tech.redplanetlabs.com/2020/01/06/serializing-and-deserializing-clojure-fns-with-nippy/)
* prior art on serializing continuations in Scheme (Tube, Dreme, [ok](http://okmij.org/ftp/continuations/#shift-cgi), ...), Smalltalk (Seaside), and CL (uncommonweb, dwim.hu...)

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

### Blockchain 101

* [A Beginner's Guide To Blockchain](https://blockchaingirls.org/beginners-guide)
* Adversariality: [Ethereum is a Dark Forest, by Dan Robinson and Georgios Konstantopoulos](https://medium.com/@danrobinson/ethereum-is-a-dark-forest-ecc5f0505dff)

### Proofs for Blockchain

* Algorand proof of correctness and controversy:
  [announce by runtime verification](https://runtimeverification.com/blog/formally-verifying-algorand-reinforcing-a-chain-of-steel-modeling-and-safety/),
  [medium article](https://medium.com/algorand/formal-verification-of-the-algorand-protocol-bbde5a52b830).
  [Counter-example](https://arxiv.org/abs/1905.04463),
  [also this](https://arxiv.org/pdf/1907.05523.pdf).

### Proof Systems

* [Lean](https://leanprover.github.io/theorem_proving_in_lean/)
* CakeML, and [Ramana Kumar's thesis](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-879.html)

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

* Algorithmic Complexity 102: [user feedback is an instant O(1)]()
* [How Complex Systems Fail](https://web.mit.edu/2.75/resources/random/How%20Complex%20Systems%20Fail.pdf)

### Version Control

* Git
* [Fossil](https://fossil-scm.org/home/doc/trunk/www/index.wiki) by the SQLite author.

### Smart Contract Vulnerabilities

* [Smart Contract Weakness Classification and Test Cases](https://swcregistry.io/)

### Machine Learning

* [100 Must-Read NLP Papers](https://github.com/mhagiwara/100-nlp-papers)

## Blockchain Projects to Integrate With

### Kadena

* [Public Chain Interaction](https://kadena-io.github.io/kadena-docs/Public-Chain-Docs/)
* [wallet signing api](https://kadena-io.github.io/signing-api/)
* [nodes](https://github.com/kadena-io/chainweb-node)

### Trezor (Hard Wallet)

* [Trezor](https://wiki.trezor.io/Developers_guide)

### Cryptographic primitives

* multisig: Schnorr signatures, BLS signatures
  + Q: can we efficiently reshuffle a *same* key, such that the old committee
    has no advantage with the new committee?

* [Time-lock encryption](https://www.gwern.net/Self-decrypting-files)

### Optimistic Roll-Up

* [Optimism](https://medium.com/ethereum-optimism/optimism-cd9bea61a3ee)

### More...

* Ledger
* Substrate
