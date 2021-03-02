# Bibliography about other Relevant Technology

"You can save hours reading papers with months of hard work on your project."

This document has plenty of pointers to other technology outside of Glow.
Mostly useful for people implementing or designing Glow,
or who want to understand the wider context.

TODO: Move everything to the wiki???

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
* [Caledon](https://github.com/mmirman/caledon), [Habit](http://www.habit-lang.org), [Tyrade](https://github.com/willcrichton/tyrade)
* LEAN, Idris, Coq, Agda, F*, ATS, NuPRL, Isabelle, HOL, Epigram, Cayenne.

### Compilers

* [Jay McCarthy's course on compiler construction](https://jeapostrophe.github.io/courses/2019/spring/406/course/)
* [Nanopass compilers](https://nanopass.org/documentation.html), and [this example](https://github.com/akeep/scheme-to-c/blob/master/c.ss) or [that one](https://github.com/akeep/scheme-to-llvm), [this course](https://iucompilercourse.github.io/IU-P423-P523-E313-E513-Fall-2020/) with [video lectures](https://iu.mediaspace.kaltura.com/media/Compiler+Course%2C+September+1%2C+2020/1_7o6702no)
* [Urlang](https://github.com/soegaard/urlang), compiling from SEXP syntax with macros to JavaScript as a Racket `#lang` using a nanopass compiler.
* Not to be confused with Chlipala's [Ur](http://www.impredicative.com/ur/) is a very different language, that supports metaprogramming using row types.
* Daniel Patterson on [proving a compiler fully abstract](https://dbp.io/essays/2018-04-19-how-to-prove-a-compiler-fully-abstract.html)
* Abstract Abstract Machines, Virtual Virtual Machines...
* [Language Oriented Programming in Racket, a cultural anthropology](https://gumroad.com/l/lop-in-racket-cultural-anthro)

### Other Lisp code of interest

* [Nyxt browser](https://github.com/atlas-engineer/nyxt)

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
* [Graham Christensen: erase your darlings](https://grahamc.com/blog/erase-your-darlings)
* [Cachix](https://cachix.org/) (including [our cache](https://app.cachix.org/cache/mukn))
* See also [Fugue](https://fugue.co), [Darklang](https://darklang.com).

### Game Theory

* [Jules Hedges's compositional game theory bibliography](https://julesh.com/compositional-game-theory-bibliography/), including [The game semantics of game theory](https://arxiv.org/abs/1904.11287).

### Help with Gerbil Scheme

* [GitHub Workflows CI for Gerbil](https://github.com/belmarca/gerbil-fwd/blob/master/.github/workflows/main.yml)
* For complex information propagation during type analysis, ekmett suggests Rete + propagators?

### Blockchain 101

* [A Beginner's Guide To Blockchain](https://blockchaingirls.org/beginners-guide) (also at [she256](https://she256.org/guide/))
* Adversariality: [Ethereum is a Dark Forest, by Dan Robinson and Georgios Konstantopoulos](https://medium.com/@danrobinson/ethereum-is-a-dark-forest-ecc5f0505dff)

### Proofs for Blockchain

* [A Survey of Smart Contract Formal Specification and Verification](https://arxiv.org/pdf/2008.02712.pdf)

* Algorand proof of correctness and controversy:
  [announce by runtime verification](https://runtimeverification.com/blog/formally-verifying-algorand-reinforcing-a-chain-of-steel-modeling-and-safety/),
  [medium article](https://medium.com/algorand/formal-verification-of-the-algorand-protocol-bbde5a52b830).
  [Counter-example](https://arxiv.org/abs/1905.04463),
  [also this](https://arxiv.org/pdf/1907.05523.pdf).

* [Noise Explorer](https://symbolic.software/noiseexplorer.html)

* [Modularity for Decidability of Deductive Verification with Applications to Distributed Systems](https://www.cs.tau.ac.il/~odedp/modularity-for-decidability.pdf), a good approach to proving correctness of complex distributed systems.
* [Type System for Resource Bounds with Type-Preserving Compilation and Its Application for Ethereum Smart Contracts](https://www.csail.mit.edu/event/type-system-resource-bounds-type-preserving-compilation-and-its-application-ethereum-smart) can prove bounds on gas usage in some EVM contracts.
* [Formal Specification and Verification of Smart Contracts in Azure Blockchain](https://arxiv.org/pdf/1812.08829.pdf) uses model checking to find and fix bugs in simple Solidity contracts.

### Proof Systems

* [Lean](https://leanprover.github.io/theorem_proving_in_lean/)
* CakeML, and [Ramana Kumar's thesis](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-879.html)

### Issues with Solidity

* [Consensys: known attacks](https://consensys.github.io/smart-contract-best-practices/known_attacks/)

### Non-blockchain Languages to learn from

* [Fugue](https://fugue.co), a proprietary alternative to Hashicorp's [Terraform](https://terraform.io), puts engineers in command of cloud security with tools for visualizing resources and changes over time.
* [dhall](https://dhall-lang.org), a strongly typed terminating programmable configuration language that you can think of as: JSON + functions + types + imports.
* [Hazel](https://hazel.org), a live functional language programming environment with typed holes.
* [Dark](https://darklang.com), a holistic programming language, editor, and infrastructure for building backends without accidental complexity ([impressions](https://dev.to/syntaxseed/darklang-intro-and-first-impressions-46oi)).
* [Unison](https://unisonweb.org), an open source functional programming language based on a simple idea with big implications: code is content-addressed and immutable.
* [Pony](https://www.ponylang.io/), an open-source, object-oriented, actor-model, capabilities-secure, high-performance programming language.
* [Factor](https://factorcode.org), a practical concatenative language with interactive development capabilities.

### Blockchain languages to watch

* [Plutus](https://github.com/input-output-hk/plutus) from IOHK
* [Marlowe](https://alpha.marlowe.iohkdev.io/tutorial/index.html)
* Blockly from IOHK
* Pact
* [Aquamarine](https://github.com/fluencelabs/aquamarine) (see its [demo](https://github.com/fluencelabs/aqua-demo))
* StateBox
* Libra's MOVE
* [Reach](https://docs.reach.sh/index.html)
* Rho-lang
* [LLL](https://lll-docs.readthedocs.io/en/latest/lll_introduction.html)
* See more in [this file](https://github.com/AlacrisIO/alacrity/blob/master/archive/competition.md)
* Tooling for Solidity: HardHat.org, truffle, etc. [template](https://github.com/paulrberg/solidity-template)

### Reflection for Security

* [Auditors](http://www.erights.org/elang/kernel/auditors/)

### Complexity

* Algorithmic Complexity 102: [user feedback is an instant O(1)]()
* [How Complex Systems Fail](https://web.mit.edu/2.75/resources/random/How%20Complex%20Systems%20Fail.pdf)

### Version Control

* Git
* [Fossil](https://fossil-scm.org/home/doc/trunk/www/index.wiki) by the SQLite author.

### Smart Contract Vulnerabilities

* [Smart Contract Weakness Classification and Test Cases](https://swcregistry.io/)

### Smart Contract Background Information

* [Smart Contracts History](https://infominer.id/bitcoin-history/smart-contracts/#smart-contract-history)
  is a resource with lots of information about the design of languages to specify "smart contracts",
  not all of them backed by a blockchain (indeed, going back to the 1960s).
* Works by [Fritz Henglein](http://hjemmesider.diku.dk/~henglein/).

### Machine Learning

* [100 Must-Read NLP Papers](https://github.com/mhagiwara/100-nlp-papers)

## Blockchain Projects to Integrate With

### Kadena

* [Public Chain Interaction](https://kadena-io.github.io/kadena-docs/Public-Chain-Docs/)
* [wallet signing api](https://kadena-io.github.io/signing-api/)
* [nodes](https://github.com/kadena-io/chainweb-node)

### Fluence

* [Fluence](https://fluence.network/)

### Hard Wallet

* [Ledger](https://www.ledger.com/)
* [Trezor](https://wiki.trezor.io/Developers_guide), distant second
* [KeepKey](https://shapeshift.com/keepkey), distant third

### Soft Wallets and chats

* [Status](https://status.im/developer_tools/)
* [MetaMask](https://metamask.io/)
* [Telegram](https://telegram.org/)
* [Taquito](https://tezostaquito.io/), a Tezos wallet, automatically generates Michelson + JavaScript from various languages.

### YubiKey

* [Purse](https://github.com/drduh/Purse)


### Cryptographic primitives

* multisig: Schnorr signatures, BLS signatures
  + Q: can we efficiently reshuffle a *same* key, such that the old committee
    has no advantage with the new committee?

* [Time-lock encryption](https://www.gwern.net/Self-decrypting-files)

* SNARKs, e.g. from O(1)labs, ZoKrates, Zoracles, Zcash, ...

* STARKs, e.g. from [Starkware](https://starkware.co/)

### Optimistic Roll-Up

* [Optimism](https://medium.com/ethereum-optimism/optimism-cd9bea61a3ee) [docs](https://docs.optimism.io/)

### DEX

* [Saturn](https://www.saturn.network/blog/what-is-saturn-network/) ([git](https://github.com/saturn-network/))
* [Paraswap](https://www.paraswap.io/)
* [Serum, DEX on Solana](https://projectserum.com/serum_white_paper.pdf)

### State Channels

* Cardano's Hydra
* [Connext](https://github.com/connext/vector/tree/main/modules/contracts/src.sol)

### Some toolkits

* [Ethereum build tools](https://ethereum.org/en/developers/learning-tools/)
* [Blockstack](https://www.blockstack.org/)
* Parity: the [Substrate](https://www.parity.io/substrate/) framework, and
  Polkadot and its [parachains](https://wiki.polkadot.network/docs/en/learn-parachains);

### Documentation

* [The documentation system, by David Laing](https://documentation.divio.com/)

### Visualization

* [ggplot](https://ggplot2.tidyverse.org/) and the grammar of graphics for statistical data

### Consensus Algorithms

* Improve performance with [Compartmentalization](https://mwhittaker.github.io/publications/compartmentalized_paxos.html) (see [this summary](https://twitter.com/judofyr/status/1288929031816204294))
* [Self-Healing](https://eprint.iacr.org/2020/1021.pdf)
* [Ledger Design Language: Designing and Deploying Formally Verified Public Ledgers](https://eprint.iacr.org/2018/416.pdf) --- a meta-language to develop proven-by-construction consensus.

### Zero-Knowledge Proofs

* [zkps in 2019](https://www.theblockcrypto.com/post/52004/look-there-where-emerging-use-cases-for-zkps-in-2019)

### Bad papers

* [Impossibility of the Obama-Trump contract](https://eprint.iacr.org/2018/252.pdf) - this article already assumes a successful 51% attack, which makes it entirely moot.


### More...

* Storage: [Swarm](https://swarm.ethereum.org) ([Book](https://gateway.ethswarm.org/bzz/latest.bookofswarm.eth/)), LBRY, SIA, FileCoin (IPFS), Swarm, NeoFS, Storj...
* [Near](https://near.org)
* [PolkaDot](https://polkadot.network)
* [Fluence](https://fluence.network)
* [Cosmos](cosmos.network)
* [Aether](https://getaether.net/)
* [ScuttleButt](https://ScuttleButt.nz)
* [RGB](https://rgb-org.github.io/)
* [Kleros](https://kleros.io)
* [Ergo](https://ergoplatform.org/en/)
* [THOR](https://thorchain.org/)

<!-- Done:
* Alacrity [bootstrap plan](https://github.com/AlacrisIO/alacrity/blob/master/docs/bootstrap.md)
  => moved to our wiki [Roadmap](https://gitlab.com/mukn/glow/-/wikis/Roadmap).
* [state channels](https://github.com/AlacrisIO/alacrity/blob/master/archive/StateChannel.sol)
  => moved to [#112](https://gitlab.com/mukn/glow/-/issues/112).
* [competition](https://github.com/AlacrisIO/alacrity/blob/master/archive/competition.md)
  => moved to our wiki [Competition](https://gitlab.com/mukn/glow/-/wikis/%7CCompetition).
-->
<!-- Sort me:
Integrate old documents such as the [Legicash FaCTS bibliography](https://github.com/AlacrisIO/legicash-facts/blob/master/bibliography.md) (below), and from the Alacrity archive: [language scope](https://github.com/AlacrisIO/alacrity/blob/master/archive/language-scope.md)

* [Plasma](https://plasma.io/),
  [Plasma Group](https://plasma.group/) ([RIP](https://medium.com/plasma-group/on-to-new-beginnings-e9d76b170752)),
  [ETH Research on Plasma](https://ethresear.ch/search?q=plasma),
  [Construction of a Plasma Chain 0x1](https://blog.omisego.network/construction-of-a-plasma-chain-0x1-614f6ebd1612) ([MVP git](https://github.com/omisego/plasma-mvp.git)),
  [Joseph Poon's talk at Deconomy 2018: "Consensus and Cryptoeconomic Incentive Mechanisms"](https://youtu.be/nZKdy7kZGBc).

* Interoperability:
  [Efficiently Bridging EVM Blockchains Relay Networks V2](https://blog.gridplus.io/efficiently-bridging-evm-blockchains-8421504e9ced) by Alex Miller,
  [Dogethereum: Retrofitting a two-way peg between blockchains](http://people.cs.uchicago.edu/~teutsch/papers/dogethereum.pdf),
  [Cosmos Inter-Blockchain Communication (IBC) Protocol](https://cosmos.network/docs/spec/ibc/).

* [Alice and Bob](https://en.wikipedia.org/wiki/Alice_and_Bob)

* Purely Functional Data Structures:
 [Okasaki's book](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf),
 [a follow-up](https://cstheory.stackexchange.com/questions/1539/whats-new-in-purely-functional-data-structures-since-okasaki)

* [Computability Logic](http://www.csc.villanova.edu/~japaridz/CL/)

* [K Framework](http://www.kframework.org/index.php/Main_Page),
  [KEVM: Semantics of EVM in K](https://github.com/kframework/evm-semantics),
  and the [Jello Paper](https://jellopaper.org/)

* Session Types for conversations:
 * [Parametrized Extensible Effects and Session Types](http://okmij.org/ftp/Haskell/extensible/param-eff.pdf)
   by Oleg Kiselyov,
 * [Acute](http://www.cl.cam.ac.uk/~pes20/acute/) and its successor
   [HashCaml](http://www.cl.cam.ac.uk/~pes20/hashcaml/) by Peter Sewell.
   Interesting, but they "only" address naming and marshalling,
   and with a weak security model.
 * [Cryptographic Protocol Explication and End-Point Projection](https://cs.brown.edu/~sk/Publications/Papers/Published/mk-crypto-prot-expl-epp/)
   by Jay McCarthy and Shriram Krishnamurthi (2008), or that cites it,
   [Cryptographic protocol synthesis and verification for multiparty sessions](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/01/cryptographic-protocol-synthesis-and-verification-for-multiparty-sessions-csf09.pdf)
   by Karthikeyan Bhargavan, Ricardo Corin, Pierre-malo Deniélou, Cédric Fournet, James J. Leifer (2009).
   Or [things that cite the latter](http://citeseerx.ist.psu.edu/showciting?doi=10.1.1.156.187).
   [Conversational Concurrency](http://syndicate-lang.org/tonyg-dissertation/) by Tony Garnock-Jones.

* Byzantine Fault Tolerance:
 [Papers selected by Rick Dudley](https://medium.com/@AFDudley/byzantine-fault-tolerant-consensus-papers-1b4b47d27463) (2015);
 [A Guide to 99% Fault Tolerant Consensus, by Vitalik Buterin](https://vitalik.ca/general/2018/08/07/99_fault_tolerant.html);
 [Recycling the Agreement: Distributed Consensus on DAGs, by Aleph Zero's Adam Gagol](https://medium.com/aleph-zero-foundation/recycling-the-agreement-distributed-consensus-on-dags-b415e4ebc6d)

* Scaling:
 [Ethereum wiki Sharding FAQ](https://github.com/ethereum/wiki/wiki/Sharding-FAQ),
 [Ethereum Sharding introduction and implementations](https://github.com/ethereum/wiki/wiki/Sharding-introduction-and-implementations).

* Oracles:
 [Truthcoin on Oracles vs Contracts](http://www.truthcoin.info/blog/contracts-oracles-sidechains/)
 (lots of great ideas, lots of more dubious ideas; great food for thought).

* Contracts in Tezos:
  [Michelson: the language of Smart Contracts in Tezos (PDF)](https://www.tezos.com/static/papers/language.pdf),
  [Michelson-lang.com](https://www.michelson-lang.com/),
  [Tezos Forum on Smart contracts](https://forums.tezos.community/c/smart-contracts),
  [Michelson contracts need access to blockchain state](https://gitlab.com/tezos/tezos/issues/158),
  [Watching the Tezos blockchain](https://github.com/MiloDavis/Hacky-OCaml-to-connect-to-Tezos-node).

* Contracts in EVM:
  [opcodes](https://ethereum.stackexchange.com/questions/119/what-opcodes-are-available-for-the-ethereum-evm),
  [old Ethereum block hashes](https://github.com/amiller/ethereum-blockhashes),
  [Zeppelin: deconstructing a solidity contract](https://blog.zeppelin.solutions/deconstructing-a-solidity-contract-part-ii-creation-vs-runtime-6b9d60ecb44c),
  [Proxy libraries](https://blog.zeppelin.solutions/proxy-libraries-in-solidity-79fbe4b970fd),
  [DelegateProxy](https://blog.gnosis.pm/solidity-delegateproxy-contracts-e09957d0f201),
  [Security Audit by QuantStamp](https://quantstamp.com/start),
  [Towards verifying ethereum smart contract bytecode in Isabelle/HOL](https://dl.acm.org/citation.cfm?doid=3176245.3167084),
  [the Parity Light Protocol](https://wiki.parity.io/The-Parity-Light-Protocol-%28PIP%29)
  (PR for similar light client functionality [in geth](https://github.com/ethereum/go-ethereum/pull/16534/files)),
  [Consensys Labs' Ethereum developer tools list](https://github.com/ConsenSysLabs/ethereum-developer-tools-list),
  [Solium, linter and formatter for Solidity](https://github.com/duaraghav8/solium),
  [Ethertrust, analysis tool for smart contracts](https://www.netidee.at/ethertrust),
  [The Challenges of Building Ethereum Infrastructure](https://medium.com/@lopp/the-challenges-of-building-ethereum-infrastructure-87e443e47a4b),
  [Ethereum Smart Contract Best Practices](https://consensys.github.io/smart-contract-best-practices/) including
  [Solidity Recommendations](https://consensys.github.io/smart-contract-best-practices/recommendations/).

* Compiling to the EVM:
  [pirapira's efforts](https://github.com/pirapira/ethereum-formal-verification-overview/blob/master/README.md) including
  [eth-isabelle](https://github.com/pirapira/eth-isabelle) and
  [Bamboo](https://github.com/cornellblockchain/bamboo),
  [EtherVM](https://ethervm.io/),
  [Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf),
  [Jello Paper](https://jellopaper.org/evm/).

* Formal Methods:
  [What do Formal Methods actually Guarantee?](https://medium.com/alacris/what-do-formal-methods-actually-guarantee-d94ae8802be2)
  [Formally Verified Software in the Real World](https://cacm.acm.org/magazines/2018/10/231372-formally-verified-software-in-the-real-world/fulltext),
  [Z3 tutorial](https://rise4fun.com/z3/tutorial),
  [DeepHOL](https://deepai.org/publication/holist-an-environment-for-machine-learning-of-higher-order-theorem-proving).

* Formalization for smart contracts:
  [Scilla-coq](https://github.com/ilyasergey/scilla-coq)
  (see paper [Scilla: a Smart Contract Intermediate-Level LAnguage](http://ilyasergey.net/papers/scilla-overview.pdf)),
  [TezosCoq](https://github.com/tezos/tezoscoq),
  [Ergo](https://ergo.readthedocs.io/en/latest/Overview.html),
  [Russell O'Connor: "Simplicity: A New Language for Blockchains"](https://arxiv.org/abs/1711.03028),
  [Peng Wang](https://people.csail.mit.edu/wangpeng/)['s](https://www.csail.mit.edu/event/type-system-resource-bounds-type-preserving-compilation-and-its-application-ethereum-smart) [thesis](https://people.csail.mit.edu/wangpeng/phd-thesis.pdf) ([code](https://github.com/wangpengmit/phd-thesis-supplemental), [timl](https://github.com/mit-plv/timl)),
  [ERC777-K](https://runtimeverification.com/blog/erc777-k-formal-executable-specification-of-erc777/),
  [Formal Verification of OpenZeppelin's ERC20 Token Contract](https://github.com/runtimeverification/verified-smart-contracts/blob/master/erc20/zeppelin/README.md),
  [ERC20: Missing return value bug](https://medium.com/coinmonks/missing-return-value-bug-at-least-130-tokens-affected-d67bf08521ca).

* Atomic swaps:
  [Ethereum atomic swaps with Bitcoin and most other coins, about to hit release](https://www.reddit.com/r/ethereum/comments/865e0l/ethereum_atomic_swaps_with_bitcoin_and_most_other/),
  [AltCoin](https://github.com/AltCoinExchange/ethatomicswap/),
  [RepublicProtocol](https://github.com/republicprotocol/eth-atomic-swap),
  [Komodo BarterDEX](https://komodoplatform.com/decentralized-exchange/)

* Building secure software:
  [Some thoughts on security after ten years of qmail 1.0 (2007)](https://cr.yp.to/qmail/qmailsec-20071101.pdf),
  ...

* UI tools for Crypto-currency dApps:
  [MetaMask Browser Extension](https://github.com/MetaMask/metamask-extension)

* Usability and UX:
  [Nielsen Norman Group](https://www.nngroup.com/articles/)

* Discipline for more robust programming
  [STAMPING ON EVENT-STREAM](https://www.hillelwayne.com/post/stamping-on-eventstream/)

* Low-level Cryptographic protocols:
  - Threshold signatures:
  [Threshold-optimal DSA/ECDSA signatures and an application to Bitcoin wallet security](https://eprint.iacr.org/2016/013.pdf),
  [Schnorr signatures](https://blog.chain.link/threshold-signatures-in-chainlink/) are O(1) space-efficient but take O(n³) messages so only scale to a few participants;
  BLS signatures are slightly more expensive to check but only take O(n) messages and so scale to larger number of participants.

* Interactive help for Ethereum:
  [Go Ethereum gitter](https://gitter.im/ethereum/go-ethereum),
  [Ethereum Magicians](https://ethereum-magicians.org/top/all)...

* Nix: [Overlays](https://nixos.org/nixpkgs/manual/#chap-overlays),
  [Typing Nix](https://www.tweag.io/posts/2017-05-23-typing-nix.html)
  ([tix-papers](https://github.com/regnat/tix-papers), [tix](https://github.com/regnat/tix))...

* Formal Method advocacy:
  [Formally Verified Software in the Real World](https://cacm.acm.org/magazines/2018/10/231372-formally-verified-software-in-the-real-world/fulltext),
  [What do Formal Methods actually Guarantee?](https://medium.com/alacris/what-do-formal-methods-actually-guarantee-d94ae8802be2)

* Homomorphic encryption:
  [Reusable Non-Interactive Secure Computation](https://eprint.iacr.org/2018/940.pdf),
  [Efficent Multi-Party computation toolkit](https://github.com/emp-toolkit),
  [Jonathan Katz](http://www.cs.umd.edu/~jkatz/papers.html),
  ... something in F* or Coq from MSR Cambridge?

* Algorand:
  See the original whitepaper,
  [Vault](https://eprint.iacr.org/2018/269.pdf),
  and their bibliography...
  also [flaws?](https://hackernoon.com/a-fatal-flaw-in-algorand-professor-yongge-wang-takes-apart-their-renown-consensus-agreement-4c111286cdbb)

* More Coq for operating systems: [Bedrock](https://github.com/mit-plv/bedrock2), [CertiKOS](https://www.cs.yale.edu/flint/certikos/)

* Contracts on BCH:
  [New Bitcoin Cash Opcode Shows an Onchain Game of Chess is Possible](https://news.bitcoin.com/new-bitcoin-cash-opcode-shows-an-onchain-game-of-chess-is-possible/)

* Operating Systems: [Alexia Massalin's Synthesis](http://valerieaurora.org/synthesis/SynthesisOS/),
  [Mainframe](https://docs.mainframeos.com/docs/introduction/)

* Other bibliographies:
  [Bitcoin History](https://infominer.id/bitcoin-history/),
  [IOHK papers](https://iohk.io/research/papers/),
  [CoinMetrics resources](https://coinmetrics.io/resources/),
  (some old timer once published a link to a trove of papers... where?),
  ...

* [Elixxir](https://elixxir.io/introduction)...

* ["Common Knowledge vs Mutual Knowledge", as illustrated by Friends](https://youtu.be/AksTw43U998)

* [Ant Routing scalability for the Lightning Network](https://arxiv.org/pdf/2002.01374.pdf)

Arbitrum
Chainlink

* [Loyc](http://loyc.net/), Language-of-your-choice

Diaspora, Scuttlebutt, Dreamwidth, MeWe, Minds, Hive.io, Discussions[CENSORED].app, Flote.app, Hubzilla, Element.io, Signal groups, etc

https://en.wikipedia.org/wiki/Obliq

Learning Tezos: https://twitter.com/ArthurB/status/1365642482919301122
-->
