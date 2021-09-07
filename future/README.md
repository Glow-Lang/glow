# Future work to be done for Glow

These include examples that ought to work in the future,
and other incomplete projects.

## Benchmark Contracts

### Selling a signature

[buy_sig.glow](../dapps/buy_sig.glow): selling signatures that can be used on the same chain,
on a different chain, or even off-chain.

Required Features:
- Support for signing and verifying signatures.

### Coin Flip

[coinflip.glow](coinflip.glow): Simple gambling contract, even simpler than Rock, Paper, Scissors.
Less fun, though.
NB: compared to the [current code](../dapps/coin_flip.glow), the future version assumes that
escrows will be automatically added by the compiler.

Required Features:
- Compiler front-end
- Support for automatic escrow generation.
- Support for specifying the safety with assertions.

### Rock, Papers, Scissors

File [rps.glow](rps.glow) specifies this very simple two-person game,
to illustrate our conflict resolution mechanisms on the simplest adversarial system possible.

File [rps-min.glow](rps-min.glow) is essentially the same, stripped to the bare essential,
so that we can compare the complexity of this application with Glow vs another combination of languages.

Required Features:
- Same as Coin-Flip
- Support for user-defined data types and "small" functions

### Dead-man Switch

[deadmanswitch.glow](deadmanswitch.glow):
Allow an alternate key to control the assets,
but only if the first key doesn't react for some time,
and optionally only after some challenge to deny the alternate key.
Such a contract was famously invented
[on BCH by Karol Trzeszczkowski](https://github.com/KarolTrzeszczkowski/Electron-Cash-Last-Will-Plugin).

Required Features:
- Explicit timeouts
- Support for safety with explicit timeouts
- Choice restricted to timeouts.
- (Tail) Recursion

### Simple Auction

[auction.glow](auction.glow):
Sell an asset (or a signature or procedure call ?)
in a simple English (open ascending price) forward candle auction.

The candle effect doesn't require any special care in the contract itself:
just the uncertainty of whether your transaction will make it on the official block,
or whether someone else's overbidding transaction will,
is assumed to provide enough uncertainty for the "candle" effect
that discourages overly last-minute sniping.

Required Features:
- Explicit timeouts
- Open set of participants though a finite number at once
- Choice restricted to timeouts and open participation.
- (Tail) Recursion

### Crowdfunding

Participants pledge money; the total amount is disbursed to the designated campaign recipient
if and only if the required amount is reached before the deadline;
otherwise it is returned to pledgers.
This benchmark allows to compare our system to Scilla and Plutus.

Required Features:
- Explicit timeouts
- Unrestricted open set of participants
- Choice restricted to timeouts and open participation.
- Shared state between multiple small interactions

### Complex Auctions

Dutch auctions with sealed bids, escrowed amount that must be larger than the bid
(and equal to the bid if the sealed bid is not revealed), and other auctions
that involve shared state, multiple UTXOs, etc.

Required Features:
- Shared state between multiple small interactions
- Data structures across such shared state

### Fungible Tokens in native Glow style

How would we like to implement and use fungible tokens
in a native Glow application that didn't care about compatibility?

Required Features:
- Unrestricted open set of participants
- Multiple assets
- API with multiple entry-points
- General choice with an open set of participation and multiple entry points
- Data structures with containers.

### Atomic Swap between Same-Chain Fungible Tokens

Demonstrate how to swap tokens in Glow.

Required Features:
- Multiple assets
- Calling another contract.

### Repeated Atomic Swap between Same-Chain Fungible Tokens

Demonstrate how to swap tokens in Glow, in multiple parts.

Required Features:
- Multiple assets
- Calling another contract.
- Loop with user-defined bound. Division?

### Interoperate with Standard Fungible Tokens

Support legacy standards within Glow applications, first to use them, second to implement them:
ERC-20, ERC-223, ERC-777,
[ERC777-K](https://runtimeverification.com/blog/erc777-k-formal-executable-specification-of-erc777/).

Required Features:
- Multiple assets
- Open set of participants
- Choice with multiple entry points,
- FFI to Solidity API
- Data structures with containers.

### Non-Fungible Tokens in native Glow-style

How would we like to implement and use non-fungible tokens
in a native Glow application that didn't care about compatibility?

Cryptoscalies, video-game tokens, foo-on-the-blockchain.

Include atomic swap capability so contracts can be written that atomically sell a NFT for ETH, etc.

Required Features:
- Multiple assets
- Open set of participants
- Choice between multiple entry points
- Data structures with containers.

### Interoperate with Standard Non-Fungible Tokens

Support legacy standards for Non-Fungible Tokens within Glow applications,
first to use them, second to implement them:
ERC-721, ERC-998, ERC-1155.

Required Features:
- Multiple assets
- Open set of participants
- Choice with multiple entry points
- Data structures with containers
- FFI to Solidity API.

### Staking Rewards

Required features:
- Shared state
- Multiple tokens

### Unanimous multisig

The naive way, with a list of RSA keys. Or the more clever way, with Schnorr or BLS keys.

The consensual part of a state channel or other closed smart contract.

Required Features:
- Data structures with containers
- Iteration
- Off-chain negotiation,
- FFI to new cryptographic primitives.

### N-out-of-M multisig

The naive way, with a list of RSA keys. Or the more clever way, with Schnorr or BLS keys.

2-out-of-3: Trusted Arbitration to contracts. A trusted intermediary may arbitrate disputes over
two people by siding with one against the other, or validate the casual transactions of one party
that keeps a second key in cold storage.

N-out-of-M: committee for a side-chain, oracle, etc.

Required Features:
- data structures with containers
- iteration
- off-chain negotiation
- FFI to new cryptographic primitives
- Trusted operators.

### Arbitration

2-out-of-3 contract, or otherwise (consensus-or-arbitration),
with semi-automated off-chain rules for arbitration, e.g. for an oracle
where humans are only used for the non-clear-cut cases,
and/or after extra escrow for appeal.
This is a good contract for shopping of physical goods.

### Buy-sig with Arbitration

This is a good contract for renting temporary space (AirBnB on the blockchain),
since it fully automates the consensual case, while leaving
an arbitration procedure for the conflictual case.

### Generalized State Channels

Required Features:
- Second pass transformation that transforms transactions into a combination of
  optimistic off-chain transaction and fallback on-chain transaction.
- Pre-sign a unanimous multisig message to exit to the dispute-resolution contract
  before you enter the multisig message.
- Good timeout management
- Off-chain watchtowering... (more?)

### State Channels Networks

Lightning-style channels.
Beware the challenge-relative vs absolute timeouts for transactions,
the relationships between those timeouts within a given channel, and
within the many channels in a circuit.

Required Features:
- Good timeout management
- Interface to good off-chain protocols,
- Off-chain watchtowering, etc.

### Poker

Some complex card game where each party can only see part of the state,
which includes logical constraints.
See the [ROYALE paper](https://iohk.io/research/papers/#MPEKMMQP) by IOHK.

Required Features:
- Commutative zero knowledge cryptographic primitives
- Off-chain key exchange
- Generalized state channels.

### Automated Bill Payment

Subscription to a service with a service meter, and
an arbiter in case of timely disputes.

Required Features:
- Modelling Trust in Operator
- Delayed claims

### Oracle

Futures contract assuming an oracle for e.g. weather or earthquakes or prices.

Required Features:
- Modelling Trust in Operator and/or in a side-chain validation committee.
- Either internalizing the validation rules, or trusting the committee to enforce them anyway.

### Insurance Contract

Futures contract assuming an oracle for e.g. weather or earthquakes or prices.

Required Features:
- Being able to call another DApp for its Oracle
- Being able to internalize the mathematical formulas to compute the price

### Future Contract with Complex Strike Price

The strike price depends on some complex formula that processes a lot of time-series data
from trusted oracles.

Required Features:
- Being able to call another DApp for its Oracle
- Trusting the Oracle that the data is public, or having an MKB to store the data
- Being able to internalize complex computations mathematical formulas to compute the price

### DAI-style stable coin in native Glow style

Reimplement the DAI in native Glow style.

Required Features:
- Being able to call another DApp for its Oracle
- Being able to call other DApps
- Unrestricted multiparty interactions
- Data structures with containers
- Support for multiple assets
- more?

### Support for legacy DAI contract

Actually interact with MakerDAO's system.

- Being able to call other DApps
- Unrestricted multiparty interactions
- Data structures with containers
- Support for multiple assets
- more?
- FFI to Solidity

### Collateralized lending in native Glow style

Reimplement compound.finance.

- Wrapping tokens on other blockchains
- Bridging other blockchains
- Being able to call other DApps for an oracle
- Wrapping tokens on this blockchain
- Being able to call other DApps
- Unrestricted multiparty interactions
- Data structures with containers
- Support for multiple assets
- FFI to Solidity
- more?

### Fast Payments Side-Chain

Simple Side-Chain with just payments, nothing fancy on-chain.
Can it be derived from our fungible token DApp with minimal changes?
Can zk-rollup, optimistic rollup, and their MKB equivalent, all be derived
as variants of the same code with slightly different backend configurations?

Make transactions fast with a market of side-chain operators kept honest by the MKB.
Allow for variant where the MKB does all the checking, in the style of the Blockstream Liquid network.
Allow for censorship-resistant side-chains with higher-latency initially blinded blocks.
Maybe have a separate file for each variant?

Develop off-chain part of the solution:
start with phone-to-phone payments, then add some Point-of-Sale capability.

- Being able to call other DApps for an oracle
- Wrapping tokens on this blockchain
- Being able to call other DApps
- Unrestricted multiparty interactions
- Recursive data structures with containers
- Logical claims to validate the data structure
- Delayed claims

### General Purpose Side-Chain

Elaboration of the previous, a Side-chain that allows for arbitrary contracts.
Maybe with builtin support for multiple assets, fungible or non-fungible.

Scale all the previous contracts!

- Being able to call other DApps for an oracle
- Wrapping tokens on this blockchain
- Being able to call other DApps
- Unrestricted multiparty interactions
- Recursive data structures with containers
- Logical claims to validate the data structure
- Delayed claims

### Non-Custodial Decentralized Exchange

Matching partners via an order book, who are then bound to complete an atomic swap
between the two matched sets of assets, fast, through collateralized side-chains.

- Wrapping tokens on other chains
- Bridging other chains
- Being able to call other DApps for an oracle
- Wrapping tokens on this blockchain
- Being able to call other DApps
- Unrestricted multiparty interactions
- Recursive data structures with containers
- Logical claims to validate the data structure
- Delayed claims

### Private Transactions

ZCash or MimbleWimble on a side-chain.

- ZK Computations.

### Supply Chain

Public acknowledgements for fixed multiparty workflow on a scalable side-chain.

- Scaling solution.
- Contracts that validate claims about logical predicates on the data structures.
- Arbitration in case of dispute.
- Ways to publish encore amendments to otherwise immutable data structure.

### Accounting Audit

Audit accounting based on blockchain.

Off-chain, on top of supply chain on-chain.

### Digital Asset Lending Platform

A peer-to-peer lending platform for copies of digital assets, e.g. CDs or DVDs.
Participants who hold physical copies or digital rights can escrow their copies in the system
and generate "tokens" that they can then lend to other participants who will be using those copies remotely.

In practice, the participants use convergent encryption to encrypt each file
with a key derived from its blake3 tree hash, and
participants may hold backup copies of the files of other participants,
that they cannot decrypt.
When a participant borrows the right to use a file, they receive
a temporary token that contains the key for that file, encrypted against their private key;
the token expires after some time, or may be returned before it expires,
at which point the borrower forgets the key
(or doesn't, but then it's the same as if he had made an unauthorized copy).
Various oracles may vouch for the validity of the tokens issued;
mostly, people with an identity (from another identity oracle) vouch for themselves;
lenders and borrowers are each responsible for their individual behavior and its consequences.
Borrowers may pay a small fee to the lenders and/or to the underlying system.
A ledger of who borrows which copy when is generated;
alternatively, it's all anonymous based on zkSNARKs.
White-lists or reputedly legit lenders, and/or black-lists of bad lenders,
may be generated and consulted by users before to borrow, to avoid legal liabilities.

This application would involves at the same time some large scale "on chain" interaction
to track the tokens, and some large scale "off chain" interaction to exchange those the encrypted
files and the decryption tokens.

### DAO

On top of existing Token:

- voting for decisions.
- voting
