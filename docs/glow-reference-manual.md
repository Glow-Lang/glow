## Intro

THIS FILE HAS OLD STUFF THAT NEEDS TO BE MOVED TO THE Scribble DOCUMENTATION AND/OR THE Wiki.

### Hello, there! Who are you?

Hi, and welcome to Glow's reference manual.

To be as useful as possible, this manual has been designed with multiple layers of complexity,
depending on your level of proficiency in programming and in computer science:
this means you can re-read it as you get better at using *Glow*.
This manual has been written both
by nearly total-beginners, and computer science experts,
so that *Glow* can be learnt without being confusing.

### Level 1: I am new to computer programming

Great! Your presence here means our product is becoming mainstream.

For now, though, *Glow* is a bit tricky to start programming,
but in the future, we plan to make tutorials that will take you from zero knowledge
to a point where you can code DApps with *Glow* without any uncertainty.
We want to make *Glow* as accessible as possible, and we'll do everything for you to reach this goal.

In the meantime, you can browse the Internet to learn the two prerequisites for *Glow*-programming:

  - The general notions of computer science and app-development.
  - A basic understanding of how blockchain-related programming works.

You can still read this documentation for business purposes and skip the parts that contain code.

### Level 2: I can make standard apps

You're a developer with knowledge of the production of standard apps.
You know how to make one for your users,
but you lack the knowledge and experience in the field of DApps:
*Glow* comes with many typical smart-contracts that you can use in your DApps.

You just have to invoke one of our pre-made templates,
with parameters suitable to your use-case,
and our software will let you safely interact with the blockchain.

You'll be sure that your DApp's transactions are secure, so that you only have
to take care of what matters the most in any app:
its general design, its business logic, its front-end, etc.

While you won't need to program _in_ the *Glow* language, you may still read this manual
to understand what *Glow* does for you and how it works, so you can fit
the functionality it provides into the architecture of your application.
Thus you can ultimately use *Glow* and its runtime as an efficient tool
that will save you time and money, a lot of a worrying uncertainty
about the risks you would otherwise have to face while building your DApp.

### Level 3: I can make DApps, and want to make my own smart-contracts

You have read this manual and tried *Glow* for a while,
or you are already a bit experienced in blockchain-related programming.

This documentation will allow you to make your own smart-contracts:
you will develop your own versions of our tools,
for your own unique vision of smart-contracts.
The team behind *Glow* can audit and certify your smart-contracts for a fee,
if you want to make sure they are safe, and
want your potential users to trust it, too.

Each chapter of this manual links to sub-documents for Level-3 users.
Each time you want to be more autonomous and make the best use of *Glow*,
those links will give you advanced tips about
what our community and development team understood over time.

It'll save you precious tinkering-time, and
you'll be able to grasp *Glow* (and DApps) like never before.

### Level 4: I want to help develop *Glow* and understand all the details

A contributor, are we? Well, welcome backstage:
each chapter of this manual contains sub-documents for level-4 users.

These sub-documents can contain either the source code of *Glow* (written in other languages)
or details about the thinking process we went through
(you'll find some high-level mathematical proofs,
links to our previous versions, and many other things).

In the end, these documents will allow you to contribute to *Glow*,
make your own version of it, or just audit it and know it as well as we do.

Feel free to share your progress with the community.

## Why use *Glow*?

Regular apps and computer programs are generally safe and reliable when used by only one person:
i.e. solo video games, text editors, calculators, graphic design softwares, or anything, really...

But when you have to interact with other people with one of today's regular apps,
it needs to be regulated by a centralized entity: a state, a company, or a person.
This entity will use servers physically located in some data-center around in the world,
to ensure the safety of the interactions:
i.e. AirBnB, Facebook, Uber, Amazon, any bank account or wallet, sending emails, etc.
This entity, or people who manage to coerce it (governments and other mafia)
or to penetrate it (crackers and spies), can control or disrupt the users' experience:
from charging a monopoly rent to censoring dissenting activities,
from spying on users to tampering with data, from spamming the users with unwanted advertising
to trying to influence them with propaganda, false or biased news, omissions and distractions.
Sometimes, the users are just victim of the central entity's incompetence,
or its change of mind that causes the service to be cancelled, disrupted, corrupted,
or just slow and buggy and never fixed.

An app that runs with being controled by any single entity or consortium of colluding entities
that can unaccountably collude against users is called a *DApp*,
which stands for Decentralized Application, and is pronounced either "dap" or "D-app".

A DApp is distributed around all the users of a network, and
compared to its centralized ancestor, it has many advantages:

- Uncensorable, ad-less, bias-less, spam-less social networks.
- Peer-to-peer services where individual service providers are paid more.
  (because they don't pay monopoly rent to a centralized server).
- Increased safety of your asset transfers.
- Complete traceability of an asset, for supply chain managers, stock-management, or even lawyers.
- Enhanced privacy.

### Problem

Compared to centralized programs,
DApps are both *much* harder to write and *much* less tolerant to mistakes.

DApps are harder to write, because they inherently involve
multiple untrusting participants over asynchronous networks;
when designing protocols, you must therefore make sure that
the incentives of all participants are aligned at all times.

DApps are less tolerant to mistakes,
because active adversaries constantly seek to take advantage of any mistakes to steal your assets,
and there is no central authority to enforce your rights if that happens (or to violate them ordinarily);
thus you cannot afford any single bug in the parts of your apps that deal
with  immutable decentralized ledgers.

Hundreds of millions of dollars worth of assets have been lost to various mistakes
made in DApps, already.
Even seasoned experts make simple manipulation mistakes and lose tens of thousands of dollars.

### Solution

To make DApps more mainstream, the world needed a toolset to automate
all the critical parts of a DApp and make them safe.

This is precisely what *Glow* is:
it drastically simplifies the way a DApp is programmed, to make it safer.

## What is *Glow* for?

### A programming language for DApps

*Glow* is a programming language used to make DApps.
It comes with a wide set of tools.

*Glow's* three main specificities are:

  - **Safety**: each newly implemented function is thoroughly audited
    so that a programmer using the *Glow* language or its associated toolset
    won't have to worry about securing interactions between users.

  - **User-friendliness**: we want to make DApps mainstream, and
    *Glow*'s development team always aims at making it obvious what a DApp does or doesn't,
    so users can ascertain that it indeed matches their expectations.

  - **Portable* to any blockchain**: applications written in *Glow*
    can produce run on any blockchain, so you unlike with previous tools,
    you don't have to learn all the tooling specific to a single blockchain
    only to realize later the users you want will be on another blockchain.
    A same DApp will be able to use the resources of multiple blockchains at once,
    and meet the users where they are and wherever they will be.

In other words: what usually takes a huge amount of lines of code,
and many experts in various fields, has been automated.
Some people will use contracts pre-made in *Glow*,
some others will make their contracts with a very safe language.

### What *Glow* can and can't do

*Glow* is a language dedicated to the critical parts of DApps:
interactions between multiple participants who do not trust each other,
to exchange digital assets registered on decentralized ledgers,
according to rules verifiable by computers.

The role of the DApp is to create a sufficient trust between the participants
for them to actually partake in an interaction that they wouldn't otherwise have dared participate in,
based on their trust in the ledgers that maintain their digital assets,
based on their trusting how these ledgers supports "smart contracts"
that will enforce the rules of the interaction,
and based on their understanding of the rules of the interaction and agreement to them.

The role of *Glow* is to allow the participants to think of this interaction
in terms of the asset transfers that matter to them,
while taking care of all the difficult tasks required to translate this interaction
in terms of messages exchanged on asynchronous digital networks.

At the moment, *Glow* cannot be used to program every type of software, and isn't meant to do so.
Just like some languages are dedicated to specialized tasks
(*e.g.* R for data science and statistics), *Glow*'s main focus is safe DApps,
including the underlying smart contracts, but not limited to them---DApps
also include client or server code for each participant, that must critically match the contract,
and exactly follow the same protocol, or someone will lose their assets.

For that reason, every time we add a new feature,
we carefully check if its addition is safe in the whole *Glow* ecosystem.
Maybe one day you will be able to program complete apps, full video games
and everything in *Glow*,
but for the moment, we focus on *Glow*'s specialty,
so that DApps developed with *Glow* stay safe.


#### Conceptual Basis

We compiled and extensive list of what you should read
to understand our thought-process during the development of *Glow*.

We hope you'll enjoy reading our
[bibliography about relevant related subjects](https://gitlab.com/mukn/glow/-/blob/master/docs/extra-bibliography.md).

Also, if you are keen on reading them, check what kind of
[mathematical safety proofs](https://gitlab.com/mukn/glow/-/blob/master/docs/proofs.md)
we are currently working on for DApps written in *Glow*.

### Environments

#### Use *Glow* in any browser

TODO: In the near future, our code will run on any browser
on any desktop or laptop or mobile device,
with a nice user interface. But this is still work in progress.

#### Use *Glow* locally on Linux and macOS

At the moment, *Glow* can run on Linux and macOS.

Any Linux distribution will do: we use the [Nix](https://nixos.org) package manager.
Nix can run for users on top of any Linux distribution (not just NixOS),
and also runs well on macOS.
Nix notably builds software in a deterministic way,
so that if it works for us it will work for you,
and if you experience a bug, we can reproduce it, too, and fix it.

TODO: We will soon add support for Windows.
Nix doesn't yet run out-of-the-box on Windows; given sufficient resources, it could be made to.
*Glow* can also be built without Nix, though making that run on Windows too will require resources.
Instead, we plan to have *Glow* target the JavaScript platform,
and be able to run on Windows that way.

## Getting started

### Simplified install on Linux or macOS


##### Safety

This installation script first installs the [Nix](https://nixos.org) package manager,
which may require you to manually type `yes`, or `y` and/or type the administrator password
around the beginning, to authorize parts of the installation. It is a normal part of the process.

Of course *we* trust the installation code, but *you* shouldn't until *you* audit it,
and so you would be more prudent to run it inside a virtual machine.
For extra security, you might want to use [Qubes OS](https://www.qubes-os.org/).

##### Why the installation is that long (levels 3 and 4)

The reason the installation is that long is that you are using a platform
for which we don't have precompiled binary packages, and so the installation
is going to recompile the entire compiler suite, starting with Gambit Scheme.

If you are using a computer with a supported target architecture, and it's recompiling,
then there must be a bug in our release discipline. Please contact us about it.

##### Configuring Nix to use our repository

For alpha quality releases (the only ones at this moment),
we offer this nixpkgs channel:

```
export NIX_PATH=nixpkgs=http://github.com/muknio/nixpkgs/archive/alpha.tar.gz
```

For the latest development code on the bleeding edge (which can be broken at times),
we offer this nixpkgs channel:

```
export NIX_PATH=nixpkgs=http://github.com/muknio/nixpkgs/archive/devel.tar.gz
```


# Languages basics

## Overview

*Glow* is a domain-specific language for DApps, Decentralized Applications.

The language syntax is based on that of JavaScript,
with some inspiration from ReasonML where we have to diverge from JavaScript.
While it should feel familiar to you if you know JavaScript,
there are many differences underneath designed to keep your DApps safe:

  - The language is statically typed.
    Its type system is in the same general style as ReasonML or TypeScript,
    yet differs in many details, wherein we optimize our design for the safety of DApps.

  - Though the syntax looks imperative, it is a function language underneath.
    In particular, you cannot modify bindings to existing variables,
    you can only have new variables shadow old variables.
    You also cannot side-effect data structures, only create new data structures
    that shadow the old data structures as those in consideration.

  - Many features are currently missing, such as loops or recursion,
    that will be added slowly as the need arises.

If you are used to programming in imperative languages, you will also find common concepts.

_For programming language buffs_:
DApps are asynchronous interactions between multiple mutually-untrusting participant
manipulating digital assets on decentralized ledgers,
according to rules verifiable by blockchain smart contracts.
Semantically, *Glow* is an applicative language with a pure functional programming core
extended with a few primitives and annotations making it suitable for for multiparty computation.
It has a static type system based on MLsub, and static safety analyses
that are discharged by a theorem solver. (TODO: complete the static analyses.)

If you are a seasoned developer, you can check our less-detailed and straight-to-the-point grammar,
our [front-end syntax](https://gitlab.com/mukn/glow/-/blob/master/docs/internal/syntax/glow-sexpr.scrbl) and [surface grammar](https://gitlab.com/mukn/glow/-/blob/master/docs/internal/syntax/glow-surface.scrbl).

Otherwise the next chapters are designed to make you grasp it quickly.


## Fundamentals: Hello world!

Once *Glow* is installed, you can code in any local text editor on Linux or macOS.

TODO: Later, we will implement a user-interface for *Glow*
that will allow you to code directly in your browser.

Unlike most languages, including most languages supposedly dedicated
to specifying DApps or just their smart contracts,
*Glow* is a language focused on interactions between multiple participants.

Hence, there is no typical `"hello world!"` program like in most languages,
but there is something alike: a `"hello world!"` for two...

Let's have a look at this very simple code:

```
#lang glow
@interaction([A, B])
let publishHello = () => {
  @A let ha = "Hello, B, I am A. How do you do?";
  publish! A -> ha;

  @B let hb = "Hello, A. I am B. How do YOU do?";
  publish! B -> hb;
};
```

This is not a very useful DApp, but then neither is printing `"Hello, world"`.
For a minimal useful DApp, see [buy-sig](#Buying a Signature) below.

Any program that you write in *Glow* starts with the mandatory line:
```
#lang glow
```

Interactions are specified using annotations, that begin the "at-sign" (a.k.a. arobase): `@`

If you remove the annotations, it looks like a regular JavaScript or ReasonML program,
except with those special `publish!` statements.

(Also, note that white spaces and interlines don't matter.
We didn't implement indentation-based structure, and don't intend to in the short-run.
It is too easy to introduce *subtle* with such semi-invisible structure;
we prefer bugs to remain painfully obvious, instead.)

**The first line** of this code specifies through an annotation
that the following function will be an interaction between two participants
the roles of which are called `A` and `B`.

You can see that semicolons are used to end statements.
They can be used to end every expression, but are mandatory only for statements.

**The second line** defines the interaction function.

**The third and fifth lines** define two private variables respectively named `ha` and `hb`
using the keyword `let`, each bound to a different byte string.
The former is initially private to `A` only,
while the latter is initially private to `B` only.

**The fourth and sixth** lines use the `publish!` statement to specify that
the respective sentences `ha` and `hb` will be sent
respectively on behalf of users `A` and `B`.

Note that these instructions will be executed line by line, so the dialog will happen like a real one.


## Caveats when using both *Glow* and JavaScript

The syntax or *Glow* was specifically designed to be familiar to those who use
JavaScript, ReasonML, TypeScript, etc.

One major principle we follow is one to avoid confusion:
**If program fragment is valid in both *Glow* and JavaScript,
the *meaning* of that fragment should be the same in both languages, and/or
any discrepancy in this meaning should be *obvious*.**

If at any point you are confused by how a *Glow* program works
based on your expectations as a JavaScript or ReasonML or TypeScript programmer,
or on the contrary if when writing code in JavaScript or ReasonML or TypeScript
you are led to wrong expectations based on what you read or wrote in *Glow*,
then this is a serious design issue that we will try to address in a future version of *Glow*.

That said, here are some differences between *Glow* and JavaScript:

- *Glow* integers are all `BigInt`s, yet *Glow* integer literals do not end with an `n`
  whereas JavaScript `BigInt` literals do, and JavaScript integer literals without an `n`
  are read as floating point-numbers that only keep 52 bits worth of mantissa information.
  This difference should be pretty obvious when reading or writing *Glow* or JavaScript programs
  and translating from one to the other, but bears mentioning.

- *Glow* uses `|` for alternatives in types or in pattern matching, not for bitwise-or.
  Also, *Glow* does not use `&` or `^` or `~` for bitwise and, xor, not.
  Instead, *Glow* uses tripled operators `&&&`, `|||`, `^^^`, `~~~`
  for bitwise and, or, xor, not operations.
  The syntactic difference is obvious and leaves no room for confusion.

- *Glow* uses `<<` and `>>` for bitwise shift left and right operations.
  These work the same as in JavaScript as long as you use `BigInt` in JavaScript.
  Note however that legacy bitwise operations on "normal" numbers in JavaScript
  first interprets floating-point numbers as 32-bit signed integers,
  then computes on those 32-bit signed integers;
  it then provides a special `>>>` "logical" shift right operation
  that unlike other operations specially treats its left-hand-side argument as
  an 32-bit *unsigned* integer; this operator is not available on `BigInt`.
  There again, these differences should be relatively obvious to people dealing
  with integers in JavaScript vs *Glow*.

## Compiling your program

At the moment, *Glow* doesn't have much or a user-interface for building and deploying DApps.

But we will include user satisfaction in the design.

## Examples of DApps in *Glow*

If you are at the beginning of your journey in DApp programming,
you might want to just program DApps buy importing ready-made smart contracts.

We have plenty of them at [this address](https://gitlab.com/mukn/glow/-/tree/master/future#dead-man-switch), where you'll find a list of our contracts (those already done and our works-in-progress) with a list of their features, and, of course, `.glow` files for you to import in your DApp. Each of these files is open-source and auditable.

But of course, if you want to program your smart-contracts yourself, you will have to write one from scratch, or from a modified copy of one of the existing contracts.

Some of *Glow*'s functionalities will be hard to grasp if you already need to focus on the main functions of your DApp. All the examples below are copy-pastable in *Glow*, but if you want to use a clean version of them in your DApp, you can just replicate them.

TODO: ???

## Buying a Signature

The simplest useful DApp you can write is one to buy and sell a signature.
This is what our example DApp `buy-sig` below does.

Its practical applications include:
  - "Swaps" where you sign a message for use on another blockchain.
  - Rental where you sign a message that grants you access to some digital resource.
  - "Diploma", certification or other affidavit---also for purely digital goods.

However, for applications that involve real-world goods and services,
you will want a slight modification of `buy-sig` at the end of which
the price is deposited into another DApp:
an escrow that allows a trusted third-party or decentralized oracle
to be used as an arbitrator in case of later dispute between the two participants.

```
#lang glow
@interaction([Buyer, Seller])
let payForSignature = (digest : Digest, price : Nat) => {
  deposit! Buyer -> price;

  @publicly!(Seller) let signature = sign(digest);
  withdraw! Seller <- price;
}
```

This contract starts with a special line `#lang glow` to indicate that it is a glow program indeed.

Then we define a function `payForSignature` with two parameters `digest` and `price`,
in a syntax familiar to JavaScript, TypeScript or ReasonML programmers.
However, this definition is preceded by an *annotation* `@interaction([Buyer, Seller])`
that indicates that this function involves a blockchain interaction
with two participant roles:
A first role called `Buyer`, and a second role called `Seller`.

This interaction involves *two* steps, each step being
a blockchain transaction involving one participant and the on-chain smart contract.

In the first step, the `Buyer` deposits the agreed-upon price in escrow into the smart contract,
with this statement:
```
  deposit! Buyer -> price;
```

In the second step, the `Seller` publishes his signature, and
after the contract verifies the signature, withdraws the price:
```
  @publicly!(Seller) let signature = sign(digest);
  withdraw! Seller <- price;
```

Note that the line `@publicly!(Seller) let signature = sign(digest);` above
is equivalent to the three statements below:
```
  @verifiably!(Seller) let signature = sign(digest);
  publish! Seller -> signature;
  verify! signature;
```

That is, in a first, `let`, statement, the `Seller`,
in the privacy of his personal computer,
signs the (digest of the) message, using his secret key.
Then, in a second, `publish!` statement, the `Seller`
publishes this signature onto the consensus.
Finally, in a third, `verify!` statement,
everyone verifies that the signature is valid.
If the signature isn't valid, the step fails,
the blockchain transaction is rejected by the smart contract,
and it is as if the seller didn't do a thing
(he can try again with a valid signature).

How does the `verify!` statement know what to verify?
Because the `let` was annotated with `@verifiably!`,
that records how to verify the definition.
Thus, the verification will be as if the programmer had written:
```
  require! isValidSignature(Seller, signature, digest);
```
Except that you don't need to audit that line, because the compiler did it for you
(you may still want to audit the compiler instead).
This is especially true as verifications can become involved,
and as the formulas to be verified may evolve through time,
and manual propagation of changes is sure to introduce subtle bugs sooner or later.
The use of `@verifiably!` thus makes for shorter, more reliable DApps.

How do we (the programmers, and the compiler) know there exactly two steps?
Because that's the number of changes in which participant is *active*:
in the first step, everything can be done by the `Buyer` in interaction with the consensus.
Then everything can be done by the `Seller` in interaction with the consensus.
This is a different participant, and therefore requires a separate transaction.
Each step will span as many statements and expressions as can be done
without changing the active participant.
Statements that are done solely by the consensus, such as the final withdrawal,
will take place without changing the current active participant, in the same transaction step.

In practice, the two participants must agree *off-chain* on the terms of the interaction.
Then, the first participant, in this case the `Buyer`,
creates the *on-chain* smart contract as part of enacting his first transaction step.
He then communicates the contract address to the `Seller` as part of an off-chain handshake.
The `Seller` verifies that the terms of the handshake indeed correspond to a previous agreement,
and then enacts his transaction. At the end, the contract concludes,
the `Buyer` can read the signature, and the `Seller` has been paid.

What if the `Buyer` never creates the contract?
Then he times out, and the `Seller` drops the interaction from his active set.
All he achieved was wasting his own time.

What if the `Seller` never gets the handshake or otherwise fails to publish the signature?
Then *he* times out, and the `Buyer` can invoke the contract to get his money back.
All that happened is the `Buyer` wasted some time and some transaction fees
to create and destroy the contract.

**Details on contract execution**

To invoke the contract at runtime:

Let’s suppose that two participant Alice and Bob agree
that Alice will buy a signature by Bob of a string that says:
`"I, Bob, sell item #101 to Alice"`, for 100 ethers (10**20 wei)
on the Cardano KEVM testnet.

First, the two participants use some authenticated private channels *off-chain*
to negotiate the terms of the agreement.
They encode this agreement as a standardized JSON object as follows,
where the hexadecimal bits are the ethereum addresses of the respective users,
and various other digests:

```
["interaction-agreement",
  {"glow-version": "v0.0-383-g83524b1",
    "interaction": "mukn/glow/dapps/buy_sig#payForSignature",
    "participants": {"Buyer": "0xC54e86DFFb87B9736E2E35DD85c775358F1c31CE",
                     "Seller": "0x9CcaEd210CE8c0Cb49c5Ad1C4f583406c264BA69"},
    "parameters": {"digest": "0x7a33c718fe7f3b9c56bd67b7b3e20fec6e3edf083626a7e11a10bba90243e405"
                   "price": "0x246ddf97976680000"},
    "reference": {"Buyer": "Purchase #42", "Seller": "Sale #101"},
    "parameters": {"wagerAmount": {"TestEther": 100}},
    "options": {"blockchain": "Cardano KEVM testnet",
                "escrowAmount": {"TestEther": 10},
                "timeoutInBlocks": 100,
                "maxInitialBlock": 61247},
    "code-digest": "0xaac1265d31e58390f2971bb58004f1944082116908ddb9c4a64be8b7d495c757"}]
```

Then, as the buyer and first participant, Alice creates the contract,
and sends a handshake containing verifiable contract creation information
together with the agreement as context.

```
["agreement-handshake",
  <… copy of the interaction agreement …>
  {"contract-address": "0xaf0FdEA3C5eF883dB314fb8d5c5cf2892c8efC30",
   "code-hash": "0xaac1265d31e58390f2971bb58004f1944082116908ddb9c4a64be8b7d495c757",
   "creation-hash": "0xdffd466220658c75bf7a300babd981599b0fd1c268a073239605b391bf3b396e",
   "creation-block": 61240}]
```

Bob's *Glow* agent will verify
that the handshake indeed corresponds to an agreement he had with Alice,
that the contract was indeed created as per the agreed upon parameters,
and was suitably confirmed on the blockchain.
The agent will then do the parts agreed upon.

## Rock, Paper, Scissors (RPS)

Here is a minimal variant of an interaction for Rock, Paper, Scissors.

In this well-known game, two participants play by each choosing one of three possible hands,
that they reveal at the same time.
Depending on the choices, one wins, or the other wins, or it’s a draw.

Now, there is no such thing as “at the same time” on the blockchain:
it is built on top of an *asynchronous* message passing network,
iu`;a,and simultaneity isn't available as a primitive.

Therefore, in a naive attempt to implement that interaction,
whichever participant shows his hand first is sure to lose,
as the other participant may make a last minute change to his hand so as to win,
[like this real-life robot](https://www.youtube.com/watch?v=ZVNnoOcohaU).

In our version of RPS we simulate simultaneity using a simple cryptographic technique called
a [commitment scheme](https://en.wikipedia.org/wiki/Commitment_scheme).

Let's call our two participants [Alice and Bob](https://en.wikipedia.org/wiki/Alice_and_Bob),
as per the tradition.

First, Alice chooses her hand, but does not reveal it.
Instead, she reveals a *commitment*:
a digest of her hand concatenated to a random salt by a cryptographic one-way hash function.

This commitment ensures that she cannot change her mind later,
but can only play the hand that she committed to.
Therefore Bob is confident that Alice cannot cheat.

The long enough salt ensures that Bob cannot feasibly guess her hand by trying
all the possible pre-images to the digest function: with no salt at all or a short salt,
Bob could just [brute force](https://en.wikipedia.org/wiki/Brute-force_attack) his way
into guessing Alice's hand by trying all the possibilities.
Therefore Alice is confident that she reveals no usable information to Bob with the commitment.

In a second step, Bob, can then safely reveal his hand without any possibility of cheating by either party.

Finally, Alice reveals her hand; the outcome is computed;
and the winner takes the pot of tokens, while the loser gets nothing---unless it’s a draw
at which point they each get their wager back.

The interaction is thus in three steps, as follows:

```
data Hand = | Rock | Paper | Scissors;

data Outcome = | B_Wins | Draw | A_Wins;

let winner = (handA : Hand, handB : Hand) : Outcome => {
    Outcome.ofNat((Hand.toNat(handA) + (4 - Hand.toNat(handB))) % 3) }

@interaction([A, B])
let rockPaperScissors = (wagerAmount) => {
  @A let handA = Hand.input("First player, pick your hand");
  @A let salt = randomUInt256();
  @verifiably!(A) let commitment = digest(salt, handA);
  publish! A -> commitment;
  deposit! A -> wagerAmount;

  @B let handB = Hand.input("Second player, pick your hand");
  publish! B -> handB;
  deposit! B -> wagerAmount;

  publish! A -> salt, handA;
  verify! commitment;
  let outcome = winner(handA, handB);
  switch(outcome) {
    | B_Wins => withdraw! B <- 2 * wagerAmount
    | Draw =>   withdraw! A <- wagerAmount;
                withdraw! B <- wagerAmount
    | A_Wins => withdraw! A <- 2 * wagerAmount }}
```

The contract starts with the familiar and mandatory line `#lang glow`.

The first two statements define two simple data types:

- The first one, `Hand`, is a sum type describing the choices
  that each participant can make as to a hand to play.

- The second one, `Outcome`, is a sum type describing
  the potential outcomes of the interaction.

Each of these types happens to have three *constructors*:

- a `Hand` can be `Rock`, `Paper` or `Scissors`.

- an `Outcome` can be `B_Wins`, `Draw` or `A_Wins`.

```
data Hand = | Rock | Paper | Scissors;

data Outcome = | B_Wins | Draw | A_Wins;
```

Implicitly defined are four functions:

1. `Hand.toNat`

2. `Hand.ofNat`

3. `Outcome.toNat`

4. `Outcome.ofNat`

These functions map between the constructors of each type and natural integers, in order.
Thus:

- `Hand.toNat(Rock)` is `0`
- `Hand.toNat(Paper)` is `1`
- `Hand.toNat(Scissors)` is `2`
- `Hand.ofNat(0)` is `Rock`
- `Hand.ofNat(1)` is `Paper`
- `Hand.ofNat(2)` is `Scissors`

Similarly, the functions for `Outcome` establish a correspondance between
`B_Wins`, `Draw` and `A_Wins` (respectively) with `0`, `1` and `2` (respectively).

These implicitly defined functions and the carefully chosen order of the outcomes
make it possible to compute the outcome with this “magic”-looking function:

```
let winner = (handA : Hand, handB : Hand) : Outcome => {
  Outcome.ofNat((Hand.toNat(handA) + (4 - Hand.toNat(handB))) % 3) }
```

Without using the conversion to integers, we could have used a more verbose definition
using pattern-matching as follows:

```
let winner = (handA : Hand, handB : Hand) : Outcome => {
  switch([handA, handB]) {
  | [Rock,     Rock]     => Draw
  | [Rock,     Paper]    => B_Wins
  | [Rock,     Scissors] => A_Wins
  | [Paper,    Rock]     => A_Wins
  | [Paper,    Paper]    => Draw
  | [Paper,    Scissors] => B_Wins
  | [Scissors, Rock]     => B_Wins
  | [Scissors, Paper]    => A_Wins
  | [Scissors, Scissors] => Draw }
```

Now comes the definition of the interaction `rockPaperScissors`.
As the annotation specifies, it has two roles called `A` and `B`,
and a single parameter, the `wagerAmount`.
We will keep calling Alice that participant with role `A`,
and Bob the participant with role `B`.

The first step is for Alice to enact, as follows:

```
  @A let handA = Hand.input("First player, pick your hand");
  @A let salt = randomUInt256();
  @verifiably!(A) let commitment = digest([salt, handA]);
  publish! A -> commitment;
  deposit! A -> wagerAmount;
```

Note that the variable definitions preceded by `@A`
only take place on Alice’s computer.
Their values are jealously kept secret by Alice,
who stashes them safely in her private database.
They are not revealed to Bob or to anyone,
until and unless Alice reveals them as part of the protocol,
if she ever does.
And indeed, the variable `commitment` is published
by the fourth statement `publish! A -> commitment;`

Moreover, notice how the commitment definition is annotated by `@verifiably!`.
This ensures that the formula used to define it can later be verified with `verify!`.

Also, in the beginning, the `input` statement means that Alice will choose a hand.
This invokes a private interaction on Alice's computer,
between the *Glow* runtime that handles the blockchain layer,
and the user interface of Alice's DApp.
Note that `input` statements are only allowed within the private context
of a single participant, in this case, `@A`.
It is not meaningful and not allowed by the language for an `input` statement
to happen within the consensual part of the interaction, without an `@<participant>` annotation.

In the middle, the `commitment` is computed using the special function `digest`,
applied to the pair of the cryptographic digest of the salt and hand.
A `digest` is computed by a “one-way hash function”,
such that the odds that two different values of the same type have the same digest
is astronomically unlikely.

In the end, Alice publishes her commitment, and deposits her wager into the contract,
that manages “the pot”.

The second step is simpler:

```
  @B let handB = Hand.input("Second player, pick your hand");
  publish! B -> handB;
  deposit! B -> wagerAmount;
```

Bob just reveals his hand and deposits his share into the pot.

The third step in only slightly involved:

```
  publish! A -> salt, handA;
  verify! commitment;
  let outcome = winner(handA, handB);
  switch(outcome) {
    | B_Wins => withdraw! B <- 2 * wagerAmount
    | Draw =>   withdraw! A <- wagerAmount;
                withdraw! B <- wagerAmount
    | A_Wins => withdraw! A <- 2 * wagerAmount }}
```

Alice reveals her salt and hand.

Everybody verifies that this is indeed correct
(or else Alice’s transaction attempt fails and it is as if she did nothing).
The outcome is computed, and depending on it, the pot is redistributed.

Now, what if Alice never starts the interaction? Then she times out,
and she just wasted a bit of her and Bob’s time.

What if Alice does her first step, but Bob never replies? Then Bob times out,
Alice invokes the contract to get her money back, and
all Bob did was waste a bit of her time and some gas she spent
to create the contract and get her money back.

What if Alice does the first step, Bob does the second step,
but Alice never does the third step? Then she times out, and
Bob can invoke the contract to get the entire pot since she’s in default.

Now, what if Alice, after Bob reveals his hand, realizes that she has lost?
Why should she bother to publish the last message? She loses anyway,
she could as well just let Bob waste his time and pay the transaction fees
to collect his winnings after she times out.

To disincentivize Alice from behaving in this way,
*Glow* can (and in the near future, will) automatically
transform the above contract into the below, wherein
Alice has to deposit an additional amount of money in escrow
to ensure she would lose more by timing out than by participating, and
that Bob would be compensated more this way than he would have wasted while waiting.
Compare with the above, and/or look for lines that include the new variable `escrowAmount`.

```
@interaction([A, B])
let rockPaperScissors = (wagerAmount, escrowAmount) => {
  @A let handA = Hand.input("First player, pick your hand");
  @A let salt = randomUInt256();
  @verifiably!(A) let commitment = digest(salt, handA);
  publish! A -> commitment;
  deposit! A -> wagerAmount;
  deposit! A -> escrowAmount;

  @B let handB = Hand.input("Second player, pick your hand");
  publish! B -> handB;
  deposit! B -> wagerAmount;

  publish! A -> salt, handA;
  verify! commitment;
  let outcome = winner(handA, handB);
  switch(outcome) {
    | B_Wins => withdraw! B <- 2 * wagerAmount
    | Draw =>   withdraw! A <- wagerAmount;
                withdraw! B <- wagerAmount
    | A_Wins => withdraw! A <- 2 * wagerAmount }
  withdraw! A <- escrowAmount }
```

Notice how Bob does *not* need to deposit an escrow,
because his first message is also his last message,
so there is no opportunity for him to stop cooperating after he started.
