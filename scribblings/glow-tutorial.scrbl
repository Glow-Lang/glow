#lang scribble/manual

@(require "glow-code.rkt"
          "glow-docs.rkt"
          (for-label glow))

@title{Glow Tutorial}

Welcome to Glow's tutorial.

Glow is a domain-specific language to write safe DApps (Decentralized Applications).
Here you will learn everything about this mighty language and its associated toolset
and perform feats previously inaccessible to mere mortals!

This document was updated on January 25th, 2021.

@section[#:tag-prefix "glow-tutorial"]{Intro}

@bold{Hello, there! Who are you?}

Hi, and welcome to Glow's tutorial.

To be as useful as possible, this tutorial has been designed with multiple layers of complexity,
depending on your level of proficiency in programming and in computer science:
this means you can re-read it as you get better at using Glow.
This tutorial has been written both
by nearly total-beginners, and computer science experts,
so that Glow can be learnt without being confusing.

If that's not enough, you can always:

@itemize[
 @item{join us on @hyperlink["https://discord.gg/Zx7p5Pp3yq"]{our discord channel}}
 @item{contact us directly: @tt|{contact@mukn.io}|}
]

So here's a little rundown...

@subsection{Level 1: I am new to computer programming}

Great! Your presence here means our product is becoming mainstream.

For now, though, Glow is a bit tricky to start programming,
but in the future, we plan to make tutorials that will take you from zero knowledge
to a point where you can code DApps with Glow without any uncertainty.
We want to make Glow as accessible as possible, and we'll do everything for you to reach this goal.

In the meantime, you can browse the Internet to learn the two prerequisites for Glow-programming:

@itemize[
 @item{The general notions of computer science and app-development.}
 @item{A basic understanding of how blockchain-related programming works.}
]

You can still read this documentation for business purposes and skip the parts that contain code.

@subsection{Level 2: I can make standard apps}

You're a developer with knowledge of the production of standard apps.
You know how to make one for your users,
but you lack the knowledge and experience in the field of DApps:
Glow comes with many typical smart-contracts that you can use in your DApps.

You just have to invoke one of our pre-made templates,
with parameters suitable to your use-case,
and our software will let you safely interact with the blockchain.

You'll be sure that your DApp's transactions are secure, so that you only have
to take care of what matters the most in any app:
its general design, its business logic, its front-end, etc.

While you won't need to program in the Glow language, you may still read this tutorial
to understand what Glow does for you and how it works, so you can fit
the functionality it provides into the architecture of your application.
Thus you can ultimately use Glow and its runtime as an efficient tool
that will save you time and money, a lot of a worrying uncertainty
about the risks you would otherwise have to face while building your DApp.

@subsection{Level 3: I can make DApps, and want to make my own smart-contracts}

You have read this tutorial and tried Glow for a while,
or you are already a bit experienced in blockchain-related programming.

This documentation will allow you to make your own smart-contracts:
you will develop your own versions of our tools,
for your own unique vision of smart-contracts.
The team behind Glow can audit and certify your smart-contracts for a fee,
if you want to make sure they are safe, and
want your potential users to trust it, too.

Each chapter of this tutorial links to sub-documents for Level-3 users.
Each time you want to be more autonomous and make the best use of Glow,
those links will give you advanced tips about
what our community and development team understood over time.

It'll save you precious tinkering-time, and
you'll be able to grasp Glow (and DApps) like never before.

@subsection{Level 4: I want to help develop Glow and understand all the details}

A contributor, are we? Well, welcome backstage:
each chapter of this tutorial contains sub-documents for level-4 users.

These sub-documents can contain either the source code of Glow (written in other languages)
or details about the thinking process we went through
(you'll find some high-level mathematical proofs,
links to our previous versions, and many other things).

In the end, these documents will allow you to contribute to Glow,
make your own version of it, or just audit it and know it as well as we do.

Feel free to share your progress with the community.

@section{Why use Glow?}

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
that can unaccountably collude against users is called a DApp,
which stands for Decentralized Application, and is pronounced either "dap" or "D-app".

A DApp is distributed around all the users of a network, and
compared to its centralized ancestor, it has many advantages:

@itemize[
 @item{Uncensorable, ad-less, bias-less, spam-less social networks.}
 @item{Peer-to-peer services where individual service providers are paid more.
       (because they don't pay monopoly rent to a centralized server).}
 @item{Increased safety of your asset transfers.}
 @item{Complete traceability of an asset, for supply chain managers, stock-management, or even lawyers.}
 @item{Enhanced privacy.}
]

@bold{Problem}

Compared to centralized programs,
DApps are both much harder to write and much less tolerant to mistakes.

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

@bold{Solution}

To make DApps more mainstream, the world needed a toolset to automate
all the critical parts of a DApp and make them safe.

This is precisely what Glow is:
it drastically simplifies the way a DApp is programmed, to make it safer.

@section{What is Glow for?}

@subsection{A programming language for DApps}

Glow is a programming language used to make DApps.
It comes with a wide set of tools.

Glow's three main specificities are:

@itemize[
 @item{@bold{Safety}: each newly implemented function is thoroughly audited
       so that a programmer using the Glow language or its associated toolset
       won't have to worry about securing interactions between users.}
 @item{@bold{User-friendliness}: we want to make DApps mainstream, and
       Glow's development team always aims at making it obvious what a DApp does or doesn't,
       so users can ascertain that it indeed matches their expectations.}
 @item{@italic{Portable to any blockchain}*: applications written in Glow
       can produce run on any blockchain, so you unlike with previous tools,
       you don't have to learn all the tooling specific to a single blockchain
       only to realize later the users you want will be on another blockchain.
       A same DApp will be able to use the resources of multiple blockchains at once,
       and meet the users where they are and wherever they will be.}
]

In other words: what usually takes a huge amount of lines of code,
and many experts in various fields, has been automated.
Some people will use contracts pre-made in Glow,
some others will make their contracts with a very safe language.

@subsection{What Glow can and can't do}

Glow is a language dedicated to the critical parts of DApps:
interactions between multiple participants who do not trust each other,
to exchange digital assets registered on decentralized ledgers,
according to rules verifiable by computers.

The role of the DApp is to create a sufficient trust between the participants
for them to actually partake in an interaction that they wouldn't otherwise have dared participate in,
based on their trust in the ledgers that maintain their digital assets,
based on their trusting how these ledgers supports "smart contracts"
that will enforce the rules of the interaction,
and based on their understanding of the rules of the interaction and agreement to them.

The role of Glow is to allow the participants to think of this interaction
in terms of the asset transfers that matter to them,
while taking care of all the difficult tasks required to translate this interaction
in terms of messages exchanged on asynchronous digital networks.

At the moment, Glow cannot be used to program every type of software, and isn't meant to do so.
Just like some languages are dedicated to specialized tasks
(e.g. R for data science and statistics), Glow's main focus is safe DApps,
including the underlying smart contracts, but not limited to them---DApps
also include client or server code for each participant, that must critically match the contract,
and exactly follow the same protocol, or someone will lose their assets.

For that reason, every time we add a new feature,
we carefully check if its addition is safe in the whole Glow ecosystem.
Maybe one day you will be able to program complete apps, full video games
and everything in Glow,
but for the moment, we focus on Glow's specialty,
so that DApps developed with Glow stay safe.

@subsection{More information}

@subsubsection{Official pages and documents}

Official website: @url{https://glow-lang.org}

Social media pages:

@itemize[
 @item{Twitter: @hyperlink["https://twitter.com/MutualKnowledge"]|{@MutualKnowledge}|}
 @item{Facebook: @hyperlink["https://facebook.com/MuKnSys/"]{facebook.com/MuKnSys/}}
 @item{Telegram: @hyperlink["https://t.me/mukn_io"]{t.me/mukn_io}}
 @item{Linkedin: @hyperlink["https://linkedin.com/company/mukn-io/"]{linkedin.com/company/mukn-io/}}
 @item{Youtube:
  @hyperlink["https://www.youtube.com/playlist?list=PLiEa4i6H-qcdosr5x1jPkGJRUVPB1GOlL"]{
   www.youtube.com/playlist?list=PLiEa4i6H-qcdosr5x1jPkGJRUVPB1GOlL}}
 @item{Discord: @url{https://discord.com/channels/655606984514469899/655606984967585832}}
]

And of course, to get more information about Glow,
you can read our @hyperlink["https://docs.google.com/document/d/1nBmI28yISX2HynodZnYWW0Px6re4JyYNNw2ncaFfJSg/edit"]{whitepaper}.

TODO: Social pages in Russian and Chinese (VK and Wechat) will come later

TODO: The documentation will be translated to French and perhaps other languages before June 2021.

TODO: Russian, Chinese and other translations will come late 2021

@subsubsection{Team and license}

Glow is Free and Open Source and will forever remain so.

Glow is distributed under the Apache License, version 2.0
(see the file @hyperlink["https://gitlab.com/mukn/glow/-/blob/master/LICENSE"]{LICENSE}).

Copyright © 2019 Mutual Knowledge Systems, Inc. All rights reserved.

US-based company Mutual Knowledge Systems, Inc. has been developing Glow, keeps developing it, yet doesn't want to have a legal monopoly over it: we just want to keep it accessible for all to see and audit, to use and adapt, to fix and improve, so you can trust it to indeed always be the best language in which to write DApps.

Check our @hyperlink["https://mukn.io/"]{website} to discover the human beings behind the code.

@subsubsection{Gitlab repo}

You can check our @hyperlink["https://gitlab.com/mukn/glow"]{gitlab repository} to know the code as well as we do.

@subsubsection{Our history}

Check our history to learn more about how we arrived there.

We also thoroughly keep
@hyperlink["https://gitlab.com/mukn/glow/-/blob/master/docs/bibliography.md"]{track}
of every source of reliable information about Glow.

@subsubsection{Glow's predecessor}

For the sake of history, you can learn more about our previous attempt
at building a safe and reliable language for DApp-development:
@hyperlink["https://gitlab.com/mukn/glow/-/blob/master/docs/alacrity.md"]{Alacrity}.

@subsubsection{Conceptual Basis}

We compiled and extensive list of what you should read
to understand our thought-process during the development of Glow.

We hope you'll enjoy reading our
@hyperlink["https://gitlab.com/mukn/glow/-/blob/master/docs/extra-bibliography.md"]{
 bibliography about relevant related subjects}.

Also, if you are keen on reading them, check what kind of
@hyperlink["https://gitlab.com/mukn/glow/-/blob/master/docs/proofs.md"]{
 mathematical safety proofs}
we are currently working on for DApps written in Glow.

@subsection{Environments}

@subsubsection{Use Glow in any browser}

TODO: In the near future, our code will run on any browser
on any desktop or laptop or mobile device,
with a nice user interface. But this is still work in progress.

@subsubsection{Use Glow locally on Linux and macOS}

At the moment, Glow can run on Linux and macOS.

Any Linux distribution will do: we use the @hyperlink["https://nixos.org/"]{Nix} package manager.
Nix can run for users on top of any Linux distribution (not just NixOS),
and also runs well on macOS.
Nix notably builds software in a deterministic way,
so that if it works for us it will work for you,
and if you experience a bug, we can reproduce it, too, and fix it.

TODO: We will soon add support for Windows.
Nix doesn't yet run out-of-the-box on Windows; given sufficient resources, it could be made to.
Glow can also be built without Nix, though making that run on Windows too will require resources.
Instead, we plan to have Glow target the JavaScript platform,
and be able to run on Windows that way.

@section{Getting started}

@subsection{Simplified install on Linux or macOS}

@subsubsection{Install Glow with only one command line}

To install Glow as a user or developer, go to a terminal window
(or any app on your system that allows you to enter shell commands).

Once there, type or copy/paste the following command line:

@verbatim|{
curl -L https://glow-lang.org/install/glow-install | sh
}|

If you use Linux or macOS on @tt{x86_64}, all the binary packages for the software we use
will be cached, and the installation should only take a few minutes,
depending on the speed of your Internet connection.
Note that this software may take above 2GB of memory,
so make sure you have enough space available on disk.

If you use another platform, your computer may recompile a lot of code from source
before the software is ready to run, especially the first time over.
Let it run overnight.

@bold{Safety}

This installation script first installs the @hyperlink["https://nixos.org/"]{Nix} package manager,
which may require you to manually type @tt{yes}, or @tt{y} and/or type the administrator password
around the beginning, to authorize parts of the installation. It is a normal part of the process.

Of course we trust the installation code, but you shouldn't until you audit it,
and so you would be more prudent to run it inside a virtual machine.
For extra security, you might want to use @hyperlink["https://www.qubes-os.org/"]{Qubes OS}.

@bold{Why the installation is that long (levels 3 and 4)}

The reason the installation is that long is that you are using a platform
for which we don't have precompiled binary packages, and so the installation
is going to recompile the entire compiler suite, starting with Gambit Scheme.

If you are using a computer with a supported target architecture, and it's recompiling,
then there must be a bug in our release discipline. Please contact us about it.

@bold{Configuring Nix to use our repository}

For alpha quality releases (the only ones at this moment),
we offer this nixpkgs channel:

@verbatim|{
export NIX_PATH=nixpkgs=http://github.com/muknio/nixpkgs/archive/alpha.tar.gz
}|

For the latest development code on the bleeding edge (which can be broken at times),
we offer this nixpkgs channel:

@verbatim|{
export NIX_PATH=nixpkgs=http://github.com/muknio/nixpkgs/archive/devel.tar.gz
}|

@bold{Building from source}

Once you have installed Glow and all its dependencies via Nix,
and have downloaded its source code, you can re-build it from source

@verbatim|{
nix-build
}|

Or you can install the resulting software with

@verbatim|{
nix-env -iA .
}|

@section{Trying it out with Docker (levels 3 and 4)}

When we have a stable release, we'll directly provide an image
@tt{mukn/glow:stable} on
@hyperlink["https://hub.docker.com/repository/docker/mukn/glow"]{Docker}.

In the meantime, you can build your own with:

@verbatim|{
docker build -t mukn/glow -f scripts/Dockerfile
}|

@section{Install Glow the hard way on other systems (levels 3 and 4)}

If you don't use either Linux or macOS, and can't use Docker,
you will have to build and install Glow the hard way:
installing GCC (usually easy), and a bunch of libraries (tedious),
on top of that Gambit Scheme with the correct options (requires care),
on top of it Gerbil Scheme, on top of it all Glow.

If you really want to try it this way, you can check our
@hyperlink["https://gitlab.com/mukn/glow/-/blob/master/INSTALL.md"]{
 hardcore installation file}.

@section{Language Overview}

@italic{Glow} is a domain-specific language for DApps, Decentralized Applications.
The language syntax is based on that of JavaScript,
with some inspiration from ReasonML where we have to diverge from JavaScript.
While it should feel familiar to you if you know JavaScript,
there are many differences underneath designed to keep your DApps safe:

@itemize[
 @item{
  The language is statically typed.
  Its type system is in the same general style as ReasonML or TypeScript,
  yet differs in many details, wherein we optimize our design for the safety of DApps.
 }
 @item{
  Though the syntax looks imperative, it is a function language underneath.
  In particular, you cannot modify bindings to existing variables,
  you can only have new variables shadow old variables.
  You also cannot side-effect data structures, only create new data structures
  that shadow the old data structures as those in consideration.
 }
 @item{
  Many features are currently missing, such as loops or recursion,
  that will be added slowly as the need arises.
 }
]

If you are used to programming in imperative languages, you will also find common concepts.

@italic{For programming language buffs}:
DApps are asynchronous interactions between multiple mutually-untrusting participant
manipulating digital assets on decentralized ledgers,
according to rules verifiable by blockchain smart contracts.
Semantically, Glow is an applicative language with a pure functional programming core
extended with a few primitives and annotations making it suitable for for multiparty computation.
It has a static type system based on MLsub.

If you are a seasoned developer, you can check our less-detailed and straight-to-the-point grammar,
our @secref["Glow_Language_Grammar"].
Otherwise the next chapters are designed to make you grasp it quickly.

@section{Fundamentals: Hello world!}

Unlike most languages, including most languages supposedly dedicated
to specifying DApps or just their smart contracts,
Glow is a language focused on interactions between multiple participants.
Hence, there is no typical @racket["hello world!"] program like in most languages,
but there is something alike: a @racket["hello world!"] for two...

Let's have a look at this very simple code:
@glowmodblock|{
#lang glow
@interaction([A, B])
let publishHello = () => {
  @A let ha = "Hello, B, I am A. How do you do?";
  publish! A -> ha;

  @B let hb = "Hello, A. I am B. How do YOU do?";
  publish! B -> hb;
};
}|

This is not a very useful DApp, but then neither is printing @racket["Hello, world"].
For a minimal useful DApp, see @secref{buy_sig} below.

Interactions are specified using annotations, that begin the "at-sign" (a.k.a. arobase): @"@"

If you remove the annotations, it looks like a regular JavaScript or ReasonML program,
except with those special publish! statements.

(Also, note that white spaces and interlines don't matter.
We didn't implement indentation-based structure, and don't intend to in the short-run.
It is too easy to introduce subtle with such semi-invisible structure;
we prefer bugs to remain painfully obvious, instead.)

The first line of this code specifies through an annotation
that the following function will be an interaction between two participants
the roles of which are called A and B.

You can see that semicolons are used to end statements.
They can be used to end every expression, but are mandatory only for statements.

The second line defines the interaction function.

The third and fifth lines define two private variables respectively named ha and hb
using the keyword let, each bound to a different byte string.
The former is initially private to A only,
while the latter is initially private to B only.

The fourth and sixth lines use the publish! statement to specify that
the respective sentences ha and hb will be sent
respectively on behalf of users A and B.

Note that these instructions will be executed line by line, so the dialog will happen like a real one.

@section[#:tag "buy_sig"]{Buying a Signature}

The simplest useful DApp you can write is one to buy and sell a signature.
This is what our example DApp @tt{buy_sig} below does.
Its practical applications include:

@itemize[
 @item{"Swaps" where you sign a message for use on another blockchain.}
 @item{Rental where you sign a message that grants you access to some digital resource.}
 @item{"Diploma", certification or other affidavit---also for purely digital goods.}
]

However, for applications that involve real-world goods and services,
you will want a slight modification of @tt{buy_sig} at the end of which
the price is deposited into another DApp:
an escrow that allows a trusted third-party or decentralized oracle
to be used as an arbitrator in case of later dispute between the two participants.

@glowmodblock|{
#lang glow
@interaction([Buyer, Seller])
let payForSignature = (digest : Digest, price : Nat) => {
  deposit! Buyer -> price;

  @publicly!(Seller) let signature = sign(digest);
  withdraw! Seller price;
};
}|

This contract starts with a special line @tt{#lang glow} to indicate that it is a glow program indeed.

Then we define a function @glowexp{payForSignature} with two parameters @glowexp{digest} and @glowexp{price},
in a syntax familiar to JavaScript, TypeScript or ReasonML programmers.
However, this definition is preceded by an annotation @glowexp|{@interaction([Buyer, Seller])}|
that indicates that this function involves a blockchain interaction
with two participant roles:
A first role called @glowexp{Buyer}, and a second role called @glowexp{Seller}.

This interaction involves @italic{two} steps, each step being
a blockchain transaction involving one participant and the on-chain smart contract.

In the first step, the @glowexp{Buyer} deposits the agreed-upon price in escrow into the smart contract,
with this statement:

@glowstmblock|{
  deposit! Buyer -> price;
}|

In the second step, the @glowexp{Seller} publishes their signature, and
after the contract verifies the signature, withdraws the price:

@glowstmblock|{
  @publicly!(Seller) let signature = sign(digest);
  withdraw! Seller <- price;
}|

Note that the line @glowstm|{@publicly!(Seller) let signature = sign(digest);}| above
is equivalent to the three statements below:

@glowstmblock|{
  @verifiably!(Seller) let signature = sign(digest);
  publish! Seller -> signature;
  verify! signature;
}|

That is, in a first, @glowexp{let}, statement, the @glowexp{Seller},
in the privacy of their personal computer,
signs the (digest of the) message, using their secret key.
Then, in a second, @glowexp{publish!} statement, the @glowexp{Seller}
publishes this signature onto the consensus.
Finally, in a third, @glowexp{verify!} statement,
everyone verifies that the signature is valid.
If the signature isn't valid, the step fails,
the blockchain transaction is rejected by the smart contract,
and it is as if the seller didn't do a thing
(they can try again with a valid signature).

How does the @glowexp{verify!} statement know what to verify?
Because the let was annotated with @glowexp|{@verifiably!}|,
that records how to verify the definition.
Thus, the verification will be as if the programmer had written:

@glowstmblock|{
  require! isValidSignature(Seller, signature, digest);
}|

Except that you don't need to audit that line, because the compiler did it for you
(you may still want to audit the compiler instead).
This is especially true as verifications can become involved,
and as the formulas to be verified may evolve through time,
and manual propagation of changes is sure to introduce subtle bugs sooner or later.
The use of @glowexp|{@verifiably!}| thus makes for shorter, more reliable DApps.

How do we (the programmers, and the compiler) know there exactly two steps?
Because that's the number of changes in which participant is @italic{active}:
in the first step, everything can be done by the @glowexp{Buyer} in interaction with the consensus.
Then everything can be done by the @glowexp{Seller} in interaction with the consensus.
This is a different participant, and therefore requires a separate transaction.
Each step will span as many statements and expressions as can be done
without changing the active participant.
Statements that are done solely by the consensus, such as the final withdrawal,
will take place without changing the current active participant, in the same transaction step.

In practice, the two participants must agree @italic{off-chain} on the terms of the interaction.
Then, the first participant, in this case the @glowexp{Buyer},
creates the @italic{on-chain} smart contract as part of enacting their first transaction step.
They then communicate the contract address to the Seller as part of an off-chain handshake.
The @glowexp{Seller} verifies that the terms of the handshake indeed correspond to a previous agreement,
and then enacts their transaction. At the end, the contract concludes,
the @glowexp{Buyer} can read the signature, and the Seller has been paid.

What if the @glowexp{Buyer} never creates the contract?
Then they time out, and the Seller drops the interaction from their active set.
All they achieved was wasting their own time.

What if the @glowexp{Seller} never gets the handshake or otherwise fails to publish the signature?
Then @italic{they} time out, and the @glowexp{Buyer} can invoke the contract to get their money back.
All that happened is the @glowexp{Buyer} wasted some time and some transaction fees
to create and destroy the contract.

