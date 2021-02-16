#lang scribble/manual

@(require "glow-code.rkt"
          "glow-docs.rkt"
          (for-label glow))

@(define em emph)

@title{Glow Explanation}

@section[#:tag-prefix "glow-explanation"]{Intro}

To quote the abstract of the
@hyperlink["https://j.mp/GlowWhitepaper2020"]{Glow Whitepaper}:
@blockquote{
We present @(Glow), a Domain-Specific Language (DSL) to develop
secure Decentralized Applications (DApps) on the blockchain.
Unlike existing languages, @(Glow) covers much more than a DApp’s “smart contract”:
the @(Glow) compiler also generates crucially matching client code,
and a logical model of your DApp so you can prove it correct.
Formal methods are not an afterthought in @(Glow),
they are built into the language and its implementation.
Furthermore, @(Glow)’s logic is designed to deal with
the inherently adversarial aspect of DApps,
that existing formal tools blatantly overlook.
Underlying @(Glow) is an architecture that in the future
will make it possible to prove correctness of @(Glow) itself,
and can later grow into a complete DApp Operating System.
@hyperlink["https://mukn.io"]{Mutual Knowledge Systems, Inc.}
is developing @(Glow) as an Open Source platform,
with an ambitious Business Model to become the go-to company for all blockchain developments.
}

We refer you to the aforementioned Whitepaper for much details on the technology behind @(Glow).
This document aims to give the somewhat technical reader
a good enough grasp of the principles behind @(Glow)
so they can decide what to use it for, when, and how:

@itemize[
 @item{What the purpose of @(Glow), what it aims to achieve for its users, what it doesn't try to do.}
 @item{What constraints it tries to abide by, what hypotheses underlie its design.}
 @item{How it works underneath the hood, and why those choices were made.}
 @item{How it is intended to be used, how to take advantage of its capabilities.}
]

@section{Why @(Glow)?}

This section presents the goals and purpose of @(Glow).
Why did the world need Yet Another Programming Language?
Because so far there were no adequate tools for the important purpose of building secure DApps.

@subsection{DApps: The Very Hardest Programs To Write!}

It is harder to write a secure Decentralized Application (or DApp, or dApp)
than any other piece of software. Here are the reasons why.

@subsubsection{Zero tolerance to bug}

For most software, a bug is just an issue you file,
that may or may not be fixed at the next release.
Even for serious bugs, it considered alright
to issue a fix after the fact in a matter of days,
and to stop using the software in the meantime if the bug is really serious.
By contrast, the "bug budget" for blockchain-facing parts of a DApp is zero:
DApps make high-stake irreversible transactions on public blockchains,
such that if a bug is found, users stand to lose all their assets at stake,
possibly millions of dollars, with no recourse whatsoever.
Any bug fix will come too late for these users.
Thus, in the famous second hack for the Parity Wallet,
one single bug was found in this relatively short contract of only 400 lines,
written by the greatest specialists, and users forever lost 280 M$ worth of tokens.

@subsubsection{All code is fragile}

Most software is written by developers used to writing bugs,
with tools designed to let bugs be written and be handled later.
Indeed, because they @em{can} afford to ship bugs, but they can't afford to ship late,
most developers are used to writing software that has bugs in them;
most programming languages, software development tools, programming methodologies
@em{don't even try} to write software that is bug-free;
they are not designed for that, not optimized for that.
This makes these languages, tools, methodologies—and developers—completely inappropriate
to write software that is fit to run on blockchains.
For that, you need software that is correct-by-construction,
with programmers who think like mathematicians, languages, tools and methodologies
that are designed around the purpose of writing correct software.
There are a few outliers, notably in the aerospace or biomedical industries,
who do care about correct software; but even they don't have it as hard as DApp developers.

@subsubsection{Active Adversaries}

Most software only has to survive naturally occurring accidents.
If corner case is only triggered by the simultaneous occurrence of a hundred adverse conditions,
then it doesn't need be handled correctly, because the event is astronomically unlikely.
Their "adversary" is Nature, passive.
But such is not the case for DApps.
The Internet is hostile, populated with black hat hackers, crackers, spies, thieves and mobsters,
who are eager to steal any available assets, and disrupt any process they can hold to ransom;
and these bad guys will contrive "attacks" that will ensure all required adverse conditions
occur simultaneously any time that DApp developers leave any remote corner case unhandled.
Any bug in a DApp is automatically a @em{bounty to the bad guys} in direct proportion
to how large are the assets controlled by that DApp.
A million dollar DApp automatically has a million dollar bounty on it for bad buys to break it.
The "adversary" is all the bad guys in the world, active.

@subsubsection{All code is public}

Military computer systems also feature high stakes and active adversaries.
But their code is top secret, guarded by armed men.
They can and do use social means to protect they software,
and their secrets are not freely accessible on the Internet (or so you hope).
By contrast, in a DApp, no secret is possible:
all parties have to see and audit the code to trust it;
you must share the entirety of your code with the party most interested in breaking it
and most capable of it: the other side of your transactions.
Any third party who is connected to the Internet can see all the code and all the data
that is published on the blockchain;
and if there is any vulnerability in your DApp, it will be impossible to hide.

@subsubsection{Usual reasoning techniques are lacking}

In most programs, you can safely assume that all the parts of a program are cooperating
to achieve the desired result. Bugs are not just unintentional, but counter-intentional.
These are assumptions so deeply ingrained that usual reasoning tools with which
advanced programmers prove their programs correct cannot even express the concept
of parts of the program working against each other, of there being divergent goals
for multiple actors, that are not necessarily aligned.
Not just the tools, but the theories on which they are based, are wholly inadequate
to even discuss whether or not a DApp is correct—they cannot even express
the concepts necessary to describe correctness, much less prove anything about them.
To reason about the correctness of DApps, you need specific extensions to the usual techniques,
that may have already been invented, but that no one so far have ever used together.


For all these reasons, we argue that indeed writing a secure DApp is
a much harder and much more expensive endeavor
than writing any other software of similar size.
This also means that for a given budget, you can only write DApps of much smaller size
and keep your users' assets secure.
Therefore, a tool that drastically reduces the size of a DApp required to achieve some given features
can unlock a universe of DApps currently impossible to achieve.

@subsection{DApps: Creating Trust Between Participants}

Before we go further, we need to characterize what is a DApp.

@bold{A DApp is an @em{interaction} between multiple @em{untrusting} participants,
exchanging @em{assets} on public ledgers, subject to algorithmically @em{verifiable} rules.}

@itemize[
  @item{It's an interaction:
    it involves multiple participants, who exchange messages with each other.
    Each participant possesses private information that the others don't possess,
    and publishes some information that the others learn, some of it unlocking assets.
  }
  @item{The participants don't trust each other. If they did, they wouldn't need
    a blockchain, a smart contract, or a DApp: they could just take each other's words.
    The entire point of the DApp is to create the trust required for them cooperate
    despite their mutual defiance.
  }
  @item{The interaction involves the control of assets on a public ledger,
    where all transactions are final and without recourse.
    There is no monopoly entity to racketeer honest participants,
    but also no higher authority to make things right when participants err.
    Therefore the DApp must be correct by construction for all parties.
  }
  @item{The interaction follows rules that can be automatically verified by a piece of code
    running on the blockchain, a "smart contract", that can thereby enforce the rules
    and hold all participants accountable for their compliance.
  }]

Note that the DApp involves much more than merely the "smart contract":
it involves all the code that runs on each of the participants' computers.
And the DApp does not create trust out of nothing:
it creates trust between participants out of a common trust by all participants
in the blockchain validation network and in the software that runs the DApps.
Therefore, the code of the blockchain and the code of the DApps must have been suitably audited
by people the participants trust, and the validation network must be properly capitalized,
while the participants are suitably collateralized.

@subsection{DApps: What it is not}

If an application doesn't involve a multiple participants interacting to exchange assets,
it is @em{not} a DApp.
If the same application could be rebuilt and deployed unchanged on a private cloud,
without a change for the users, then it isn't a DApp.

There are many projects for decentralized platforms on which to run distributed applications.
The serious ones among these projects are themselves DApps, and great ones.
But that does not automatically make applications running on top of them DApps,
anymore than running on top of the Linux kernel turns an application itself into the Linux kernel.

Most programs are @em{not} DApps, according to our narrow definition.
And that's good news: that means you can keep using
your regular developers, programming languages, tools and methodologies
to build and maintain most programs, and enjoy the economies of scale of using mainstream ecosystems.

But some programs @em{are} DApps.
And for @em{these} programs, you need to use adequate developers, languages, tools and methodologies,
that are unlike any of the usual ones.
For these programs, you need @(Glow).

@subsection{DApps: A Narrow Domain Fit for a DSL}

Another good news: because we have narrowly defined the domain DApps,
we can create tools that are specialized in this domain, can cover it completely,
and make it tractable.
The first such tool is a @em{Domain Specific Language}, or DSL.
And @(Glow) is the one we created.

A Domain Specific Language is programming language designed to express
exactly the programs that handle the problems in the given domain.
Developers can express the concepts that matter to the users
with enough precision to describe their solutions,
yet without all the ancillary details that do not matter.
Therefore programs can be as simple as possible yet cover all the domain.

Meanwhile, the evaluator for the DSL
(an interpreter or a compiler, often a mix of the two)
will take care of those details, automatically deriving solutions
to the usual subproblems of the domain from a combination of
the high-level specification from the programmer and the expertise
put in the evaluator by its authors.
Those details would be many costly months of work for regular developers using general purpose languages,
for which you'd get as many opportunities to introduce fatal bugs;
instead they become a few minutes of automated processing here and there,
where no bug is ever possible anymore.

The solution to building and maintaining large applications is often
to first identify the application's domain and build and maintain a DSL for it.
In the case of DApps, this strategy is not just a good idea,
it is imperative, to affordably and feasibly achieve the requirement of zero bugs.

@section{How @(Glow)?}

This section presents the constraints and hypotheses under which we designed @(Glow).
What guides the choices we make as we build and
@hyperlink["https://langev.com/pdf/steele99growing.pdf"]{grow this language}?
We strive toward clarity to make @(Glow) programs easy not just to write, but most importantly, to audit.
Programs should be concise and simple, yet without introducing imprecision, ambiguity or confusion.

@subsection{Building Trust}

The goal of @(Glow) is to enable the development of programs that all participants in a DApp can trust.
And they can trust it only if they or people they trust have audited the programs
and found them to be trustworthy. But what if their trust is misplaced?
Is it possible for a program to mean something else than what the auditors thought it meant?
Can you trust the trust that a program inspires?

A big goal in designing the syntax and semantics of @(Glow) is
to make it easy to assess that a correct program is indeed correct,
to make it very hard to misunderstand an incorrect program as being correct.
The first part means that it should be as simple as possible to express solutions.
But the second parts also means that it should be hard to write
@hyperlink["http://underhanded-c.org/"]{underhanded code}.
We strive to live by the principle that
@bold{fishy code should smell fishy}.

If there is a familiar way to express things in JavaScript and similar languages,
and the according syntax is non-ambiguous, and has no pitfalls,
then @(Glow) shall use this syntax, too, to mean the very same thing.
Conversely, if some statement in @(Glow) looks like it means some thing to
practitioners of JavaScript and other such languages, then indeed,
that statement in @(Glow) must mean exactly that.
In other words, @em{if} some @(Glow) code
looks like valid code in one of those familiar languages, then
it should be behave @em{the exact same way} in @(Glow) as in that language.
There may be exceptions to this principle (indeed, already are),
but it should be totally obvious @em{when} the behavior differs between
the @(Glow) and a familiar language, @em{how} it differs, and @em{why} it differs
(e.g. see Integer Literals below).

It is OK if it you need a little bit of learning to understand what a @(Glow) program did.
It is @emph{not} OK if before or after that little bit of learning, and based on your familiarity
with JavaScript or other languages, you are easily confused about what the program does,
or make incorrect assumptions about it.

A DApp language is used to express agreements upon which valuable assets are staked.
That is why the ultimate goal of DApp language is @bold{clarity} as to what the code does.

@subsection{Syntax: No Surprises}

The syntax of @(Glow) was designed to @bold{minimize surprise}.

We expect that few people will @em{write} new @(Glow) programs,
and even fewer people will write them from scratch.
On the other hand we expect that a lot of people will @em{read} @(Glow) programs,
and spend time trying to figure out what they do
and to assess that they do not do anything wrong.

We assume further that these people will be somewhat fluent in at least one of
JavaScript, TypeScript, ReasonML, OCaml, Haskell, PureScript,
and passably familiar with some others in the list.
Indeed, we assume that while writing or auditing a DApp,
people will switch a lot between parts written in @(Glow)
for the blockchain-facing aspects of the DApp,
and parts written in one of these other languages
for the non-blockchain-facing aspects.

Here are a few telling examples of syntactic choices we made,
to illustrate how we interpret this principle of "no surprise".

@subsubsection{Function Definition Syntax}

Our function definition syntax is the same as in ReasonML,
and almost the same as in JavaScript, modulo the fact that we allow type declarations:
`let f = x => x + y;`.

There is another function definition syntax in JavaScript that we purposefully do not use,
probably for the same reason as ReasonML:
`function f (x) { return x + y ;}`

Indeed in JavaScript the former syntax returns the value of the last expression,
whereas the latter returns a unit value when you fall through, and you have to explicitly use
a `return` statement to override that.

If we used this syntax and changed the semantics so we return the value of the last expression,
we would create great yet subtle confusion when "code-switching" between two languages,
i.e. reading in one language then the other, or adapting code from one language to the other.
This would cause many hours of wasted time, or much worse, bugs that get unnoticed into production.

We could accept the same syntax and semantics, but somehow reject the code unless we
can prove that the "fall through" case never happens. But that would require additional
sophistication in our code to correctly analyze this behavior, and our analysis might
be either too primitive and a pain to the user, or too elaborate and surprising to the user.
We could try to introduce this syntax and accompanying analysis later, but it's a lot of effort
for little gain, therefore not a priority when we already have a cheap compatible syntax.
Therefore, until further notice, we wholly reject this style of function definition syntax.

@subsubsection{Integer Literals}

For the longest time, JavaScript didn't have an "integer" type of its own.
Instead, all numbers are IEEE 754 double-precision floating point numbers,
64-bit total with a 53 bits of mantissa.
Historically, people have been using those numbers to represent
all contiguously integers from -2**53 to +2**53 —
though bitwise operations like `&` only preserve 32 bits of data.
But recent versions of JavaScript have introduced a new type of integer "BigInt";
for backward compatibility reasons (i.e. for existing code to still work),
BigInt is a disjoint type, and its literals have a "n" suffix to distinguish them,
so that @code{12345678901234567890} will be a floating point number, approximating away the low digits,
whereas @code{12345678901234567890n} will be an integer, preserving them.

In @(Glow), we only have BigInt-style integers and no floating point,
because blockchains casually operate on numbers up to 256-bit wide,
and some blockchains can even go beyond.
Like in ReasonML and unlike in JavaScript, our integer literals do not have this @code{n} suffix,
and it is very clear what we are doing.
Furthermore, our bitwise operations are tripled,
so @code{&&&} for bitwise-and, @code{|||} for bitwise-or, @code{^^^} for bitwise-xor,
and @code{~~~} for bitwise-not, so it is both very obvious to the reader what they do,
yet obvious that something must be changed when copying and pasting between @(Glow) and JavaScript,
where these operations only use 32 bits.

Thus, we did choose to introduce a discrepancy between @(Glow) and JavaScript
in the semantics of a same syntactic expression and the syntax of a same semantic operation,
but this discrepancy in the end makes things clearer,
with no possible ambiguity to the auditor, and obvious translation between languages.

@subsubsection{Scoping}

In @(Glow), the participant locality annotations such a @code{@Buyer} do not affect scoping rules.
A program stripped of its annotations is still a valid program that has the same effects,
though a less useful program with no actual exchange of assets.
The scoping rules are thus the same as in JavaScript or ReasonML.

Our design makes it much easier to reason about @(Glow) programs
than about programs in another language
where the syntax of JavaScript would have been preserved,
but the scoping rules would have been subtly modified.
That other language could cause major confusion when switching between languages,
and auditors might easily misunderstand what a program does,
opening opportunities for malicious underhanded code.

@section{What @(Glow)?}

@section{When @(Glow)?}

