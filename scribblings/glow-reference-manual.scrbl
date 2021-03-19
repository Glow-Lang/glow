#lang scribble/manual

@(require syntax/parse/define
          "glow-code.rkt"
          "glow-docs.rkt"
          (for-syntax racket/base)
          (for-label glow))

@title{Glow Reference Manual}

@section{Basic Syntax}

@subsection[#:tag "hash-lang"]{The @hash-lang[] @racketmodname[glow] line}

Any program that you write in @(Glow) starts with the mandatory line:

@glowmodblock|{
#lang glow
}|

@subsection{Commenting your code}

To comment your code in @(Glow), @litchar{//} creates a line comment and
@litchar{/*} and @litchar{*/} create a block comment,
just like in ReScript and JavaScript:

@glowstmblock|{
// This is a line comment
}|

@glowstmblock|{
/* This is
a block comment */
}|

@subsection{Naming objects in @(Glow)}

A variable name is made one or more of the characters
@litchar{[_a-zA-Z0-9!$]} but must start with an alphabetic
character.

Moreover, the variable @racket[_] is reserved to denote a pattern
that matches anything but whose value is ignored. A few other names are reserved,
notably for the keywords of the language or a few languages that might be problematic.

There are terms that can't be used to name objects,
as they are already used as Keywords by either @(Glow) or other languages that interact with it:

@racket[abstract] @racket[arguments] @racket[assert!] @racket[boolean] @racket[break] @racket[byte] @racket[case] @racket[catch] @racket[class] @racket[char] @racket[const] @racket[continue] @racket[data] @racket[debugger] @racket[default] @racket[delete] @racket[deposit!] @racket[do] @racket[double] @racket[else] @racket[enum] @racket[eval] @racket[export] @racket[extends] @racket[false] @racket[final] @racket[finally] @racket[float] @racket[for] @racket[function] @racket[goto] @racket[if] @racket[implements] @racket[import] @racket[in] @racket[instanceof] @racket[int] @racket[interface] @racket[let] @racket[long] @racket[native] @racket[new] @racket[null] @racket[package] @racket[private] @racket[protected] @racket[public] @racket[publicly!] @racket[publish!] @racket[rec] @racket[require] @racket[require!] @racket[return] @racket[short] @racket[static] @racket[sum] @racket[super] @racket[switch] @racket[synchronized] @racket[this] @racket[throw] @racket[throws] @racket[transient] @racket[true] @racket[try] @racket[type] @racket[typeof] @racket[var] @racket[verifiably!] @racket[verify!] @racket[void] @racket[volatile] @racket[while] @racket[with] @racket[yield] @racket[withdraw!]

Capitalization is conventionally used for types, constructors and constants,
while regular variables conventionally start with lower-case letter and are in "CamelCase".
This convention isn't enforced by the language.

@section{Definitions}

@defglowstm[(let name expr) @{let name = expr;}]{
Defines @glowexp{name} and assigns it the value of @glowexp{expr}.

Example:
@glowstmblock|{
let c = 299792458;
}|

The @glowexp{name} is only defined for the scope
@emph{after} the let statement, so the @glowexp{expr} cannot
refer to @glowexp{name} recursively. If there is a previous
@glowexp{name}, the let statement shadows it for statements
after it, but the @glowexp{expr} will only refer to the
previous one.

Example:
@glowstmblock|{
let x = 1;
let x = x + 1;
}|

The @glowexp{x} in the @glowexp{x + 1} refers to the first
let statement defining it as @glowexp{1}, and so second let
statement defines @glowexp{x} as @glowexp{2}.
}

@defglowstm[(=> name params body) @{let name = (params) => { body };}]{
Defines @glowexp{name} as a function that takes the
@glowexp{params} as input and produces @glowexp{body} as
output.

Example:
@glowstmblock|{
let avg = (a, b) => {
    (a + b) / 2
};
}|

The @glowexp{name} is only defined for the scope
@emph{after} the let statement, so the @glowexp{body} cannot
refer to @glowexp{name} recursively.

In the future there will be an option for @tt{let rec} to
allow recursion, as described in @secref{Near-Future_Plans}.
}

@defglowstm[(\@interaction name participants params body)
            @|{@interaction([participants])
               let name = (params) => { body };}|]{
Defines @glowexp{name} as an interaction function that takes
the @glowexp{participants} and @glowexp{params} as input and
produces @glowexp{body} as output.

Example:
@glowstmblock|{
@interaction([A, B])
let publishHello = () => {
  @A let ha = "Hello, B, I am A. How do you do?";
  publish! A -> ha;

  @B let hb = "Hello, A. I am B. How do YOU do?";
  publish! B -> hb;
};
}|

The @glowexp{name} is only defined for the scope
@emph{after} the let statement, so the @glowexp{body} cannot
refer to @glowexp{name} recursively.
}

@section{Interaction Effects}

The Interaction Effects in this section are only valid
within interactions, or within the body of an
@glowexp|{@interaction}| statement.

@defglowstm[(publish! participant names) @{publish! participant -> names;}]{
For the given @glowexp{participant}, makes them the active
participant and sends a message with the values of all the
given @glowexp{names} to the consensus.

For all other participants, it receives the message and
defines the @glowexp{names} with the values from the
message.

For example in an interaction between @glowexp{A} and
@glowexp{B}, @glowstm|{publish! A -> x;}| makes @glowexp{A}
the active participant, has @glowexp{A} publish
@glowexp{x} to the consensus, and has @glowexp{B} receive
@glowexp{x} from the consensus.
}

@defglowstm[(deposit! participant amount) @{deposit! participant -> amount;}]{
For the given @glowexp{participant}, makes them the active
participant and deposits the given @glowexp{amount} of
their assets to the consensus.

For all other participants, it checks that the next message
to the consensus includes this deposit.

For example in an interaction between @glowexp{A} and
@glowexp{B}, @glowstm|{deposit! A -> 1;}| makes @glowexp{A}
the active participant, has @glowexp{A} deposit 1 unit of
their assets to the consensus, and has @glowexp{B} check
that the message included that deposit.
}

@defglowstm[(withdraw! participant amount) @{withdraw! participant <- amount;}]{
Withdraws the given @glowexp{amount} of assets from the
consensus and transfers it to the given
@glowexp{participant}. This does @emph{not} have to be the
active participant, it can be any participant in the
interaction.

For example in an interaction between @glowexp{A} and
@glowexp{B}, @glowstm|{withdraw! A <- 1; withdraw! B <- 2;}|
withdraws a combined @glowexp{3} units of assets from the
consensus and transfers @glowexp{1} to @glowexp{A} and
@glowexp{2} to @glowexp{B} without changing the active
participant.
}

@defglowstm[(\@publicly! participant name expr)
            @|{@publicly!(participant) let name = expr;}|]{
Defines @glowexp{name} from the given @glowexp{participant}'s
evaluation of @glowexp{expr}, in a way that can be published
and verified on the consensus.

Equivalent to:
@glowstmblock|{
@verifiably!(participant) let name = expr;
publish! participant -> name;
verify! name;
}|
}

@defglowstm[(\@verifiably! participant name expr)
            @|{@verifiably!(participant) let name = expr;}|]{
Defines @glowexp{name} privately for the given
@glowexp{participant} from their evaluation of
@glowexp{expr}. The @glowexp{name} remains associated with
the @glowexp{expr} by an equation that can be verified later
on the consensus with @glowexp{verify!} once the
@glowexp{name} and its dependencies in @glowexp{expr} have
all been published.
}

@defglowstm[(verify! names name) @{verify! names;}]{
Verifies the equation associated with each @glowexp{name} in
@glowexp{names}. Each equation is determined by the previous
@glowexp|{@verifiably!}| statement defining @glowexp{name}.
Each @glowexp{name} and its dependencies must all be
published beforehand, and the equations must hold.
}

@defglowstm[(require! expr) @{require! expr;}]{
Checks at runtime a predicate holds and if not fails the
active user's transaction. The predicate must be computable
at runtime by all the parties using their common knowledge,
so that the contract may verify it.
If some of the data comes from
user input, it is the user's responsibility to provide data
satisfying the predicate in a timely fashion.
Informally, @glowexp{require!} means
"if this fails, the active user is at fault".

In the future @glowexp{require!} will upgraded to check
whether @glowexp{expr} is satisfiable, as described in
@secref{Near-Future_Plans}.
}

@defglowstm[(\@participant participant stmt)
            @|{@participant stmt;}|]{
Evaluates the given @glowexp{stmt} privately for the given
@glowexp{participant}.

For example in an interaction between @glowexp{A} and
@glowexp{B}, @glowstm|{@A let x = 1;}| defines @glowexp{x}
privately to @glowexp{A}. Neither @glowexp{B} nor the
consensus can see @glowexp{x} until @glowexp{A} publishes
it.
}

@section{Conditionals}

@defglowexp[(if a b c) @{if (a) { b } else { c }}]{
Evaluates @glowexp{a} and produces @glowexp{b} if
@glowexp{a} is @glowexp{true}, or @glowexp{c} if @glowexp{a}
is @glowexp{false}.
}

@defglowexp[(switch val cases) @{switch (val) { cases }}]{
Evaluates @glowexp{val}, and pattern-matches it against each
case in @glowexp{cases} in order. When a pattern in a case
matches @glowexp{val}, it produces the case body associated
with it.

Example:
@glowstmblock|{
let status = (code) =>
  switch (code) {
  | 1 => "Info"
  | 2 => "Success"
  | 3 => "Redirect"
  | 4 => "Client Error"
  | 5 => "Server Error"
  | _ => "Other"
  };
}|

In the future, @glowexp{switch} will be upgraded to deal
with compound types, as described in
@secref{Near-Future_Plans}.
}

@section{Datatypes}

@subsection{Booleans and Equality}

@defglowexp[(Bool) @{Bool}]{
The type for booleans, containing the values @glowexp{true}
and @glowexp{false}.
}

@defglowexp[(true a b) @{true}]{
The @glowexp{Bool} value true, so that
@glowexp{if (true) { a } else { b }} evaluates @glowexp{a}.
}

@defglowexp[(false a b) @{false}]{
The @glowexp{Bool} value false, so that
@glowexp{if (false) { a } else { b }} evaluates @glowexp{b}.
}

Both values have to be typed in lowercase.

@defglowexp[(== a b) @{a == b}]{
The equality operation, produces @glowexp{true} if
@glowexp{a} is equal to @glowexp{b}, which must have the
same type.
}

@defglowexp[(! a) @{! a}]{
The @glowexp{Bool} "not" operation, produces @glowexp{false}
if @glowexp{a} is true, or @glowexp{true} if @glowexp{a} is
false.

@glowstmblock|{
! true == false
}|
}

@defglowexp[(&& a b) @{a && b}]{
The @glowexp{Bool} "and" operation, produces @glowexp{true}
if both @glowexp{a} and @glowexp{b} are true, or
@glowexp{false} otherwise. It is short-circuiting, just like
in JavaScript: it only evaluates the right-hand-side
expression @glowexp{b} if @glowexp{a} produces to
@glowexp{true}.
}

@defglowexp[(\|\| a b) @{a || b}]{
The @glowexp{Bool} "or" operation, produces @glowexp{true}
if either @glowexp{a} or @glowexp{b} are true, or
@glowexp{false} if both are false. It is also
short-circuiting just like in JavaScript: it only evaluates
the right-hand-side expression @glowexp{b} if @glowexp{a}
evaluates to @glowexp{false}.
}

@subsection{Integers}

@defglowexp[(Int) @{Int}]{
The type for integers.

The syntax is for literal integers is just decimal for the
moment: just write a non-empty sequence of digits from
@glowexp{0} to @glowexp{9}. For instance, @glowexp{0},
@glowexp{1}, @glowexp{2}, @glowexp{42}, @glowexp{1729} are
values of type @glowexp{Int}.

At this moment, @(Glow) only supports the type @defid[Nat]
which contains non-negative natural integers.

Furthermore, as an implementation limitation, when deploying
on Ethereum or a similar network, numbers are also
restricted to fit in 256 bits (or 32 bytes), the same as the
usual type @tt{UInt256} of Ethereum. Operations that
yield integers greater or equal to 2**256 or less than zero
will cause a runtime error.

In the future, @glowexp{Int} will allow negative integers
and @tt{BigInt}s, as described in
@secref{Near-Future_Plans}.
}

@subsubsection{Integer Arithmetic}

@defglowexp[(+ a b) @{a + b}]{
Produces the @glowexp{Int} addition of @glowexp{a} plus
@glowexp{b}.
}

@defglowexp[(- a b) @{a - b}]{
Produces the @glowexp{Int} subtraction of @glowexp{b} from
@glowexp{a}.
}

@defglowexp[(* a b) @{a * b}]{
Produces the @glowexp{Int} multiplication of @glowexp{a} and
@glowexp{b}.
}

@defglowexp[(/ a b) @{a / b}]{
Produces the @glowexp{Int} division of @glowexp{a} by
@glowexp{b}.
}

@defglowexp[(% a b) @{a % b}]{
Produces the @glowexp{Int} remainder of the euclidian
division of @glowexp{a} by @glowexp{b}.
}

TODO: @glowexp{a ** b} for exponentiation

Unlike in Javascript, @litchar{++} and @litchar{--} aren't in use, because
variables and data structure elements cannot be modified.
You can only compute new values and new data structures that may somehow shadow
the old values and data structures.

Addition and subtraction are not allowed to overflow.
An attempt to do so will be detected at runtime, result in an error, and
cause the transaction containing the overflow to be aborted,
at which point the interaction will remain at its previous state.

For instance, you can't divide by zero; for the moment, @(Glow) doesn't support negative numbers;
and currently on our Ethereum backend, all numbers must be less than 2**256.

Also note that you can't perform operations between strings and numbers:
you have to explicitly convert values from one type to another.

@subsubsection{Integer Comparisons}

@defglowexp[(< a b) @{a < b}]{
Produces @glowexp{true} when the @glowexp{a} is less than
@glowexp{b}, or @glowexp{false} otherwise.
}

@defglowexp[(> a b) @{a > b}]{
Produces @glowexp{true} when the @glowexp{a} is less than
@glowexp{b}, or @glowexp{false} otherwise.
}

@defglowexp[(<= a b) @{a <= b}]{
Produces @glowexp{true} when the @glowexp{a} is less than
or equal to @glowexp{b}, or @glowexp{false} otherwise.
}

@defglowexp[(>= a b) @{a >= b}]{
Produces @glowexp{true} when the @glowexp{a} is less than
or equal to @glowexp{b}, or @glowexp{false} otherwise.
}

@subsubsection{Bitwise Integer Operations}

@defglowexp[(&&& a b) @{a &&& b}]{
Produces the bitwise-and of @glowexp{a} and @glowexp{b}.
}

@defglowexp[(\|\|\| a b) @{a ||| b}]{
Produces the bitwise-or of @glowexp{a} and @glowexp{b}.
}

@defglowexp[(^^^ a b) @{a ^^^ b}]{
Produces the bitwise-xor of @glowexp{a} and @glowexp{b}.
}

@defglowexp[(~~~ a) @{~~~ a}]{
Produces the bitwise-not of @glowexp{a}.
}

@defglowexp[(<< a b) @{a << b}]{
Produces the bitwise-shift-right of @glowexp{a} by @glowexp{b}.
}

@defglowexp[(>> a b) @{a >> b}]{
Produces the bitwise-shift-left of @glowexp{a} by @glowexp{b}.
}

@subsection{Byte Strings}

@defglowexp[(Bytes) @{Bytes}]{
The type for byte strings made of finite sequences
(or arrays) of bytes (integers from 0 to 255 included).
}

For now, the syntax for byte strings is to write arbitrary
ASCII characters between double quotes.

As in:
@glowexpblock|{
"hello world!"
}|

Our syntax is currently limited to ASCII characters, but we
will soon add an escape syntax for arbitrary bytes.

Currently available escape sequences are :

@itemize[
 @item{@litchar{\"} for a double-quote (ASCII 34)}
 @item{@litchar{\n} for a newline (ASCII 10)}
 @item{@litchar{\\} for a backslash (ASCII 92)}
]

The syntax for a string is a double quote @litchar{"}
followed by unescaped ASCII characters or escape sequences.

Unescaped ASCII characters are any character in the ASCII
table from space (ASCII 32) to tilde (ASCII 126), except
double-quote @litchar{"} (ASCII 34) and backslash (ASCII 92)
that must be espaced.

Recognized escape sequences currently include @litchar{\"}
to represent the double-quote (ASCII 34) and @litchar{\\} to
represent a backslash (ASCII 92). We will support more
escape sequences and an alternate byte-oriented syntax in
the near future.

@bold{Important note}

Note that the choice of characters as non-negative 8-bit bytes, is deliberate:
DApps have to deal with digesting exactly well-defined sequences of bytes
in consensually executed code that where string validation,
like each bit of computation, costs literally millions of times more
to execute than code running in private on your local device.
In this context, concepts like Unicode, valid codepoints, UTF-8,
normalization forms, etc., are extremely expensive nonsense
that will rack up immense bills for no benefit whatsoever.
Thus, when interfacing between @(Glow) and other languages, make sure that
you represent @(Glow) bytestrings as sequences of bytes in your language, and
not "native" strings of Unicode or any such dangerously misfit type.

@subsection{Digests and Signatures}

@defglowexp[(Digest) @{Digest}]{
The type for digests, or hash-values produced by @glowexp{digest}.
}

@defglowexp[(digest exprs) @{digest(exprs)}]{
Produces a @glowexp{Digest} value representing the hash of
the values of the @glowexp{exprs}.

Two digests computed from two values should only be equal
iff the values are equal.

For example, @glowexp{digest(salt, hand)} produces a digest
will be the same when computed with the same salt and hand,
but different when computed with either different, so a
different salt with the same hand produces a different
digest, preventing rainbow-table attacks if both the salt
and hand are private.

Digests are often used with @glowexp|{@verifiably!}| and
@glowexp{verify!} to commit to a private choice that can't
be changed later:
@glowstmblock|{
    @verifiably!(A) let commitment = digest(salt, handA);
    publish! A -> commitment;
}|

Then later when they must show their hand it can be
published and verified:
@glowstmblock|{
    publish! A -> salt, handA;
    verify! commitment;
}|
}

@defglowexp[(Signature) @{Signature}]{
The type for signatures, produced by @glowexp{sign}.
}

@defglowexp[(sign participant digest) @{sign(digest)}]{
Within a private @glowexp{participant}'s evaluation,
produces a @glowexp{Signature} value confirming that the
@glowexp{participant} has signed the @glowexp{Digest}
@glowexp{digest}.
}

@defglowexp[(isValidSignature participant signature digest)
            @{isValidSignature(participant, signature, digest)}]{
Produces @glowexp{true} if @glowexp{signature} comes from
the given @glowexp{participant} signing the @glowexp{digest}
with @glowexp{sign(digest)}, or produces @glowexp{false}
otherwise.

For a given @glowexp{participant} and @glowexp{digest}, the
expression
@glowexp{isValidSignature(participant, sign(digest), digest)}
always produces @glowexp{true}.
}

@section{Near-Future Plans}

@subsection{Static Safety Analyses}

In the future, @(Glow) will help you prove the safety of your entire applications.
We will start by helping you prove safety invariants of your interactions
within the execution model of our language,
with static analyses that are discharged by a theorem solver.

Eventually, we want to be able to prove entire applications safe, from interface down to hardware,
which involves proving every program correct, including compilers and runtimes, and even chips.
However, for now, we are focusing on the most pressing issue for decentralized applications,
an issue that is not handled by previously existing tools, and that causes millions of dollars every year
to be lost to mistakes and frauds:
the correctness of the financial interactions between untrusting participants.
This involves not just the "contract" on the public blockchain, but
also the programs on each participant's private computer.

@subsubsection{Specifying Application Invariants}

@defglowstm[(assert! expr) @{assert! expr;}]{
@not-supported-yet[]

Will check at compile-time that the predicate alway holds
no matter what, as long as this point in the program was
reached while the current user and the contract indeed run
code generated by the @(Glow) compiler, and the underlying
infrastructure runs within the allowed parameters.
Informally, @glowexp{assert!} means
"if this fails, the program won't even start."
}

In the future, @glowexp{require!} will be upgraded so that
the @(Glow) compiler will reject the program unless it can
prove that the required assertion is indeed satisfiable for
the active user, and generate code that satisfies the
assertion.

In the future, @glowexp{assume!} will be another kind of assertion.
Within an @tt|{@A}| it will mean that the active user assumes the given predicate to be true.
Outside an @tt|{@A}| it will mean that every user assumes the given predicate to be true.
Informally, it will mean "you must not run this program unless you know this to be true."

@subsubsection{Logical Invariants}

In the future, you will be able to express the logical invariants for your programs,
as specified in the body of the assertions above, in terms of a modal logic
that will have several aspects:
@itemize[
@item{@emph{Functional Programming} for the regular predicate logic.}
@item{@emph{Concurrent Tree Logic} (CTL) to deal with the many possible executions paths.}
@item{@emph{Temporal Logic} to deal with timeouts.}
@item{@emph{Epistemic Logic} to deal with multiple participant knowing different things
  (starting with their secret keys).}
@item{@emph{Game Semantics} to express interactive proofs and adversarial behavior.}
@item{@emph{Linear Logic} to deal with resources not being arbitrarily created, duplicated or destroyed.}
@item{@emph{Game Theory} to express the Economic Equilibrium in.}
@item{@emph{Refinement Logic} Work at many abstraction levels (optional).}
@item{@emph{Finitary Logic} to enable zk-proof backends (optional).}
]

@subsubsection{Security properties we will prove about interactions written in @(Glow)}

@itemize[
@item{@emph{Non-negativity}:
  Amounts of assets in any account or sub-account are always non-negative.}
@item{@emph{Linearity}: Assets cannot be created or destroyed or duplicated or divided,
  only added via explicit deposits, removed via explicit deposits or transfered between accounts.}
@item{@emph{Balance}: The amount in sub-accounts of an interaction at the end of the interaction is 0.}
@item{@emph{Optimality}: Within the participants' respective explicit economic hypotheses,
  a/the optimal strategy for each participant is to keep executing his program.}
@item{@emph{Non-loss}: Within the participant's explicit economic hypotheses,
  whatever the actions of other participants, each participant's wallet will keep its value,
  except for the payment of transaction fees, which is bounded in gas
  (though the price of gas itself may float up).}
@item{@emph{Programmer-defined}: The programmer may specify additional
  application-dependent invariants of his own to the interaction.}]

The @(Glow) compiler will extract a specification from these invariants, and feed them to a theorem prover.
It will reject the program if the theorem prover fails to prove any of these invariants.
It will present any counter-examples found by the theorem provers in a way that is legible
in terms of execution traces with source-level binding assignments.

@subsubsection{Correctness of the @(Glow) implementation}

In a yet further future, we will want to prove the correctness of the @(Glow) compiler toolchain itself.
That will be another endeavour that will require much more capital investment than we can currently afford.
Yet we're confident that our approach will make it much cheaper, more general and more robust
than our competitors' approaches to eventually prove the correctness of an entire toolchain.

@subsection{Recursive Functions}

@defglowstm[(rec name params body)
            @|{let rec name = (params) => { body };}|]{
@not-supported-yet[]

Will define @glowexp{name} as a @emph{recursive} function
that takes the @glowexp{params} as input and produces
@glowexp{body} as output.

Example:
@glowstmblock|{
let rec factorial = (n) => {
  if (n == 0) {
    0
  } else {
    n * factorial(n - 1)
  }
};
}|
}

@subsection{Compound Data Types and Switch Patterns}

@defglowstm[(type Name Type) @{type Name = Type;}]{
@not-supported-yet[]

Will define @glowexp{Name} as a type alias for the given
existing @glowexp{Type}.

Example:
@glowstmblock|{
type Point2D = (Int, Int);
}|

The @glowexp{Name} will only be defined for the scope
@emph{after} the type statement, so the @glowexp{Type} cannot
refer to @glowexp{Name} recursively.
}

@defglowstm[(data Name Variants) @{data Name = Variants;}]{
@not-supported-yet[]

Will define @glowexp{Name} as a new algebraic data type with the
given @glowexp{Variants}.

It will be able to define enumerations:
@glowstmblock|{
data Hand = Rock | Paper | Scissors;
}|

Or structures containing values:
@glowstmblock|{
data Color = Rgb(Int, Int, Int);
}|

Or enumerations of structures, for example:
@glowstmblock|{
data Shape2D = Circle(Point2D, Int)
             | Triangle(Point2D, Point2D, Point2D)
             | Quadrangle(Point2D, Point2D, Point2D, Point2D);
}|

It is called an algebraic data type because the structures
form products of types, while the enumerations form sums,
making the new type a sum of products.

The @glowexp{Name} will be defined for the scope of the
@glowexp{Variants} as well as the scope after the data
statement, so the @glowexp{Variants} @emph{can} refer to
@glowexp{Name} recursively.
}

In the future, @glowexp{switch} will be upgraded to deal
with compound types including constructors from
@glowexp{data} types, for example:

@glowstmblock|{
type Point2D = (Int, Int);
data Shape2D = Circle(Point2D, Int)
             | Triangle(Point2D, Point2D, Point2D)
             | Quadrangle(Point2D, Point2D, Point2D, Point2D);

let center = (shape) =>
  switch (shape) {
  | Circle(c, r) => c
  | Triangle((x1,y1), (x2,y2), (x3,y3)) =>
    ((x1 + x2 + x3)/3, (y1 + y2 + y3)/3)
  | Quadrangle((x1,y1), (x2,y2), (x3,y3), (x4,y4)) =>
    ((x1 + x2 + x3 + x4)/4, (y1 + y2 + y3 + y4)/4)
  };
}|

@subsection{Integers with Negatives and BigInts}

In the future, the @glowexp{Int} type will allow negative
integers, and it will not be limited in size as it is now:
it will allow @tt{BigInt}s in JavaScript parlance
(or bignums as called in the Lisp tradition).

Nevertheless integer literals in @(Glow) do not and will not
take a final @litchar{n} at the end like they do in
JavaScript.

Also, we will support a richer set of integer types in the
near future, and will add hexadecimal syntax.

@subsection{Unit and Tuple Types}

@defglowexp[(Tuple Types A B C) @{(Types)}]{
@not-supported-yet[]

Creates a tuple type, a simple cartesian product of the
given @glowexp{types}. They contain as elements "tuples",
each made of one element of each of the types in the
product, in order. There can be any natural number of types
in a tuple type. @glowexp{()} is an empty tuple,
@glowexp{(A, B)} is a pair, @glowexp{(A, B, C)} is a triple,
and so on.

For example, the type @glowexp{(Bool, Nat, Bytes)} contains
such elements as @glowexp{(true, 4, "hello")} and
@glowexp{(false, 42, "world")}.

Do note that @glowexp{(A,B,C)} is different from both
@glowexp{(A,(B,C))} and @glowexp{((A,B),C)}: these types are
syntactically different and semantically disjoint;
just in a sense they are "isomorphic" since you can write a
reversible pair of functions that take one into the other
and vice versa.
}

@defglowexp[(Unit) @{Unit}]{
@not-supported-yet[]

Another name for the empty tuple type @glowexp{()}, contains
the single value @glowexp{()}.
}

@defglowexp[(tuple exprs) @{(exprs)}]{
@not-supported-yet[]

Will create a tuple as an expression. @glowexp{()} is an
empty tuple, @glowexp{(a, b)} is a pair, @glowexp{(a, b, c)}
is a triple, and so on.
}

@subsection{Record Types}

@defglowexp[(Record TypeEntries A B C) @{{ TypeEntries }}]{
@not-supported-yet[]

Creates a record type. @glowexp{{ x: A, y: B, z: C }} is a
record type with fields @glowexp{x}, @glowexp{y}, and
@glowexp{z}, where the types of the fields are @glowexp{A},
@glowexp{B}, and @glowexp{C} respectively.

The order of the fields does not matter for record types.

For the syntax see the @secref["Glow_Language_Grammar"].
}

@defglowexp[(record expr_entries) @{{ expr_entries }}]{
@not-supported-yet[]

Will create a record as an expression.
@glowexp{{ x: a, y: b, z: c }} creates a
record with fields @glowexp{x}, @glowexp{y}, and
@glowexp{z}, where the values of the fields are @glowexp{a},
@glowexp{b}, and @glowexp{c} respectively.

The order of the fields determines evaluation order, but
once evaluated, the field order doesn't matter for the value
itself. @glowexp{{ x: a, y: b } == { y: b, x: a }}.

For the syntax see the @secref["Glow_Language_Grammar"].
}

@include-section["glow-surface-grammar.scrbl"]
