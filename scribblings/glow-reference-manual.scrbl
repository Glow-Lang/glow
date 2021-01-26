#lang scribble/manual

@(require syntax/parse/define
          "glow-code.rkt"
          (for-syntax racket/base)
          (for-label glow))

@title{Glow Reference Manual}

@section{Basic Syntax}

@subsection[#:tag "hash-lang"]{The @hash-lang[] @racketmodname[glow] line}

Any program that you write in Glow starts with the mandatory line:

@glowmodblock|{
#lang glow
}|

@subsection{Commenting your code}

To comment your code in Glow, @litchar{//} creates a line comment and
@litchar{/*} and @litchar{*/} create a block comment,
just like in ReasonML and JavaScript:

@glowstmblock|{
// This is a line comment
}|

@glowstmblock|{
/* This is
a block comment */
}|

@subsection{Naming objects in Glow}

A variable name is made one or more of the characters
@litchar{[_a-zA-Z0-9]} and doesn't start with a digit.

Moreover, the variable @racket[_] is reserved to denote a pattern
that matches anything but whose value is ignored. A few other names are reserved,
notably for the keywords of the language or a few languages that might be problematic.

There are terms that can't be used to name objects,
as they are already used as Keywords by either Glow or other languages that interact with it:

@racket[abstract] @racket[arguments] @racket[assert] @racket[boolean] @racket[break] @racket[byte] @racket[case] @racket[catch] @racket[class] @racket[char] @racket[const] @racket[continue] @racket[data] @racket[debugger] @racket[default] @racket[delete] @racket[deposit] @racket[do] @racket[double] @racket[else] @racket[enum] @racket[eval] @racket[export] @racket[extends] @racket[false] @racket[final] @racket[finally] @racket[float] @racket[for] @racket[function] @racket[goto] @racket[if] @racket[implements] @racket[import] @racket[in] @racket[instanceof] @racket[int] @racket[interface] @racket[let] @racket[long] @racket[native] @racket[new] @racket[null] @racket[package] @racket[private] @racket[protected] @racket[public] @racket[rec] @racket[require] @racket[return] @racket[short] @racket[static] @racket[sum] @racket[super] @racket[switch] @racket[synchronized] @racket[this] @racket[throw] @racket[throws] @racket[transient] @racket[true] @racket[try] @racket[type] @racket[typeof] @racket[var] @racket[verify] @racket[void] @racket[volatile] @racket[while] @racket[with] @racket[yield] @racket[withdraw]

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
}

@defglowstm[(=> name params body) @{let name = (params) => { body };}]{
Defines @glowexp{name} as a function that takes the
@glowexp{params} as input and produces @glowexp{body} as
output.

Example:
@glowstmblock|{
let hypotenuse = (a, b) => {
    sqrt(sqr(a) + sqr(b))
};
}|
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
}

@defglowstm[(type Name Type) @{type Name = Type;}]{
Defines @glowexp{Name} as a type alias for the given
existing @glowexp{Type}.

Example:
@glowstmblock|{
type Point2D = (Int, Int);
}|
}

@defglowstm[(data Name Variants) @{data Name = Variants;}]{
Defines @glowexp{Name} as a new datatype with the given
@glowexp{Variants}.

It can be used to define enumerations:
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
}

@section{Interaction Effects}

@defglowstm[(publish! participant names) @{publish! participant -> names;}]{
For the given @glowexp{participant}, sends a message with
the values of all the given @glowexp{names} to the
consensus.

For all other participants, it receives the message and
defines the @glowexp{names} with the values from the
message.
}

@defglowstm[(deposit! participant amount) @{deposit! participant -> amount;}]{
For the given @glowexp{participant}, deposits the given
@glowexp{amount} of assets to the consensus.

For all other participants, it checks that the next message
to the consensus includes this deposit.
}

@defglowstm[(withdraw! participant amount) @{withdraw! participant <- amount;}]{
Withdraws the given @glowexp{amount} of assets from the
consensus and transfers it to the given
@glowexp{participant}.
}

@defglowstm[(\@publicly participant name expr)
            @|{@participant @publicly let name = expr;}|]{
Defines @glowexp{name} from the given @glowexp{participant}'s
evaluation of @glowexp{expr}, in a way that can be published
and verified on the consensus.

Equivalent to:
@glowstmblock|{
@participant @verifiably let name = expr;
publish! participant -> name;
verify! name;
}|
}

@defglowstm[(\@verifiably participant name expr)
            @|{@participant @verifiably let name = expr;}|]{
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
@glowexp{names}. Each @glowexp{name} and its dependencies
must all be published beforehand, and the equations must
hold.
}

@defglowstm[(assert! expr) @{assert! expr;}]{
Checks at compile-time that the predicate always hold no
matter what, as long as this point in the program was
reached while the current user and the contract indeed run
code generated by the Glow compiler, and the underlying
infrastructure runs within the allowed parameters.
Informally, @glowexp{assert!} means
"if this fails, the program won't even start."
}

@defglowstm[(require! expr) @{require! expr;}]{
Checks at runtime a predicate holds and if not fails the
active user's transaction. The predicate must be computable
at runtime by all the parties using their common knowledge,
so that the contract may verify it. The Glow compiler will
reject the program unless it can prove that the assertion is
indeed satisfiable for the active user, and generate code
that satisfies the assertion. If some of the data comes from
user input, it is the user's responsibility to provide data
satisfying the predicate in a timely fashion.
Informally, @glowexp{require!} means
"if this fails, the active user is at fault".
}

@defglowstm[(\@participant participant stmt)
            @|{@participant stmt;}|]{
Evaluates the given @glowexp{stmt} privately for the given
@glowexp{participant}.
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
type Point2D = (Int, Int);
data Shape2D = Circle(Point2D, Int)
             | Triangle(Point2D, Point2D, Point2D)
             | Quadrangle(Point2D, Point2D, Point2D, Point2D);

let dist = (a, b) =>
  switch (a, b) {
    ((x1, y1), (x2, y2)) => hypotenuse(x2 - x1, y2 - y1)
  };

let perimeter = (shape) =>
  switch (shape) {
  | Circle(center, radius) => 2 * pi * radius
  | Triangle(a, b, c)      => dist(a,b) + dist(b,c) + dist(c,a)
  | Quadrangle(a, b, c, d) => dist(a,b) + dist(b,c) + dist(c,d) + dist(d,a)
  };
}|
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

Integers in Glow are not limited in size:
they are @glowexp{BigInt}s in JavaScript parlance
(or bignums as called in the Lisp tradition).
Nevertheless integer literals in Glow do not take a final
@litchar{n} at the end like they do in JavaScript.

However, when compiling Glow to smart contracts running on a
blockchain,

At this moment, Glow only supports the type @defid[Nat]
which contains non-negative natural integers.

Furthermore, as an implementation limitation, when deploying
on Ethereum oro a similar network, numbers are also
restricted to fit in 256 bits (or 32 bytes), the same as the
usual type @glowexp{UInt256} of Ethereum. Operations that
yield integers greater or equal to 2**256 or less than zero
will cause a runtime error.

TODO: We will support a richer set of integer types in the
near future, and will add hexadecimal syntax.
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

For instance, you can't divide by zero; for the moment, Glow doesn't support negative numbers;
and currently on our Ethereum backend, all numbers must be less than 2**256.

Also note that you can't perform operations between strings and numbers:
you have to explicitly convert values from one type to another.

@defglowexp[(sqr a) @{sqr(a)}]{
Produces the square of the @glowexp{Int} @glowexp{a}, or
@glowexp{a * a}.
}

@defglowexp[(sqrt a) @{sqrt(a)}]{
Produces the @glowexp{Int} square-root of @glowexp{a}.
}

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

Note that the choice of characters as non-negative 8-bit
bytes, is deliberate: DApps have to deal with digesting
exactly well-defined sequences of bytes, and concepts like
Unicode, UTF-8, etc., only get in the way. When interfacing
between Glow and other languages, make sure that you
represent Glow bytestrings as sequences bytes in your
language, and not "native" strings of Unicode or any such
dangerously misfit type.

@subsection{Digests and Signatures}

@defglowexp[(Digest) @{Digest}]{
The type for digests, or hash-values produced by @glowexp{digest}.
}

@defglowexp[(digest exprs) @{digest(exprs)}]{
Produces a @glowexp{Digest} value representing the hash of
the values of the @glowexp{exprs}.

Two digests computed from two values should only be equal
iff the values are equal.
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
}

@subsection{Unit and Tuple Types}

@defglowexp[(Tuple Types A B C) @{(Types)}]{
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
Another name for the empty tuple type @glowexp{()}, contains
the single value @glowexp{()}.
}

@defglowexp[(tuple exprs) @{(exprs)}]{
Creates a tuple as an expression. @glowexp{()} is an empty
tuple, @glowexp{(a, b)} is a pair, @glowexp{(a, b, c)} is a
triple, and so on.
}

@subsection{Record Types}

@defglowexp[(Record TypeEntries A B C) @{{ TypeEntries }}]{
Creates a record type. @glowexp{{ x: A, y: B, z: C }} is a
record type with fields @glowexp{x}, @glowexp{y}, and
@glowexp{z}, where the types of the fields are @glowexp{A},
@glowexp{B}, and @glowexp{C} respectively.

The order of the fields does not matter for record types.
}

@defglowexp[(record expr_entries) @{{ expr_entries }}]{
Creates a record as an expression.
@glowexp{{ x: a, y: b, z: c }} creates a
record with fields @glowexp{x}, @glowexp{y}, and
@glowexp{z}, where the values of the fields are @glowexp{a},
@glowexp{b}, and @glowexp{c} respectively.

The order of the fields determines evaluation order, but
once evaluated, the field order doesn't matter for the value
itself. @glowexp{{ x: a, y: b } == { y: b, x: a }}.
}

@include-section["glow-surface-grammar.scrbl"]
