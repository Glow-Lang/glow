#lang scribble/manual

@(require syntax/parse/define
          "glow-code.rkt"
          (for-syntax racket/base)
          (for-label glow))

@title{Glow Reference Manual}

@section{Overview}

@deftech{Glow} is a domain-specific language for DApps, Decentralized Applications.
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
It has a static type system based on MLsub, and static safety analyses
that are discharged by a theorem solver. (TODO: complete the static analyses.)

If you are a seasoned developer, you can check our less-detailed and straight-to-the-point grammar,
our
@hyperlink["https://gitlab.com/mukn/glow/-/blob/master/docs/internal/syntax/glow-surface.scrbl"]{surface grammar}.
Otherwise the next chapters are designed to make you grasp it quickly.

@section[#:tag "hash-lang"]{Fundamentals: Hello world!}

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

@subsection{Basic Syntax}

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

@itemize[
 @item{@racket[abstract]}
 @item{@racket[arguments]}
 @item{@racket[assert]}
 @item{@racket[boolean]}
 @item{@racket[break]}
 @item{@racket[byte]}
 @item{@racket[case]}
 @item{@racket[catch]}
 @item{@racket[class]}
 @item{@racket[char]}
 @item{@racket[const]}
 @item{@racket[continue]}
 @item{@racket[data]}
 @item{@racket[debugger]}
 @item{@racket[default]}
 @item{@racket[delete]}
 @item{@racket[deposit]}
 @item{@racket[do]}
 @item{@racket[double]}
 @item{@racket[else]}
 @item{@racket[enum]}
 @item{@racket[eval]}
 @item{@racket[export]}
 @item{@racket[extends]}
 @item{@racket[false]}
 @item{@racket[final]}
 @item{@racket[finally]}
 @item{@racket[float]}
 @item{@racket[for]}
 @item{@racket[function]}
 @item{@racket[goto]}
 @item{@racket[if]}
 @item{@racket[implements]}
 @item{@racket[import]}
 @item{@racket[in]}
 @item{@racket[instanceof]}
 @item{@racket[int]}
 @item{@racket[interface]}
 @item{@racket[let]}
 @item{@racket[long]}
 @item{@racket[native]}
 @item{@racket[new]}
 @item{@racket[null]}
 @item{@racket[package]}
 @item{@racket[private]}
 @item{@racket[protected]}
 @item{@racket[public]}
 @item{@racket[rec]}
 @item{@racket[require]}
 @item{@racket[return]}
 @item{@racket[short]}
 @item{@racket[static]}
 @item{@racket[sum]}
 @item{@racket[super]}
 @item{@racket[switch]}
 @item{@racket[synchronized]}
 @item{@racket[this]}
 @item{@racket[throw]}
 @item{@racket[throws]}
 @item{@racket[transient]}
 @item{@racket[true]}
 @item{@racket[try]}
 @item{@racket[type]}
 @item{@racket[typeof]}
 @item{@racket[var]}
 @item{@racket[verify]}
 @item{@racket[void]}
 @item{@racket[volatile]}
 @item{@racket[while]}
 @item{@racket[with]}
 @item{@racket[yield]}
 @item{@racket[withdraw]}
]

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
let pythag = (a, b) => {
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

@defglowstm[(type Name existing_type) @{type Name = existing_type;}]{
Defines @glowexp{Name} as a type alias for the
@glowexp{existing_type}.

Example:
@glowstmblock|{
type Point2D = (Int, Int);
}|
}

@defglowstm[(data Name variants) @{data Name = variants;}]{
Defines @glowexp{Name} as a new datatype with the given
@glowexp{variants}.

Example:
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

At this moment, Glow only supports the type @glowexp{Nat}
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

@subsection{Unit and Tuple Types}

@defglowexp[(tuplet types) @{(types)}]{
Creates a tuple type. @glowexp{()} is an empty tuple,
@glowexp{(a, b)} is a pair, @glowexp{(a, b, c)} is a triple,
and so on.
}

@defglowexp[(Unit) @{Unit}]{
Another name for the empty tuple type @glowexp{()}, contains
the single value @glowexp{()}.
}

@defglowexp[(tuplee exprs) @{(exprs)}]{
Creates a tuple as an expression. @glowexp{()} is an empty
tuple, @glowexp{(a, b)} is a pair, @glowexp{(a, b, c)} is a
triple, and so on.
}

@subsection{Record Types}

@defglowexp[(recordt x type_entries) @{{type_entries}}]{
Creates a record type. @glowexp{{ x: a, y: b, z: c }} is a
record type with fields @glowexp{x}, @glowexp{y}, and
@glowexp{z}, where the types of the fields are @glowexp{a},
@glowexp{b}, and @glowexp{c} respectively.

The order of the fields does not matter for record types.
}

@defglowexp[(recorde expr_entries) @{{expr_entries}}]{
Creates a record as an expression.
@glowexp{{ x: a, y: b, z: c }} creates a
record with fields @glowexp{x}, @glowexp{y}, and
@glowexp{z}, where the values of the fields are @glowexp{a},
@glowexp{b}, and @glowexp{c} respectively.

The order of the fields determines evaluation order, but
once evaluated, the field order doesn't matter for the value
itself. @glowexp{{ x: a, y: b } == { y: b, x: a }}.
}

@section{buy_sig}

@glowmodblock|{
#lang glow
@interaction([Buyer, Seller])
let payForSignature = (digest : Digest, price : Assets) => {
  @Buyer deposit! price;
  @Seller @publicly let signature = sign(digest);
  withdraw! Seller price;
};
}|

@include-section["glow-surface-grammar.scrbl"]
