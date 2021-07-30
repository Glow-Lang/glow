This document proposes designs for future Glow compiler development,
with the goals of making Glow portable across multiple targets,
including EVM, Plutus Core, and possible other targets including
low-level virtual machines such as LLVM and WASM, and real hardware
ISAs such as RISC-V.

In particular, this document proposes:

- A low-level memory model and associated IR to be used when targeting
  lower-level machine-like platforms
- A higher-level IR, used in translating Glow to Plutus Core, but also
  as an intermediate step before the low level IR.

Where applicable, the document explores alternative design choices and
possible longer-term directions.

# Background And Design Considerations

Right now, all Glow code goes through several translation phases
culminating in the 'project' phase, which emits separate code for
each participant in an interaction, and for the consensus. This code is
then either interpreted (by the participants at runtime) or compiled
to EVM bytecode (by the consensus-code-generator) and run on the
blockchain.

Some salient properties of code emitted from `project` include:

- The code is in ANF; there are no complex expressions.
- Most semantically complex Glow-specific constructs (such as `verify!`)
  have been compiled out, with only more imperative constructs (such as
  `require!`) remaining. `withdraw!` and `deposit!` are still abstract
  operations.
- Participant changes happen via an imperative set-participant operation,
  rather than a declarative annotation on a set of statements.

It is somewhat ambiguous as to whether this form is closer to our low or
high level targets. Glow still lacks many of the features that would force
a distinction; we have no heap allocation and no recursion, so much of the
low level plumbing that might or might not be explicit is simply not
applicable. As the language evolves, we will have to decide where along
the translation path these become explicit. This proposal takes the
position that this should happen *after* the project phase, so we assume
that once they are implemented, `project` will still emit constructs
like lambdas and ADTs as opaque, primitive constructs.

## Design Considerations For Low-Level IR

This section details design considerations for the low-level IR & memory
model.

### Target Independence

Since the goal is to be able to translate the IR to multiple targets,
this IR must not be too closely tied to any one target.

### Amenable To Generating Efficient Code

The low level IR must facilitate generating efficient code for each
target platform.

### No Implicit Persistent State

All state that persists across transactions must be explicit. In
particular, we cannot rely on an implicit call and/or operand stack
across transactions; execution state must be stored explicitly.

### Consistent Data Layout

The IR emitted by the `project` phase has a data representation that is
consistent across participants and the consensus on the EVM. This
property will have to be maintained by the new low level IR & memory
model, for two reasons:

- To support UTXO chains, where the new state inherently must be
  computed off-chain.
- To allow us to continue to use an important optimization (detailed
  below) on the EVM, and anywhere else with similar cost considerations.

Note that it is possibly acceptable for the layout to be parametrized
over some target-platform specific details, such as endianness, word
size, etc, but the participants must agree with each other and with the
consensus on all of these.

#### Merklization Optimization

For background, one of the most expensive operations on EVM is the
SSTORE instruction, which writes data to persistent storage; most other
operations executed on the blockchain are orders of magnitude cheaper in
terms of gas.

To avoid having to pay to store the entire persistent state
of the consensus after each transaction, we instead compute a
cryptographic hash of the state, and store only that. When invoking
the contract, the participants must supply the complete persistent
state in the submitted transaction. The consensus checks this state
against its stored hash at the beginning of the transaction. In this
way, we only pay for one SSTORE instruction per transaction.

We call this the merklization optimization.

#### Interaction Multiplexing

MuKn has plans to multiplex difference instances of the same interaction
onto a single running Ethereum contract, and charge users a fee to use
the multiplexed contract, rather than pay the (high) gas costs to create
their own (see glow issue #157). The new low level IR must be able to
facilitate this. This will require being able to maintain both
interaction-local and contract-global state.

## Design considerations targeting Plutus Core

### Amenable To Generating Efficient Code

We would like Glow-on-Plutus to be reasonably efficient.

### Avoid Too Much Plutus-Specific Logic

We would like to avoid maintaining excessive amounts of Plutus-Specific
code in the Glow compiler.

### Use High-Level Facilities Of Plutus Core

As Glow adds high-level language features such closures, algebraic data
types, records, etc, low level backends will require additional runtime
support, for example for heap allocation and garbage collection.
However, Plutus Core supports some of these constructs (e.g. closures)
natively, and others can be encoded on top of Plutus' core features
(such as scott encoding ADTs). We should avoid imposing the low-level
constraints of other targets onto code targeting Plutus.

### State Comparison/Checking

On the current EVM target, we maintain state between transactions by
saving the program counter in memory, and hashing the raw bytes of
memory.  For other low level targets we can use a similar strategy.
However, Plutus is a lambda-calculus, and does not have a well defined
memory representation, so an alternative will have to be found.

In one way or another, we need to be able to store the contract state
between transactions in such a way that future transactions can restore
and verify it.

With Plutus as currently specified, this appears to be the most
difficult challenge in porting Glow to Plutus. Plutus currently provides
no facilities for serializing arbitrary terms, so any strategy here that
does not involve extensions to the platform is likely to incur
significant overheads, though we can do our best to minimise them.
Unfortunately, I(@isd) am not aware of a precise, documented cost model
for Plutus Core at present, so it is difficult to reason about this in
detail.

# Proposal

We define two additional IRs.

# Lambda Calculus

This section describes the higher-level IR, based on the lambda
calculus. This form has two purposes:

1. An intermediate stage in translation towards lower level targets.
2. A stage before targeting Plutus Core (which will not use the lower
   level IR at all).

## Overview

The high-level IR is recognizably lambda calculus. Noteworthy properties
include:

- Everything is still in ANF
- Lambdas have explicit capture lists (this will help when translating
  to low level IRs, and may help optimize serialization on plutus; see
  detailed discussion below).
- All side effects happen in an explicit effect monad, chained together
  with a monadic bind operator.
- Most high level constructs in Glow with simple operational semantics
  still exist as primitives at this level (e.g. ADTs/match, tuples,
  if/else, etc).
- The IR is not type checked. Down the line, we will likely introduce a
  typed variant, but we leave this for future work.

TODO: organize the rest of this.

One of these is a generic purely-functional lambda-calculus, that we'll
convert to plutus core or emit a lower-level IR separately. Rather than
imperative side-effecting statements such as `set-participant`,
`withdraw!` and so forth, we will have an explicit effect monad.
Code in this IR will remain in ANF. For example:

    (let [f (lambda ...)]
      (eff-bind
        (set-participant A)
        f))


    (let [g (lambda ...)]
      (eff-bind
        (deposit! ...)
        g))

When compiling this IR to Plutus, the effect monad will be a state
monad that tracks expected deposit and withdrawal amounts, much as
we currently do with global variables on EVM. For operations such
as `set-participant` which end the current transaction, Code emitted
for `eff-bind` will also be responsible for serializing the continuation
passed as its second argument and comparing it to the output of the
transaction.

Ideally, we would like IOG to add built-in functions we can call to
serialize & deserialize an arbitrary term, making this easy.

If we are unable to get such operations added, we could in the worst case
write an interpreter for our IR that runs on plutus, allowing us to
inspect the (interpreted) terms. This would introduce overheads, but
that is likely the case with any scheme we use to serialize
continuations. We may be able to optimize this such that some code
can be compiled more directly to plutus core (and treated as a builtin
by our interpreter), if it can be shown that doing so does not introduce
persistent state that cannot be serialized.

For now, this lambda calculus will be untyped, and we will target
untyped plutus core. There are two reasons for this:

- In the short term, propagating the types through the compiler is more
  work, and currently we have already lost some of this information by
  the time we get to `project`.
- Typed plutus core is based on System F-Omega, which introduces an
  impedance mismatch with Glow's type system, which is based on ML-sub.
  The problem is that the latter supports subtyping, while the former
  does not.

Longer term, we may introduce our own typed IR based on F-sub (System F
with  subtyping), and use evidence translation to compile away run-time
uses of subtyping -- independent of plutus, this could be helpful when
mapping things like records to lower-level memory representations in
lower level systems -- but we leave this as future work.

XXX

Cardano is a UTXO blockchain, where the on-chain script is a validator.
We will generate a script that computes the new state and then verifies
that it matches what's in the transaction (though in the future we can
also support state channels etc).

We will have to choose various encodings for higher-level constructs.
I suspect in the long term we will have our own IR based on F-sub. We
can use evidence translation to compile away subtyping when compiling
to the F-omega expected by Plutus core. But in the short term we can
also just do type erasure and compile to untyped plutus core, which
will work well at least until we end up needing to pick representations
of things like records. (we can also debate the benefits of going
through typed plutus core at all, but that's down the road).

## Rough stab (untyped)

exp ::=
    (lam v exp)
    (var v)
    (app exp exp)
    (eff-bind exp exp)
    (eff-pure exp)
    (eff-builtin eff-op exp...)
    (builtin op exp...)

eff-op ::=
    get-participant
    set-participant!
    deposit!
    withdraw!
    ...

op ::= ;; platform dependent; this can vary from one stage of compilation
       ;; to the next; we might start with *glow* builtin ops and then
       ;; translate them to the target platform.


When running on-chain, the effect monad itself will be implemented as a
combination reader + state monad, where reader part holds the data the
validator can see, and the state part contains the current state of the
contract, including balance, expected deposits, pending withdrawals.
When the effect-monad hits an operation that ends the transaction (e.g.
changing the participant) it verifies that the output state agrees with
both the monad's state and the continuation passed to bind. It also
verifies that withdrawals & deposits agree with the contents of the
transaction.

QUESTIONS:

- How do we check equality of the continuation on plutus?
  - This does not appear to be possible currently; we may actually just
    have to write a plutus contract that interprets our own IR, since we
    *need* to compare it. This introduces overhead, which may be a
    problem.
  - Alternatively, we could try to transform the necessary program state
    into a first-order form, though this is not entirely without
    overhead of its own. This is a possible route for optimization
    later, but doesn't make sense to implement in a first version. If
    we can convince IOG to add needed serialization primitives to
    plutus core itself, we may be able to skip this, and just scrap
    the interpreter entirely once those are ready.

# Low Level IR

This section describes the low level IR and memory model.

## Memory Model Overview

The memory model has some commonalities with executable formats such as
ELF. In particular, the format divides memory for statically allocated
variables into *sections*, based on the properties of variables:

- Does it persist across transactions?
- Is it interaction local, or is it going to be global once interaction
  multiplexing is implemented? (See #157 and related issues).
- Is the caller able to specify their own value for this, or is this
  protected by the consensus? Useful for passing in parameters.
- Is it merklization-safe?

Each section has an offset, and each variable has an offset within its
section. We sort the sections into memory regions as follows:

- Ephemeral (transaction-local) sections come first.
- Then come persistent, merklization-safe interaction-global (i.e. not
  interaction-local) variables.
- Then come persistent, merklization-safe interaction-*local* variables.
- Then come parameter sections.

Operations are limited to a fixed set of low-level types. We may be
able to get away with just integers of various fixed widths, but we
may also want to include:

- booleans
- (data)addresses
- function-labels

Code is grouped into functions, which have parameters, return values,
and a body:

    function-def =
        (function-name (params...) function-body)

    function-body =
        ; one or more blocks:
        (block...)

    block =
        (block label
            stmts...
            branch)

    branch =
        (return expr)
        (jump label)
        (tail-call function expr...)
        (switch expr ((value label) (value label)...) default-label?)


A statement is one of:

    (mstore variable expr)
    (ignore expr) ; evaluate an expression for its side effect,
                  ; ignoring any result.
    ...

We actually allow nested expressions at this level, again, relaxing the
ANF transformation we did earlier in the compiler. The reason for this
is that it makes it easy to generate code for the lower level layers:
Just do post-order tree traversal. If the target is a stack machine
(like EVM), we just push arguments, if it's SSA (like LLVM) we give each
subtree a name and do an assignment. This allows us to avoid superfluous
intermediate variables, which is important since at this stage we
allocate memory space for all variables -- so we can't optimize them out
later, it has to be now.

Memory *loads* can be expressions, but not memory *stores*, which are
statements.

Expressions are one of:

    (mload variable)
    (call function expr...)
    (builtin op expr...) ; misc built-in operators.
    ...

TODO: heap, dynamic segments.
