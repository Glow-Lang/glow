# Compiler Passes

This is the internal plan for how the compiler passes should be laid out.

The actual implementation is described in [passes.ss](passes.ss).


## Lexer and Parser

From Glow surface syntax text,

to Glow-sexpr syntax objects.

## Alpha-convert

From Glow-sexpr syntax objects with nested scope and name shadowing,

to Glow-sexpr syntax objects with unique names.

## Desugar

From Glow-sexpr with extra syntax niceties like `@verifiably`

to Glow-sexpr where those niceties have been expanded into simpler blocks.

`@verifiably` and `verify!` become verification of the original formula
or equivalent (for, e.g. signatures). `@publicly` becomes `@verifiably`.
`data` type definitions automatically generate an `input` function,
and `toNat` and `ofNat` functions if all constructors are simple.
Maybe `if`, `and` and `or` should be also converted to `switch`?

## Typecheck

From Glow-sexpr syntax objects with unique names,

to types for every top-level defined identifier.
May be extended later to types for every bound-occurrence of an
identifier and every sub-expression.

## ANF

From Glow-sexpr syntax objects with unique names,

to Glow-ANF form, flattened without nested function calls, etc.

## Transaction-grouping

From Glow-ANF form,

to Glow-ANF form, with extra safe-points before possible changes of active participant.

Safe-points and transitions between safe-points are the objects and morphisms (nodes and arrows)
of a category that embodies the game-theoretic and game-semantic properties of the interaction.
Optionally, in a further pre-BEPP pass, these transitions can be subdivided into chunks
that fit the current blockchain backend — which won't change these game-tastic properties.
The list of paths from a safe-point to its consecutive safe-points,
as annotated by the type of the arguments `publish!`ed along each path,
constitute the *session types* of these safe-points.

## Variable analysis

For each variable determine in which safe-points it is defined based on life-time analysis,
so that it will be stored (or merkleized) when saving a frame for that safe-point,
which happens either when reaching the safe point, or when reaching
a call that will later return to that safe point.

In this pass (or some latter one?), associate to each variable the scope in which it is defined.
Thus, it can be included or not in the representation for continuations, as stored in the contract,
either *in extenso* or merkleized.
The analyses could be done in several passes, and/or grouped with a unique pass that acts on them,
such as lambda-lifting and/or closure conversion:
recording a lambda-lifted function plus a partial list of parameters,
for which it is imperative to preserve the order at least to separate the early and late bound variables.

## Data-encoding / Representation-selection

From Glow-TX form including datatypes,

to an IR with consistent sum and product data representation.

For functions, consider that arguments are represented differently when passed from outside the contract
or inside the contract. There are clearly two kinds of compiled functions,
the "small ones" within a contract call or frame, that may even be inlined,
and the "big ones" across calls, that are (usually? always?) called with outside arguments.
This is quite similar to COBOL, where "normal" functions are quite restricted in how they can be called
(no recursion or reentrancy),
and if you want to use more complex control structures,
you must implement your own control stack,
with your own explicit representation of a continuation.
Actually, a fun exercise might be to actually represent our code at that level as COBOL syntax,
and boast that we have a COBOL backend.

## End-point projection

From an IR with consistent data representation,

to programs for each participant and a program for the
consensus.

There will be more passes and intermediate-representations on each of
those projected programs.

