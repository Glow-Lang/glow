# Compiler Passes

This is the internal plan for how the compiler passes should be laid out.

The actual implementation is described in [passes.ss](passes.ss).


## Lexer and Parser

From Glow surface syntax text,

to Glow-sexpr syntax objects.

## Alpha-convert

From Glow-sexpr syntax objects with nested scope and name shadowing,

to Glow-sexpr syntax objects with unique names.

## Typecheck

From Glow-sexpr syntax objects with unique names,

to types for every top-level defined identifier.
May be extended later to types for every bound-occurrence of an
identifier and every sub-expression.

## ANF

From Glow-sexpr syntax objects with unique names,

to Glow-ANF form, flattened without nested function calls, etc.

## Transaction-group

From Glow-ANF form,

to Glow-TX form, with statements groupped into transactions initiated
by a single participant posting to the consensus.

## Data-encoding / Representation-selection

From Glow-TX form including datatypes,

to an IR with consistent sum and product data representation.

## End-point projection

From an IR with consistent data representation,

to programs for each participant and a program for the
consensus.

There will be more passes and intermediate-representations on each of
those projected programs.
