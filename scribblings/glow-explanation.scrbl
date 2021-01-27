#lang scribble/manual

@title{Glow Explanation}

@section{Intro}

@(Glow) is a domain-specific language to write @emph{safe} DApps (Decentralized Applications).

This document aims to explain the principles behind @(Glow):

@itemize[
 @item{What its purpose is, what it aims to achieve for its users, what it doesn't try to do.}
 @item{What constraints it tries to abide by, what hypotheses underlie its design.}
 @item{How it is intended to be used, how to take advantage of its capabilities.}
 @item{How it works underneath the hood, and why those choices were made.}]

@section{Syntax}

The syntax of @(Glow) was designed to @bold{minimize surprise}.

If there is a familiar way to express things in JavaScript and similar languages,
and the according syntax is non-ambiguous, and has no pitfalls,
then @(Glow) shall use this syntax, too, to mean the very same thing.
Conversely, if some statement in @(Glow) looks like it means some thing to
practitioners of JavaScript and other such languages, then indeed,
that statement in @(Glow) must mean exactly that.

Goal: if the program is fishy, it should smell fishy.
@hyperlink["http://underhanded-c.org/"]{underhanded code} should be hard to write.

We don't expect that a lot of people would @em{write} new @(Glow) programs from scratch.
But we expect that a lot of people would @em{read} @(Glow) programs.

It is OK if it you need a little bit of learning to understand what a @(Glow) program did.
It is @emph{not} OK if before or after that little bit of learning, and based on your familiarity
with JavaScript or other languages, readers are easily confused about what the program does,
or make incorrect assumptions about it.

Clarity.
