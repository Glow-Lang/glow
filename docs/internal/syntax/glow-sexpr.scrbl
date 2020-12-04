#lang scribble/manual

@title{Glow Sexpr Syntax}

@section{Grammar}

The frontend parser for Glow will parse the ReasonML-like Glow
surface syntax into syntax objects in the following glow-sexpr
syntax:

@(racketgrammar*
  #:literals [\@
              def : λ deftype defdata
              publish! verify! require! assert! deposit! withdraw!
              quote
              ann \@dot \@list \@tuple \@record if block switch
              _ \@or-pat]
  [stmt
   (\@ attr stmt)
   (deftype id type)
   (deftype (id tyvar ...) type)
   (defdata id variant ...)
   (defdata (id tyvar ...) variant ...)
   (def id (λ (param ...) body))
   (def id (λ (param ...) : type body))
   (def id expr)
   (def id : type expr)
   (publish! id id ...)
   (verify! id ...)
   expr]
  [tyvar
   'id]
  [variant
   id
   (id type ...)]
  [param
   id
   (id : type)]
  [body
   (code:line stmt ... expr)
   (code:line stmt ...)]
  [expr
   (\@ attr decl)
   (ann expr type)
   id
   integer
   byte-string
   boolean
   (\@dot expr id)
   (\@list expr ...)
   (\@tuple expr ...)
   (\@record (id expr) ...)
   (block body)
   (if expr expr expr)
   (switch expr (pat body) ...)
   (id expr ...)
   (require! expr)
   (assert! expr)
   (deposit! id expr)
   (withdraw! id expr)]
  [pat
   (\@ attr pat)
   (ann pat type)
   id
   _
   integer
   byte-string
   boolean
   (\@list pat ...)
   (\@tuple pat ...)
   (\@record (id pat) ...)
   (\@or-pat pat ...)
   (id pat ...)]
  [type
   (\@ attr type)
   id
   tyvar
   (\@tuple type ...)
   (\@record (id type) ...)
   (id type ...)]
  [attr
   id
   (id expr ...)])

The @racket[\@] is from Javascripts @litchar|{@}| annotations / decorators.

The @racket[deftype] is from ReasonML's @racket[type] aliases
(not datatypes w/ variants).

The @racket[defdata] is from ReasonML's @racket[type] declarations with variants
(or Haskell's @racket[data] declarations).

The @racket[def] is from ReasonML's @racket[let].

The @racket[λ] is from ReasonML's @racket[=>] arrow functions.

The @racket[:] is from ReasonML's @racket[:] type annotations.

The @racket[tyvar]s are from ReasonML's @racket['a] type variables.

The @racket[block] is from ReasonML's @racket[{}] blocks.

The @racket[switch] is from ReasonML's @racket[switch] pattern matching.

The @racket[\@record] types are structural.

@section{Example}

The following surface-syntax program:
@verbatim|{
@interaction([Buyer, Seller])
let payForSignature = (digest : Digest, price : Assets) => {
  @Buyer deposit! price;
  @Seller @publicly let signature = sign(digest);
  withdraw! Seller price;
};
}|

would be parsed into this glow-sexpr representation:
@racketblock[
(\@ (interaction (\@list Buyer Seller))
   (def payForSignature
     (λ ((digest : Digest) (price : Assets)) ; inferred `: Unit`
       (\@ Buyer (deposit! price))
       (\@ Seller (\@ publicly (def signature (sign digest))))
       (withdraw! Seller price))))
]

