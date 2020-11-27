#lang scribble/manual

@title{Projection Sexpr}

@section{Grammar}

The @racketid[project] pass produces the
@racket[project.sexp] layer of s-expressions with the
following grammar:

@(racketgrammar*
  #:literals [\@
              \@module
              \@label def \@make-interaction λ deftype defdata
              consensus:set-participant participant:set-participant
              consensus:withdraw participant:withdraw
              expect-published expect-deposited
              add-to-publish add-to-deposit
              require! assert! ignore! return
              quote
              ann \@app \@dot \@list \@tuple \@record block switch
              == input digest sign
              _ \@var-pat \@app-ctor \@or-pat]
  [mod
   (\@module (id id) stmt ...)]
  [stmt
   (\@label id)
   (deftype id type)
   (deftype (id tyvar ...) type)
   (defdata id variant ...)
   (defdata (id tyvar ...) variant ...)
   (def id
     (\@make-interaction ((\@list id ...)) (id ...) (id id)
       (mpart body)
       ...))
   (def id (λ (id ...) body))
   (def id expr)
   (ignore! expr)
   (return expr)
   (consensus:set-participant id)
   (participant:set-participant id)
   (consensus:withdraw id arg-expr)
   (participant:withdraw id arg-expr)
   (expect-deposited arg-expr)
   (add-to-publish 'id arg-expr)
   (add-to-deposit arg-expr)
   (require! arg-expr)
   (assert! arg-expr)
   (switch arg-expr (pat body) ...)]
  [tyvar
   'id]
  [variant
   id
   (id type ...)]
  [mpart #f id]
  [body
   (code:line stmt ...)]
  [expr
   arg-expr
   (\@dot arg-expr id)
   (\@list arg-expr ...)
   (\@tuple arg-expr ...)
   (\@record (id arg-expr) ...)
   (expect-published 'id)
   (== arg-expr arg-expr)
   (input type arg-expr)
   (digest arg-expr ...)
   (sign arg-expr)
   (\@app arg-expr arg-expr ...)]
  [arg-expr
   id
   integer
   byte-string
   boolean
   (\@tuple)]
  [pat
   (ann pat type)
   (\@var-pat id)
   (\@app-ctor id pat ...)
   _
   (\@list pat ...)
   (\@tuple pat ...)
   (\@record (id pat) ...)
   (\@or-pat pat ...)
   integer
   byte-string
   boolean]
  [type
   id
   tyvar
   (\@tuple type ...)
   (\@record (id type) ...)
   (id type ...)])
