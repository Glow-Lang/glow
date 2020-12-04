#lang scribble/manual

@(require scribble/bnf
          syntax/parse/define)
@(define-simple-macro (defs [x:id e:expr] ...)
   (begin (define x e) ...))

@title{Glow Surface Syntax}

@section{Grammar}

This surface syntax is inspired by ReasonML, Javascript, and Haskell,
in that order. Most forms based on ReasonML. However, @litchar|{@}|
attributes are based on Javascript, and datatype definitions based on
a combination of ReasonML and Haskell.

The semantics corresponding to these forms will be most similar to
ReasonML.

@;; Literal Tokens
@(defs
   [|(| @litchar{(}]
   [|)| @litchar{)}]
   [|[| @litchar{[}]
   [|]| @litchar{]}]
   [|{| @litchar|{{}|]
   [|}| @litchar|{}}|]
   [|,| @litchar{,}]
   [|;| @litchar{;}]
   [|@| @litchar|{@}|]
   [|'| @litchar{'}]
   [\| @litchar{|}]
   [|.| @litchar{.}]
   [= @litchar{=}]
   [_ @litchar{_}]
   [=> @litchar{=>}]
   [-> @litchar{->}]
   [<- @litchar{<-}]
   [: @litchar{:}]
   [type @litchar{type}]
   [data @litchar{data}]
   [let @litchar{let}]
   [if @litchar{if}]
   [else @litchar{else}]
   [switch @litchar{switch}]
   [publish! @litchar{publish!}]
   [verify! @litchar{verify!}]
   [require! @litchar{require!}]
   [assert! @litchar{assert!}]
   [deposit! @litchar{deposit!}]
   [withdraw! @litchar{withdraw!}])

@(BNF
  (list
   @nonterm{id}
   @elem{name that isn't a reserved keyword or symbol})
  (list
   @nonterm{ids}
   (BNF-seq)
   (BNF-seq @nonterm{id} (kleenestar (BNF-group |,| @nonterm{id}))))
  (list
   @nonterm{stmt}
   (BNF-seq |@| @nonterm{attr} @nonterm{stmt})
   (BNF-seq type @nonterm{id} = @nonterm{type})
   (BNF-seq type @nonterm{id} |(| @nonterm{typarams} |)| = @nonterm{type})
   (BNF-seq data @nonterm{id} = @nonterm{variants})
   (BNF-seq data @nonterm{id} |(| @nonterm{typarams} |)| = @nonterm{variants})
   (BNF-seq let @nonterm{id} =
            |(| @nonterm{params} |)| => @nonterm{expr})
   (BNF-seq let @nonterm{id} =
            |(| @nonterm{params} |)| : @nonterm{type} => @nonterm{expr})
   (BNF-seq let @nonterm{id} = @nonterm{expr})
   (BNF-seq let @nonterm{id} : @nonterm{type} = @nonterm{expr})
   (BNF-seq let @nonterm{id} = @nonterm{expr})
   (BNF-seq publish! @nonterm{id} -> @nonterm{ids})
   (BNF-seq verify! @nonterm{ids})
   @nonterm{expr}
   )
  (list
   @nonterm{typarams}
   (BNF-seq)
   (BNF-seq @nonterm{tyvar} (kleenestar (BNF-group |,| @nonterm{tyvar})))
   )
  (list
   @nonterm{tyvar}
   (BNF-seq |'| @nonterm{id})
   )
  (list
   @nonterm{variants}
   (BNF-seq)
   (BNF-seq (optional \|) @nonterm{variant} (kleenestar (BNF-group \| @nonterm{variant})))
   )
  (list
   @nonterm{variant}
   @nonterm{id}
   (BNF-seq @nonterm{id} |(| @nonterm{arg-types} |)| )
   )
  (list
   @nonterm{params}
   (BNF-seq)
   (BNF-seq @nonterm{param} (kleenestar (BNF-group |,| @nonterm{param})))
   )
  (list
   @nonterm{param}
   @nonterm{id}
   (BNF-seq @nonterm{id} : @nonterm{type})
   )
  (list
   @nonterm{expr}
   (BNF-seq |@| @nonterm{attr} @nonterm{expr})
   (BNF-seq |(| @nonterm{expr} |)|)
   (BNF-seq @nonterm{expr} : @nonterm{type})
   @nonterm{id}
   @nonterm{integer}
   @nonterm{byte-string}
   @nonterm{boolean}
   (BNF-seq @nonterm{expr} |.| @nonterm{id})
   (BNF-seq |[| @nonterm{arg-exprs} |]|)
   (BNF-seq |(| @nonterm{arg-exprs} |)|)
   (BNF-seq |{| @nonterm{record-expr-entries} |}|)
   (BNF-seq |{| @nonterm{body} |}|)
   (BNF-seq if |(| @nonterm{expr} |)| |{| @nonterm{body} |}| else |{| @nonterm{body} |}|)
   (BNF-seq if |(| @nonterm{expr} |)| |{| @nonterm{body} |}|)
   (BNF-seq switch |(| @nonterm{expr} |)| |{| @nonterm{cases} |}|)
   (BNF-seq @nonterm{expr} |(| @nonterm{arg-exprs} |)| )
   (BNF-seq require! @nonterm{expr})
   (BNF-seq assert! @nonterm{expr})
   (BNF-seq deposit! @nonterm{id} -> @nonterm{expr})
   (BNF-seq withdraw! @nonterm{id} <- @nonterm{expr}))
  (list
   @nonterm{arg-exprs}
   (BNF-seq)
   (BNF-seq @nonterm{expr} (kleenestar (BNF-group |,| @nonterm{expr})))
   )
  (list
   @nonterm{record-expr-entries}
   (BNF-seq)
   (BNF-seq @nonterm{record-expr-entry} (kleenestar (BNF-group |,| @nonterm{record-expr-entry}))))
  (list
   @nonterm{record-expr-entry}
   (BNF-seq @nonterm{id} : @nonterm{expr}))
  (list
   @nonterm{body}
   (BNF-seq (kleenestar (BNF-group @nonterm{stmt} |;|)) @nonterm{expr})
   (BNF-seq (kleenestar (BNF-group @nonterm{stmt} |;|))))
  (list
   @nonterm{cases}
   (BNF-seq)
   (BNF-seq (optional \|) @nonterm{case} (kleenestar (BNF-group \| @nonterm{case}))))
  (list
   @nonterm{case}
   (BNF-seq @nonterm{pat} => @nonterm{body}))
  (list
   @nonterm{pat}
   (BNF-seq |@| @nonterm{attr} @nonterm{pat})
   (BNF-seq |(| @nonterm{pat} |)|)
   (BNF-seq @nonterm{pat} : @nonterm{type})
   @nonterm{id}
   _
   @nonterm{integer}
   @nonterm{byte-string}
   @nonterm{boolean}
   (BNF-seq |[| @nonterm{arg-pats} |]|)
   (BNF-seq |(| @nonterm{arg-pats} |)|)
   (BNF-seq |{| @nonterm{record-pat-entries} |}|)
   (BNF-seq @nonterm{pat} (kleenestar (BNF-group \| @nonterm{pat}))))
  (list
   @nonterm{arg-pats}
   (BNF-seq)
   (BNF-seq @nonterm{pat} (kleenestar (BNF-group |,| @nonterm{pat}))))
  (list
   @nonterm{record-pat-entries}
   (BNF-seq)
   (BNF-seq @nonterm{record-pat-entry} (kleenestar (BNF-group |,| @nonterm{record-pat-entry}))))
  (list
   @nonterm{record-pat-entry}
   (BNF-seq @nonterm{id} : @nonterm{pat}))
  (list
   @nonterm{type}
   (BNF-seq |@| @nonterm{attr} @nonterm{type})
   (BNF-seq |(| @nonterm{type} |)|)
   @nonterm{id}
   @nonterm{tyvar}
   (BNF-seq |(| @nonterm{arg-types} |)| )
   (BNF-seq |{| @nonterm{record-type-entries} |}| )
   (BNF-seq @nonterm{id} |(| @nonterm{arg-types} |)| ))
  (list
   @nonterm{arg-types}
   (BNF-seq)
   (BNF-seq @nonterm{type} (kleenestar (BNF-group |,| @nonterm{type}))))
  (list
   @nonterm{record-type-entries}
   (BNF-seq)
   (BNF-seq @nonterm{record-type-entry} (kleenestar (BNF-group |,| @nonterm{record-type-entry}))))
  (list
   @nonterm{record-type-entry}
   (BNF-seq @nonterm{id} : @nonterm{type}))
  (list
   @nonterm{attr}
   @nonterm{id}
   (BNF-seq @nonterm{id} |(| @nonterm{arg-exprs} |)| )))

@section{Example}

The following glow-surface-syntax program:
@verbatim|{
@interaction([Buyer, Seller])
let payForSignature = (digest : Digest, price : Assets) => {
  @Buyer deposit! price;
  @Seller @publicly let signature = sign(digest);
  withdraw! Seller price;
};
}|

would be parsed into this glow-sexpr-ugly representation:
@racketblock[
(\@ (interaction (\@list Buyer Seller))
   (def payForSignature
     (λ ((digest : Digest) (price : Assets)) ; inferred `: Unit`
       (\@ Buyer (deposit! price))
       (\@ Seller (\@ publicly (def signature (sign digest))))
       (withdraw! Seller price))))
]
