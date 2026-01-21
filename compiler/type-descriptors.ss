(export #t)

(import
  :clan/poo/object :clan/poo/mop :clan/poo/type
  :mukn/ethereum/evm-runtime :mukn/ethereum/assembly)

;; ---------------------
;; Glow Type descriptors
;; ---------------------
;;
;; -------------
;; Boxed/Unboxed
;; -------------
;; Describes types which are either boxed / unboxed.
(define-type (Boxed/Unboxed @ [] .boxed?))
(define-type (Boxed @ [Boxed/Unboxed]) .boxed?: #t)
(define-type (Unboxed @ [Boxed/Unboxed]) .boxed?: #f)

;; --------------
;; object<->bytes
;; --------------
;; Describes types which can be converted to and from bytes.
(define-type (objects<->bytes @ [])
  .<-bytes: identity ; overwrite
  .bytes<-: identity ; overwrite
  )

;; ----------------
;; Size descriptors
;; ----------------
;; NOTE: Size is in bytes.
;;
;; -----------
;; Static size
;; -----------
;; Describes types which have fixed size during compile time, e.g. static strings
(define-type (StaticSize/DynamicSize @ [] .static-size?))
(define-type (StaticSize @ [StaticSize/DynamicSize])
  .static-size?: #t
  .size<-bytes: (lambda (bytes) (u8vector-length bytes))
  )
(define-type (DynamicSize @ [StaticSize/DynamicSize])
  .static-size?: #f
  )

;; ----------
;; Fixed Size
;; ----------
;; Describes types which have fixed size (in bytes), e.g. boolean / UInt8
(define-type (FixedSize/NotFixedSize @ [] .fixed-size?))
(define-type (FixedSize @ [FixedSize/NotFixedSize StaticSize])
  .fixed-size?: #t
  .size: 0 ; overwrite
  .size<-bytes: (lambda (self _bytes) (.@ self .size))
  )
(define-type (NotFixedSize @ [FixedSize/NotFixedSize])
  .fixed-size?: #f
  )

;; ------------
;; Bounded size
;; ------------
;; Describes types which have bounded size
(define-type (BoundedSize/UnboundedSize @ []) .bounded-size?: #t)
(define-type (BoundedSize @ [])
  .bounded-size?: #t
  .bounded-size: 0
  )
(define-type (UnboundedSize @ [])
  .bounded-size?: #f
  )
