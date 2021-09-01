(export #t)

(import
  :clan/poo/object :clan/poo/number :clan/poo/mop :clan/poo/type
  (only-in :mukn/ethereum/types delay-type)
  ../poo-extensions)


; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Bytes:LedgerBytes
(define-type LedgerBytes
  (Record
    getLedgerBytes: [ByteString]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Crypto:Signature
(define-type Signature
  (Record
    getSignature: [ByteString]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Crypto:PubKey
(define-type PubKey
  (Record
    getPubKey: [LedgerBytes]))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:GlowValueRef
(define-type GlowValueRef
  (Sum
    Explicit: GlowValue
    Variable: ByteString))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:Expression
(define-type Expression
  (Sum
    ExpectPublished: ByteString
    IsValidSignature: (Tuple GlowValueRef GlowValueRef GlowValueRef)
    Apply: (Tuple ByteString GlowValueRef)
    NoOp: Unit))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:GlowValue
(define-type GlowValue
  (Sum
    Constructor: (Tuple ByteString Integer (List (delay-type GlowValue)))
    PubKey: PubKey
    Signature: Signature
    ByteString: ByteString
    Integer: Integer
    Boolean: Bool
    Unit: Unit))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:Statement
(define-type Statement
  (Sum
    Label: ByteString
    Declare: ByteString
    DefineInteraction: (Tuple (List ByteString) (List ByteString) (List (Tuple ByteString (List (delay-type Statement)))))
    Define: (Tuple ByteString Expression)
    DefineFunction: (Tuple ByteString ByteString (List (delay-type Statement)))
    DefineDatatype: (Tuple ByteString (List (Tuple ByteString Integer)))
    SetParticipant: GlowValueRef
    ExpectDeposited: GlowValueRef
    ExpectWithdrawn: (Tuple GlowValueRef GlowValueRef)
    AddToDeposit: GlowValueRef
    AddToWithdraw: (Tuple GlowValueRef GlowValueRef)
    Ignore: Expression
    Require: GlowValueRef
    Return: GlowValueRef))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Slot:Slot
(define-type Slot
  (Record
    getSlot: [Integer]))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:ExecutionPoint
(define-type ExecutionPoint ByteString)

; glow-cardano-0.1.0.0-inplace
; Glow.Types:DatatypeMap
(define-type DatatypeMap (Map ByteString -> (List (Tuple ByteString Integer))))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:FunctionMap
(define-type FunctionMap (Map ByteString -> (Tuple ByteString (List Statement))))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:VariableMap
(define-type VariableMap (Map ByteString -> GlowValue))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:GlowContract
(define-type GlowContract (Map ExecutionPoint -> (Tuple (List Statement) (Maybe ExecutionPoint))))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:GlowRedeemer
(define-type GlowRedeemer
  (Record
    (Tuple: [(Tuple ExecutionPoint VariableMap)])))

; glow-cardano-0.1.0.0-inplace
; Glow.Types:GlowDatum
(define-type GlowDatum
  (Record
    gdContract: [GlowContract]
    gdVariableMap: [VariableMap]
    gdFunctionMap: [FunctionMap]
    gdDatatypeMap: [DatatypeMap]
    gdExecutionPoint: [(Maybe ExecutionPoint)]
    gdDeadline: [Slot]))
