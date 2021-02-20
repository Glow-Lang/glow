(export #t)

(import
  :clan/poo/object :clan/poo/number :clan/poo/mop :clan/poo/type
  ../poo-extensions)


; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; LedgerBytes:LedgerBytes
(define-type LedgerBytes
  (Record
    getLedgerBytes: [ByteString]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Crypto:Signature
(define-type Signature
  (Record
    getSignature: [ByteString]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Crypto:PubKey
(define-type PubKey
  (Record
    getPubKey: [LedgerBytes]))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:ValueRef
(define-type ValueRef
  (Sum
    Explicit: Value
    Variable: ByteString))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:Expression
(define-type Expression
  (Sum
    ExpectPublished: ByteString
    IsValidSignature: (Tuple ValueRef ValueRef ValueRef)
    Apply: (Tuple ByteString ValueRef)))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:Value
(define-type Value
  (Sum
    Constructor: (Tuple ByteString Integer (List Value))
    PubKey: PubKey
    Signature: Signature
    ByteString: ByteString
    Integer: Integer
    Boolean: Bool
    Unit: Unit))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:Statement
(define-type Statement
  (Sum
    Label: ByteString
    Declare: ByteString
    Define: (Tuple ByteString Expression)
    DefineFunction: (Tuple ByteString ByteString (List Statement))
    DefineDatatype: (Tuple ByteString (List (Tuple ByteString Integer)))
    SetParticipant: ValueRef
    ExpectDeposited: ValueRef
    ExpectWithdrawn: (Tuple ValueRef ValueRef)
    Require: ValueRef
    Return: ValueRef))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Slot:Slot
(define-type Slot
  (Record
    getSlot: [Integer]))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:ExecutionPoint
(define-type ExecutionPoint ByteString)

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:DatatypeMap
(define-type DatatypeMap (Map ByteString -> (List (Tuple ByteString Integer))))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:FunctionMap
(define-type FunctionMap (Map ByteString -> (Tuple ByteString (List Statement))))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:VariableMap
(define-type VariableMap (Map ByteString -> Value))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:GlowContract
(define-type GlowContract (Map ExecutionPoint -> (Tuple (List Statement) (Maybe ExecutionPoint))))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:GlowRedeemer
(define-type GlowRedeemer (Tuple ExecutionPoint VariableMap))

; glow-cardano-0.1.0.0-CngPKhcIv8FI1Ajum74KPJ
; Types:GlowDatum
(define-type GlowDatum
  (Record
    gdContract: [GlowContract]
    gdVariableMap: [VariableMap]
    gdFunctionMap: [FunctionMap]
    gdDatatypeMap: [DatatypeMap]
    gdExecutionPoint: [(Maybe ExecutionPoint)]
    gdDeadline: [Slot]))
