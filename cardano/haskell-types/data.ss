(export #t)

(import
  :clan/poo/poo :clan/poo/number :clan/poo/mop :clan/poo/type
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

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:ValueRef
(define-type ValueRef
  (Sum
    Explicit: Value
    Variable: ByteString))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:Expression
(define-type Expression
  (Sum
    ExpectPublished: ByteString
    IsValidSignature: (Tuple ValueRef ValueRef ValueRef)
    Apply: (Tuple ByteString ValueRef)))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
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

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
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

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:ExecutionPoint
(define-type ExecutionPoint ByteString)

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:DatatypeMap
(define-type DatatypeMap (Map ByteString <- (List (Tuple ByteString Integer))))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:FunctionMap
(define-type FunctionMap (Map ByteString <- (Tuple ByteString (List Statement))))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:VariableMap
(define-type VariableMap (Map ByteString <- Value))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:GlowContract
(define-type GlowContract (Map ExecutionPoint <- (Tuple (List Statement) (Maybe ExecutionPoint))))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:GlowRedeemer
(define-type GlowRedeemer (Tuple ExecutionPoint VariableMap))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:GlowDatum
(define-type GlowDatum
  (Record
    gdContract: [GlowContract]
    gdVariableMap: [VariableMap]
    gdFunctionMap: [FunctionMap]
    gdDatatypeMap: [DatatypeMap]
    gdExecutionPoint: [(Maybe ExecutionPoint)]
    gdDeadline: [Slot]))
