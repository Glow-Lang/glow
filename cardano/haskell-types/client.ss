(export #t)

(import
  :clan/poo/poo :clan/poo/number :clan/poo/mop :clan/poo/type
  ../poo-extensions)


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

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Crypto:Signature
(define-type Signature
  (Record
    getSignature: [ByteString]))

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

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:ExecutionPoint
(define-type ExecutionPoint ByteString)

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

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; LedgerBytes:LedgerBytes
(define-type LedgerBytes
  (Record
    getLedgerBytes: [ByteString]))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:GlowContract
(define-type GlowContract (Map ExecutionPoint <- (Tuple (List Statement) (Maybe ExecutionPoint))))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:VariableMap
(define-type VariableMap (Map ByteString <- Value))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Crypto:PubKey
(define-type PubKey
  (Record
    getPubKey: [LedgerBytes]))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Types:DatatypeMap
(define-type DatatypeMap (Map ByteString <- (List (Tuple ByteString Integer))))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Client:MoveParams
(define-type MoveParams
  (Record
    variableMap: [VariableMap]
    entryPoint: [String]))

; gerbil-cardano-hs-0.1.0.0-GQK4VtguhjS2cuSzztyTY4
; Client:CreateParams
(define-type CreateParams
  (Record
    datatypes: [DatatypeMap]
    participants: [(List PubKey)]
    arguments: [VariableMap]
    contract: [GlowContract]
    timeoutLength: [Integer]))
