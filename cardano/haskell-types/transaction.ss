(export #t)

(import
  :clan/poo/poo :clan/poo/number :clan/poo/mop :clan/poo/type
  ../poo-extensions)


; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Interval:Closure
(define-type Closure Bool)

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Interval:Extended
(define-type Extended
  (Sum
    NegInf: Unit
    Finite: Any
    PosInf: Unit))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Interval:UpperBound
(define-type UpperBound
  (Record
    extended: [Extended]
    closure: [Closure]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Interval:LowerBound
(define-type LowerBound
  (Record
    extended: [Extended]
    closure: [Closure]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Scripts:ValidatorHash
(define-type ValidatorHash
  (Record
    byteString: [ByteString]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Crypto:PubKeyHash
(define-type PubKeyHash
  (Record
    getPubKeyHash: [ByteString]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Scripts:Redeemer
(define-type Redeemer
  (Record
    getRedeemer: [Data]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Scripts:Validator
(define-type Validator
  (Record
    getValidator: [Script]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.TxId:TxId
(define-type TxId
  (Record
    getTxId: [ByteString]))

; plutus-tx-0.1.0.0-7JAboNr35Sj2S0JH6aplg9
; Language.PlutusTx.Data:Data
(define-type Data
  (Sum
    Constr: (Tuple Integer (List Data))
    Map: (List (Tuple Data Data))
    List: (List Data)
    I: Integer
    B: ByteString))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; LedgerBytes:LedgerBytes
(define-type LedgerBytes
  (Record
    getLedgerBytes: [ByteString]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Slot:Slot
(define-type Slot
  (Record
    getSlot: [Integer]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Interval:Interval
(define-type Interval
  (Record
    ivFrom: [LowerBound]
    ivTo: [UpperBound]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Value:TokenName
(define-type TokenName
  (Record
    unTokenName: [ByteString]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Value:CurrencySymbol
(define-type CurrencySymbol
  (Record
    unCurrencySymbol: [ByteString]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Tx:TxOutType
(define-type TxOutType
  (Sum
    PayToScript: DatumHash
    PayToPubKey: Unit))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Address:Address
(define-type Address
  (Sum
    PubKeyAddress: PubKeyHash
    ScriptAddress: ValidatorHash))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Tx:TxInType
(define-type TxInType
  (Sum
    ConsumeScriptAddress: (Tuple Validator Redeemer Datum)
    ConsumePublicKeyAddress: Unit))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Tx:TxOutRef
(define-type TxOutRef
  (Record
    txOutRefId: [TxId]
    txOutRefIdx: [Integer]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Scripts:Datum
(define-type Datum
  (Record
    getDatum: [Data]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Scripts:DatumHash
(define-type DatumHash
  (Record
    byteString: [ByteString]))

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

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Scripts:MonetaryPolicy
(define-type MonetaryPolicy
  (Record
    getMonetaryPolicy: [Script]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Slot:SlotRange
(define-type SlotRange (Interval Slot))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Value:Value
(define-type Value
  (Record
    getValue: [(Map CurrencySymbol <- (Map TokenName <- Integer))]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Tx:TxOut
(define-type TxOut
  (Record
    txOutAddress: [Address]
    txOutValue: [Value]
    txOutType: [TxOutType]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Tx:TxIn
(define-type TxIn
  (Record
    txInRef: [TxOutRef]
    txInType: [TxInType]))

; plutus-ledger-0.1.0.0-E9tyq7JXFRFGbrn5sc8sO2
; Ledger.Tx:Tx
(define-type Tx
  (Record
    txInputs: [(Set TxIn)]
    txOutputs: [(List TxOut)]
    txForge: [Value]
    txFee: [Value]
    txValidRange: [SlotRange]
    txForgeScripts: [(Set MonetaryPolicy)]
    txSignatures: [(Map PubKey <- Signature)]
    txData: [(Map DatumHash <- Datum)]))
