(export #t)

(import
  :clan/poo/object :clan/poo/number :clan/poo/mop :clan/poo/type
  ../poo-extensions)


; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Interval:Closure
(define-type Closure Bool)

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Interval:Extended
(define-type Extended
  (Sum
    NegInf: Unit
    Finite: Any
    PosInf: Unit))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Scripts:ValidatorHash
(define-type ValidatorHash
  (Record
    byteString: [ByteString]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Crypto:PubKeyHash
(define-type PubKeyHash
  (Record
    getPubKeyHash: [ByteString]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Interval:UpperBound
(define-type UpperBound
  (Record
    extended: [Extended]
    closure: [Closure]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Interval:LowerBound
(define-type LowerBound
  (Record
    extended: [Extended]
    closure: [Closure]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Credential:StakingCredential
(define-type StakingCredential
  (Sum
    StakingHash: ByteString
    StakingPtr: (Tuple Integer Integer Integer)))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Credential:Credential
(define-type Credential
  (Sum
    PubKeyCredential: PubKeyHash
    ScriptCredential: ValidatorHash))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Scripts:Redeemer
(define-type Redeemer
  (Record
    getRedeemer: [Data]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Scripts:Validator
(define-type Validator
  (Record
    getValidator: [Script]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.TxId:TxId
(define-type TxId
  (Record
    getTxId: [ByteString]))

; plutus-tx-0.1.0.0-1f7ec78ba2b1f800fd5c43f7a3469e4e48a32d4a57543539efe0da297b8e2e6c
; PlutusTx.Data:Data
(define-type Data
  (Sum
    Constr: (Tuple Integer (List Data))
    Map: (List (Tuple Data Data))
    List: (List Data)
    I: Integer
    B: ByteString))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Bytes:LedgerBytes
(define-type LedgerBytes
  (Record
    getLedgerBytes: [ByteString]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Slot:Slot
(define-type Slot
  (Record
    getSlot: [Integer]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Interval:Interval
(define-type Interval
  (Record
    ivFrom: [LowerBound]
    ivTo: [UpperBound]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Value:TokenName
(define-type TokenName
  (Record
    unTokenName: [ByteString]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Value:CurrencySymbol
(define-type CurrencySymbol
  (Record
    unCurrencySymbol: [ByteString]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Address:Address
(define-type Address
  (Record
    addressCredential: [Credential]
    addressStakingCredential: [(Maybe StakingCredential)]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Tx:TxInType
(define-type TxInType
  (Sum
    ConsumeScriptAddress: (Tuple Validator Redeemer Datum)
    ConsumePublicKeyAddress: Unit))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Tx:TxOutRef
(define-type TxOutRef
  (Record
    txOutRefId: [TxId]
    txOutRefIdx: [Integer]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Scripts:Datum
(define-type Datum
  (Record
    getDatum: [Data]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Scripts:DatumHash
(define-type DatumHash
  (Record
    byteString: [ByteString]))

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

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Scripts:MonetaryPolicy
(define-type MonetaryPolicy
  (Record
    getMonetaryPolicy: [Script]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Slot:SlotRange
(define-type SlotRange (Interval Slot))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Value:Value
(define-type Value
  (Record
    getValue: [(Map CurrencySymbol -> (Map TokenName -> Integer))]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Tx:TxOut
(define-type TxOut
  (Record
    txOutAddress: [Address]
    txOutValue: [Value]
    txOutDatumHash: [(Maybe DatumHash)]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Tx:TxIn
(define-type TxIn
  (Record
    txInRef: [TxOutRef]
    txInType: [(Maybe TxInType)]))

; plutus-ledger-api-0.1.0.0-9872ce8cc16813f46ec5ada8ab54c7343df5e9cbe3b9938bc3031abb23c78b71
; Plutus.V1.Ledger.Tx:Tx
(define-type Tx
  (Record
    txInputs: [(Set TxIn)]
    txCollateral: [(Set TxIn)]
    txOutputs: [(List TxOut)]
    txForge: [Value]
    txFee: [Value]
    txValidRange: [SlotRange]
    txForgeScripts: [(Set MonetaryPolicy)]
    txSignatures: [(Map PubKey -> Signature)]
    txData: [(Map DatumHash -> Datum)]))
