# Glow Cardano Architecture

## Top level overview

```
.
---- Docs
├── Architecture.md  # Project layout, component functions
├── README.md        # Limitations, Functionality, Development, Biblio

---- Top level
├── smart-contract-backend.ss # Wrapper for PAB/SCB's HTTP API, CLI
├── wallet.ss                 # Wrapper Cardano Wallet HTTP API, CLI
├── scripts/                  # Scripts: create tx, create Wallet, starting PAB/SCB servers, run DApp

---- Libraries
├── util.ss           # Various utility functions: request builders, conversion functions
├── haskell/          # ...  . See Section <>
├── haskell-types/    # Glow type definitions for Haskell counterparts
├── poo-extensions.ss # Provides gerbil-poo type synonyms for primitive haskell counterparts. 
                      # Used for gerbil-scheme codegen (haskell/),
                      # Used to create more complex types in haskell-types/

---- Infra
├── plutus-scb-logging.yaml
├── plutus-scb.yaml
├── shell.nix
├── default.nix

---- Unused
└── plutus.ss         # NOT USED: IR sketch

```

## Scripts

Scripts for transactions, wallets, contracts and PAB initialization.

```
.
├── create-transaction.ss # Create a transaction via Cardano Wallet
├── create-wallet.ss      # Create a wallet via Cardano Wallet
├── execute-contract.ss   # Execute Closing / buy_sig contract
└── run-servers.ss        # Start servers for SCB/PAB
```

## haskell/

```
.
---- Top level
├── app
│   ├── CodeGenMain.hs  # Generate Module of Scheme types from Haskell types.
│   └── ContractMain.hs # Contract command line

---- Library
├── lib
│   ├── Client.hs         # Glow contract definition
│   ├── CodeGen.hs        # Pretty Printer for Gerbil Types. Code Gen ??
│   ├── Contract.hs       # Plutus Runtime for Glow on-chain code. Contains evaluator, state transition logic.
│   ├── Parser.hs         # Parser for Glow contract SExpr -> Haskell Types
│   ├── Types.hs          # Haskell contract types corresponding to Glow DApp constructs
│   ├── Util.hs           # NOT USED: Utils for script sizes
│   └── ExampleContracts/ # NOT USED: Example Plutus contracts

---- Infra
├── glow-cardano.cabal
├── Makefile
├── shell.nix

---- Test
└── test
    └── Main.hs
```

## Running the interaction

1. `run-servers.ss`
2. `execute-contract.ss`

### run-servers

This calls `run-all-servers` which under the hood calls `plutus-scb all-servers` to run all (mock) servers.
It also provisions `process-outbox` (NOTE: not too sure what this is for...).

## Interface

### execute-contract

1. Defines `project` consensus output for the `buy_sig/closing` DApp.

    Output is similar to `glow/t/passdata/buy_sig`,
    but without Participant Runtime (off-chain) code,
    since this is hard coded (see step 3. below).
    Defines interaction `header`, `body`.

2. Defines input parameters for contract initialization: `initial-var-map`
3. Defines input parameters for seller: `seller-var-map`
4. Defines input parameters for buyer: `empty-var-map`
5. Runs the contract:
  a. Initialize the contract - uses `glow-contract:create`, passes in `header`, `body`, `initial-var-map`.
  b. Runs buyer interaction step - uses `glow-contract:move`, passes in `empty-var-map`.
  c. Runs seller interaction step - uses `glow-contract:move`, passes in `seller-var-map`.
  
NOTE: Look at `Client.hs` for endpoint definitions.
  
### smart-contract-backend

1. Defines `glow-contract:create`.

    This sends a post request to: `"/api/contract/<contract-uuid>/endpoint/<create>"`.

    JSON body:
    ```json
    { sourceHeader: ... // header from project's consensus output
    , sourceBody: ... // body from project's consensus output
    , initialVariableMap: ... // contract parameters: e.g. Buyer = ..., Seller = ...
    , timeout: ... // How long we should wait before timing out. TODO: Is this supported??
    }
    ```
    
    FIXME: This looks incorrect. `CreateParams` is defined as:
    ```
    (define-type CreateParams
        (Record
            datatypes: [DatatypeMap]
            participants: [(List PubKey)]
            arguments: [VariableMap]
            contract: [GlowContract]
            timeoutLength: [Integer]))
    ```



2. Defines `glow-contract:move`.

    This sends a post request to: `"/api/contract/<contract-uuid>/endpoint/move"`
    
    JSON body:
    ```json
    { variableMap: ... // required parameters for this interaction step
    , entryPoint: ... // checkpoints e.g. cp, cp0 TODO: how is this wired up?
    }
    ```

### How do we initialize the contract?

1. Send in post request to `create` endpoint.

2. GlowContract is used to represent the contract code. 

    ```haskell
    type GlowContract = Map ExecutionPoint ([Statement], Maybe ExecutionPoint)
    ```
    
    FIXME: Our `glow-contract:create` function needs to be updated to
    produce our contract in this form (as a json object to be posted to `create`).
   

## Plutus Contract section

### Client

1. Defines contract entrypoint via `glowContract`.

    This includes routing logic for endpoints.
    ```
    create -> createContract
    move -> executeMove
    wait -> SM.waitForUpdate
    ```

2. Defines the endpoints: `create`, `move`, `wait`.

    This are the endpoints we are posting to from the Interface section above.
    
3. Defines route handlers.

  - `createContract`
  
      a. Converts params into `GlowDatum`.
      b. Uses `awaitSlot` to wait until slot is reached. Returns current slot.
         TODO: Currently hardcoded to be 0. Perhaps it should be the upcoming slot?
      c. Initializes state machine: `SM.runInitialise` with the `GlowDatum`, payment.
      d. Execute first step, at entrypoint `begin`.
      
  - `executeMove`
  
      a. Advance the state machine using `SM.runStep`, with:
          - SM.StateMachineInstance GlowDatum GlowRedeemer (TODO: what is this??)
          - entrypoint
          - variableMap
      b. Return new datum.
      
### Contract

We use [`GlowStateMachine`](https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week7.html)
under the hood. 

We define the necessary interface methods:

```haskell
smTransition = transition cfg -- State transition step
smFinal = isNothing . gdExecutionPoint -- TODO: what is this??
smCheck = \_datum _redeemer _ctx -> True -- Not needed,
                                         -- only required for defining checks which can't be
                                         -- expressed by TxConstraints.
```

#### Transition logic

TODO: State transition diagram for Glow runtime.

1. Looks up and verifies entrypoint
2. Extracts code section to run, exitpoint (i.e. next entrypoint) 
3. Run code section

## CodeGen.hs

Generate module of Scheme types from Haskell types.

## haskell-types/

```
.
├── client.ss      # Used by smart-contract-backend: CreateParams, MoveParams
├── data.ss        # NOT USED
└── transaction.ss # NOT USED
```

## Next steps

- [ ] Renaming changes for SCB -> PAB
- [ ] Updating dependency plutus-scb -> plutus-pab
- [ ] Update nix-dependencies for `glow-cardano` in `pkgs.nix`
- [ ] Trying out the application
- [ ] Restarting from Plutus-starter?

## How does this use the PAB?

TODO

## How does this manage checkpoints?

TODO: cp, cp0
