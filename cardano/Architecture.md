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

## CodeGen.hs

TODO

## Client.hs + Contract.hs

TODO

## Execute Contract

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
- [ ] Update nix-deps
- [ ] Trying out the application
- [ ] Restarting from Plutus-starter?

## How does this use the PAB?

TODO
