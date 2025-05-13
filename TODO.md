# TODO

## Automatic bounds detection

Use Z3 to automatically find the largest integer that makes some program work
on a given network combination.

Determine the maximum sizes for each integer at a phase before representation selection,
so that the program will automatically be optimized for the right size?
Yet on some networks, do it with a granularity that allows sharing with more users?
e.g. on Ethereum, keep 96-bit or 128-bit amounts so you can share the contract,
when the contract is general-purpose enough? (How do we know it is, though?)

## Interface

Start from the functionality of [Alacrity](https://alacrity-lang.org).
See what other webdevelopment platforms offer.
Watch particularly our rivals from
[Truffle](https://www.trufflesuite.com/),
[Scilla](https://scilla-lang.org/), etc.
Drew suggests implementing it with [Quasar](https://quasar.dev/)?

## Infrastructure

Use Nix flakes to deploy.
https://www.tweag.io/blog/2020-07-31-nixos-flakes/

## Deal with forks

Both forks as in rival blocks or 51% attacks that succeed, where only one fork is valid,
but which may change, and forks as in a new network is started and both forks are valid,
each in its own separate universe.

## Implement a good internal wallet

* Support having the secret key ring as an encrypted database
  - Encrypt the entire private key database with gnupg and decrypt it with gnupg-agent?
  - Or encrypt individual entries with gnupg instead?
  - Have a key database, not just a json file, so we can write ephemeral keys?
    Or have a separate database just for ephemeral keys?
  - On iOS and Android, use secure storage. On Browser, use IndexedDB.
* Use memory-hardening to make sure secret key information is overwritten in memory
  immediately after use and never copied around by the Garbage Collector while live
  (which should last as short as possible).
  This requires support in gerbil-crypto, gerbil, OpenSSL, etc.
* Support having (master) keys in hardware wallets, trusted platform module, or yubikey.
* Support many types of keys, not just plain ethereum keys
  - Support [BIP-32](https://en.bitcoin.it/wiki/BIP_0032) /
    [BIP-39](https://en.bitcoin.it/wiki/BIP_0039) /
    [BIP-43](https://en.bitcoin.it/wiki/BIP_0043) /
    [BIP-44](https://en.bitcoin.it/wiki/BIP_0044) HD wallet.
  - [SLIP-0044](https://github.com/satoshilabs/slips/blob/master/slip-0044.md) /
     https://wolovim.medium.com/ethereum-201-hd-wallets-11d0c93c87f7
  - https://medium.com/mycrypto/the-journey-from-mnemonic-phrase-to-address-6c5e86e11e14
  - For Cardano, use
    [CIP-5](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0005/CIP-0005.md) /
    [CIP-1852](https://github.com/cardano-foundation/CIPs/blob/master/CIP-1852/CIP-1852.md) /
    [EMIP-3](https://github.com/Emurgo/EmIPs/blob/master/specs/emip-003.md) /
    [ED25519BIP32](https://raw.githubusercontent.com/input-output-hk/adrestia/master/user-guide/static/Ed25519_BIP.pdf) /
    [Edge API](https://input-output-hk.github.io/cardano-wallet/api/edge/) as used by Daedalus
* Move some of this support to gerbil-crypto ?
  Nah, secrets and HD wallets go across networks, so not just gerbil-ethereum.
* Making key theft harder:
  - Use memory hardening for cryptographic keys that control assets.
  - Use ulimit to prevent coredumps for any process holding those keys
  - Move those keys to their own process and communicate over RPC,
    so they are never in the same process as something else?

## UI

https://notadesigner.io/

## Typechecking
https://twitter.com/effectfully/status/1868298441241309394
