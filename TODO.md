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

