# Standard library of DApps that work with the current compiler

This directory contains DApps written in *Glow* that commonly useful
and/or illustrative of some features of the language:

* [buy_sig](buy_sig.glow) is a standard closing contract,
  wherein a Buyer pays money in exchange for a Seller signing a transaction:
  digital title transfer, transaction that completes an atomic swap on another chain,
  certification, room rental agreement, pass to some online event, etc.
  If the Seller fails to sign on time, the Buyer automatically recovers his stake.
  
  See [Tutorial: Buying a Signature](buy_sig.md) for a walkthrough on how to deploy
  and run an interaction with the [buy_sig](buy_sig.glow) contract.

* [coin_flip](coin_flip.glow) illustrates how to agree on a mutually random number,
  one that both party agree is random, using a three-step commit-reveal protocol.
  In this particular contract, the number controls the outcome of a simple bet.
  But in general, random numbers can serve as the initial seed for a lot of
  randomized algorithms and cryptographic protocols, and it is important that
  it is impossible for any party to predict or control it.

* [rps_simple](rps_simple.glow) illustrates another simple multiple-step protocol.
  In this particular contract, two participants play a simple game of rock-paper-scissors.

Further examples of the language as it is envisioned, that are not yet fully supported
by the current compiler, are in the [futures](../futures/) directory.
