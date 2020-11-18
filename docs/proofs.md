# Security proofs for Glow programs

Eventually, we want to be able to prove entire applications safe, from interface down to hardware,
which involves proving every program correct, including compilers and runtimes, and even chips.
However, for now, we are focusing on the most pressing issue for decentralized applications,
an issue that is not handled by previously existing tools, and that causes millions of dollars every year
to be lost to mistakes and frauds:
the correctness of the financial interactions between untrusting participants.
This involves not just the "contract" on the public blockchain, but
also the programs on each participant's private computer.

## Security properties we will prove about interactions written in Glow

* *Non-negativity*: Amounts of assets in any account or sub-account are always non-negative.
* *Linearity*: Assets cannot be created or destroyed or duplicated or divided,
  only added via explicit deposits, removed via explicit deposits or transfered between accounts.
* *Balance*: The amount in sub-accounts of an interaction at the end of the interaction is 0.
* *Optimality*: Within the participants' respective explicit economic hypotheses, a/the optimal strategy for each participant is to keep executing his program.
* *Non-loss*: Within the participant's explicit economic hypotheses, whatever the actions of other participants, each participant's wallet will keep its value, except for the payment of transaction fees, which is bounded in gas.
* *Programmer-defined*: The programmer may specify additional application-dependent invariants of his own to the interaction.

The Glow compiler will extract a specification from these invariants, and feed them to a theorem prover.
It will reject the program if the theorem prover fails to prove any of these invariants.
It will present any counter-examples found by the theorem provers in a way that is legible
in terms of execution traces with source-level binding assignments.

## Future Plans

Our previous prototype did include some of the above functionality.
We are working to add it to our current compiler.

In a yet further future, we will want to prove the correctness of the compiler toolchain itself.
That will be another endeavour that will require much more capital investment than we can currently afford.
Yet we're confident that our approach will make it much cheaper, more general and more robust
than our competitors' approaches.
