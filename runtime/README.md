# Runtime

## Overview

First, participants create an agreement.
This is a specification of the interaction they are about to participate in.
Glow then generates the code for each participant to execute the agreement,
which they each run on their local machines.

The interaction code in turn may create or use smart contracts on agreed-upon blockchains to hold each other accountable for doing their part of the agreement.

The interaction then begins,
with participants taking active and passive roles,
depending on what is required.

## Initialization

*The following parts are pieced together in `cli/interaction/start-interaction`.*

This section describes what happens leading up to runtime execution.

When we first start the interaction,
we generate a `InteractionAgreement`,
and in the process compile the Glow contract using `start-interaction/generate-agreement`.
This gives us information required to construct the agreement,
such as `interaction`, `interaction-info`, expected `parameters` and so on.

The agreement is sent to the other party to verify the legitimacy of the interaction.
They can verify the contract via its digest, paramaters, participants and so on.

Next, we start the Runtime by calling `run:terminal`.
This recompiles the program again.
Then we parse the compiler output to locate and store transaction boundaries.
The compilation artifact is then used to construct the `Runtime` object.
We then `execute` the `Runtime` object.

## Execution

At the very start of the interaction,
we would have been prompted to enter our identity and the role in the interaction.
Using this information,
we will begin either as the active or passive participant,
depending on how the interaction was written.

For example in the `buy_sig#payForSignature` interaction:

```js
let payForSignature = (digest : Digest, price : Nat) => {
  deposit! Buyer -> price; // First code block
  @publicly!(Seller) let signature = sign(digest);
  ...
}
```

The `Buyer` begins as the active participant,
since the `Buyer` needs to deposit the funds.

If we specified our role to be a `Buyer`,
we will begin as the active participant,
otherwise we will be the passive one.

You can see:
`run-active-code-block` / `run-passive-code-block` for actual implementation of running active / passive code blocks.

*Note: For the demarcation of code blocks for participants, you can read the [Glow whitepaper Appendix B](https://docs.google.com/document/d/1nBmI28yISX2HynodZnYWW0Px6re4JyYNNw2ncaFfJSg/edit#heading=h.q5pwb5iqbzuw) for design or look at the `checkpointify` compiler pass for its implementation.*

### Active Participant: First Execution Step (Contract & Handshake creation)

Implemented in `runtime/participant-runtime/run-active-code-block`.

During the first execution step, the Active Participant does the following:

1. Deploy the contract using `deploy-contract`. This contains code for the consensus.

    At this step, we also update our Runtime object to include the `ContractConfig`.
    The `ContractConfig` gives us the `code-hash`,
    along with other metadata used for identification / verification.

1. Run the current code block to completion using `interpret-current-code-block`.
1. Create a handshake. This allows the other participants to verify the following:

    - Contract agreement is unchanged.
    - The deployed contract is correctly configured, via ContractConfig.
    - The data that should be published by the first active participant is published.
  
    You can look at the `AgreementHandshake` type to see the precise data provided.
  
1. Transition into Passive Participant at the next code block, waiting for updates to the contract state.

At this point,
we are waiting for other participants to join and start their side of the interaction.
This is described in the following section,
where the handshake generated above is sent off-chain to other participants,
such that they can synchronize their Runtime with the contract and the other participants.
  
### Passive Participant: First Execution Step (Agreement Handshake)

Implemented in `runtime/participant-runtime/run-passive-code-block/handshake`.

During the first execution step, the Passive Participant does the following:

1. Retrieve the agreement handshake which was sent by the Active Participant.
1. Extract `agreement`, `contract-config`, `published-data` from the handshake.
1. Verify the agreement.
1. Update `timer-start`.
1. Run their side of the interaction.
1. Verify the on-chain contract.

    This is done by reconstructing the transaction message for deploying the contract,
    and comparing it with the `contract-config`.
    
After this point,
if the interaction protocol is done for the current participant,
we will exit,
returning the resulting environment.

Otherwise we will execute the next code block.

### Active Participant: Subsequent steps

1. Publish current frame data to the output port.

    This contains the necessary details to restore the frame.

1. Run the current code block
1. Post their outbox to the contract as a transaction.
1. Update their `timer-start` to after their transaction has been processed.

TODO Point to documentation in `evm-runtime.md` on how we publish 0/1 marker to indicate if execution proceeds.

TODO Point to documentation in `evm-runtime.md` on `post-transaction`'s implementation and functionality.

### Passive Participant: Waiting for Contract LOGs

As the Passive Participant,
we are now waiting for other participants to post updates to the contract.
These emitted under the hood with the `LOG` command.

TODO mention `evm-runtime.md` functions where this is wired up.

1. Extract the block we should watch events from.
1. Start `watch`ing for emitted LOGs. 
1. Examine the LOG object after timeout:

    - If no LOG objects emitted we have timed out. We claim the escrowed funds.

    - If LOG objects were emitted,

        1. Update our new `timer-start`,
        1. Construct a new BlockCtx with published data,
        1. Run the current code block (i.e. their side of the interaction for the first code block).

            For example, in `buy_sig#payForSignature`,
            this would be to verify the buyer has deposited their funds:

            ```
            Executing code block begin0 ...
            (expect-deposited price)
            (@debug-label dlb1)
            ```

## Glossary

**timer-start**

Refers to the block number at which the transaction should start by or has started,
rather than actual *time* in seconds/minutes/hours.

**Consensus**

Consensus programs are used to control assets contributed by the participants.
They get deployed and ran on the Blockchain network.

**Participant Runtime**

Handles private computations and/or things which can be run off-chain by individual participants. It also handles the interaction between participants and the consensus.

### Appendix A: Deploying a contract

TODO What data we need, How we construct it from the data.

### Appendix B: Interacting with the consensus during Runtime

TODO How deposit, withdraw, publish, require etc... map to interactions between the consensus and participants.
