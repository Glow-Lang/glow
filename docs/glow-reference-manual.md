## Intro

THIS FILE HAS OLD STUFF THAT NEEDS TO BE MOVED TO THE Scribble DOCUMENTATION AND/OR THE Wiki.

## Caveats when using both *Glow* and JavaScript

The syntax or *Glow* was specifically designed to be familiar to those who use
JavaScript, ReasonML, TypeScript, etc.

One major principle we follow is one to avoid confusion:
**If program fragment is valid in both *Glow* and JavaScript,
the *meaning* of that fragment should be the same in both languages, and/or
any discrepancy in this meaning should be *obvious*.**

If at any point you are confused by how a *Glow* program works
based on your expectations as a JavaScript or ReasonML or TypeScript programmer,
or on the contrary if when writing code in JavaScript or ReasonML or TypeScript
you are led to wrong expectations based on what you read or wrote in *Glow*,
then this is a serious design issue that we will try to address in a future version of *Glow*.

That said, here are some differences between *Glow* and JavaScript:

- *Glow* integers are all `BigInt`s, yet *Glow* integer literals do not end with an `n`
  whereas JavaScript `BigInt` literals do, and JavaScript integer literals without an `n`
  are read as floating point-numbers that only keep 52 bits worth of mantissa information.
  This difference should be pretty obvious when reading or writing *Glow* or JavaScript programs
  and translating from one to the other, but bears mentioning.

- *Glow* uses `|` for alternatives in types or in pattern matching, not for bitwise-or.
  Also, *Glow* does not use `&` or `^` or `~` for bitwise and, xor, not.
  Instead, *Glow* uses tripled operators `&&&`, `|||`, `^^^`, `~~~`
  for bitwise and, or, xor, not operations.
  The syntactic difference is obvious and leaves no room for confusion.

- *Glow* uses `<<` and `>>` for bitwise shift left and right operations.
  These work the same as in JavaScript as long as you use `BigInt` in JavaScript.
  Note however that legacy bitwise operations on "normal" numbers in JavaScript
  first interprets floating-point numbers as 32-bit signed integers,
  then computes on those 32-bit signed integers;
  it then provides a special `>>>` "logical" shift right operation
  that unlike other operations specially treats its left-hand-side argument as
  an 32-bit *unsigned* integer; this operator is not available on `BigInt`.
  There again, these differences should be relatively obvious to people dealing
  with integers in JavaScript vs *Glow*.

## Compiling your program

At the moment, *Glow* doesn't have much or a user-interface for building and deploying DApps.

But we will include user satisfaction in the design.

## Examples of DApps in *Glow*

If you are at the beginning of your journey in DApp programming,
you might want to just program DApps buy importing ready-made smart contracts.

We have plenty of them at [this address](https://gitlab.com/mukn/glow/-/tree/master/future#dead-man-switch), where you'll find a list of our contracts (those already done and our works-in-progress) with a list of their features, and, of course, `.glow` files for you to import in your DApp. Each of these files is open-source and auditable.

But of course, if you want to program your smart-contracts yourself, you will have to write one from scratch, or from a modified copy of one of the existing contracts.

Some of *Glow*'s functionalities will be hard to grasp if you already need to focus on the main functions of your DApp. All the examples below are copy-pastable in *Glow*, but if you want to use a clean version of them in your DApp, you can just replicate them.

TODO: ???


**Details on contract execution**

To invoke the contract at runtime:

Let’s suppose that two participant Alice and Bob agree
that Alice will buy a signature by Bob of a string that says:
`"I, Bob, sell item #101 to Alice"`, for 100 ethers (10**20 wei)
on the Cardano KEVM testnet.

First, the two participants use some authenticated private channels *off-chain*
to negotiate the terms of the agreement.
They encode this agreement as a standardized JSON object as follows,
where the hexadecimal bits are the ethereum addresses of the respective users,
and various other digests:

```
["interaction-agreement",
  {"glow-version": "v0.0-383-g83524b1",
    "interaction": "mukn/glow/dapps/buy_sig#payForSignature",
    "participants": {"Buyer": "0xC54e86DFFb87B9736E2E35DD85c775358F1c31CE",
                     "Seller": "0x9CcaEd210CE8c0Cb49c5Ad1C4f583406c264BA69"},
    "parameters": {"digest": "0x7a33c718fe7f3b9c56bd67b7b3e20fec6e3edf083626a7e11a10bba90243e405"
                   "price": "0x246ddf97976680000"},
    "reference": {"Buyer": "Purchase #42", "Seller": "Sale #101"},
    "parameters": {"wagerAmount": {"TestEther": 100}},
    "options": {"blockchain": "Cardano KEVM testnet",
                "escrowAmount": {"TestEther": 10},
                "timeoutInBlocks": 100,
                "maxInitialBlock": 61247},
    "code-digest": "0xaac1265d31e58390f2971bb58004f1944082116908ddb9c4a64be8b7d495c757"}]
```

Then, as the buyer and first participant, Alice creates the contract,
and sends a handshake containing verifiable contract creation information
together with the agreement as context.

```
["agreement-handshake",
  <… copy of the interaction agreement …>
  {"contract-address": "0xaf0FdEA3C5eF883dB314fb8d5c5cf2892c8efC30",
   "code-hash": "0xaac1265d31e58390f2971bb58004f1944082116908ddb9c4a64be8b7d495c757",
   "creation-hash": "0xdffd466220658c75bf7a300babd981599b0fd1c268a073239605b391bf3b396e",
   "creation-block": 61240}]
```

Bob's *Glow* agent will verify
that the handshake indeed corresponds to an agreement he had with Alice,
that the contract was indeed created as per the agreed upon parameters,
and was suitably confirmed on the blockchain.
The agent will then do the parts agreed upon.

## Rock, Paper, Scissors (RPS)

Here is a minimal variant of an interaction for Rock, Paper, Scissors.

In this well-known game, two participants play by each choosing one of three possible hands,
that they reveal at the same time.
Depending on the choices, one wins, or the other wins, or it’s a draw.

Now, there is no such thing as “at the same time” on the blockchain:
it is built on top of an *asynchronous* message passing network,
iu`;a,and simultaneity isn't available as a primitive.

Therefore, in a naive attempt to implement that interaction,
whichever participant shows his hand first is sure to lose,
as the other participant may make a last minute change to his hand so as to win,
[like this real-life robot](https://www.youtube.com/watch?v=ZVNnoOcohaU).

In our version of RPS we simulate simultaneity using a simple cryptographic technique called
a [commitment scheme](https://en.wikipedia.org/wiki/Commitment_scheme).

Let's call our two participants [Alice and Bob](https://en.wikipedia.org/wiki/Alice_and_Bob),
as per the tradition.

First, Alice chooses her hand, but does not reveal it.
Instead, she reveals a *commitment*:
a digest of her hand concatenated to a random salt by a cryptographic one-way hash function.

This commitment ensures that she cannot change her mind later,
but can only play the hand that she committed to.
Therefore Bob is confident that Alice cannot cheat.

The long enough salt ensures that Bob cannot feasibly guess her hand by trying
all the possible pre-images to the digest function: with no salt at all or a short salt,
Bob could just [brute force](https://en.wikipedia.org/wiki/Brute-force_attack) his way
into guessing Alice's hand by trying all the possibilities.
Therefore Alice is confident that she reveals no usable information to Bob with the commitment.

In a second step, Bob, can then safely reveal his hand without any possibility of cheating by either party.

Finally, Alice reveals her hand; the outcome is computed;
and the winner takes the pot of tokens, while the loser gets nothing---unless it’s a draw
at which point they each get their wager back.

The interaction is thus in three steps, as follows:

```
data Hand = | Rock | Paper | Scissors;

data Outcome = | B_Wins | Draw | A_Wins;

let winner = (handA : Hand, handB : Hand) : Outcome => {
    Outcome.ofNat((Hand.toNat(handA) + (4 - Hand.toNat(handB))) % 3) }

@interaction([A, B])
let rockPaperScissors = (wagerAmount) => {
  @A let handA = Hand.input("First player, pick your hand");
  @A let salt = randomUInt256();
  @verifiably!(A) let commitment = digest(salt, handA);
  publish! A -> commitment;
  deposit! A -> wagerAmount;

  @B let handB = Hand.input("Second player, pick your hand");
  publish! B -> handB;
  deposit! B -> wagerAmount;

  publish! A -> salt, handA;
  verify! commitment;
  let outcome = winner(handA, handB);
  switch(outcome) {
    | B_Wins => withdraw! B <- 2 * wagerAmount
    | Draw =>   withdraw! A <- wagerAmount;
                withdraw! B <- wagerAmount
    | A_Wins => withdraw! A <- 2 * wagerAmount }}
```

The contract starts with the familiar and mandatory line `#lang glow`.

The first two statements define two simple data types:

- The first one, `Hand`, is a sum type describing the choices
  that each participant can make as to a hand to play.

- The second one, `Outcome`, is a sum type describing
  the potential outcomes of the interaction.

Each of these types happens to have three *constructors*:

- a `Hand` can be `Rock`, `Paper` or `Scissors`.

- an `Outcome` can be `B_Wins`, `Draw` or `A_Wins`.

```
data Hand = | Rock | Paper | Scissors;

data Outcome = | B_Wins | Draw | A_Wins;
```

Implicitly defined are four functions:

1. `Hand.toNat`

2. `Hand.ofNat`

3. `Outcome.toNat`

4. `Outcome.ofNat`

These functions map between the constructors of each type and natural integers, in order.
Thus:

- `Hand.toNat(Rock)` is `0`
- `Hand.toNat(Paper)` is `1`
- `Hand.toNat(Scissors)` is `2`
- `Hand.ofNat(0)` is `Rock`
- `Hand.ofNat(1)` is `Paper`
- `Hand.ofNat(2)` is `Scissors`

Similarly, the functions for `Outcome` establish a correspondance between
`B_Wins`, `Draw` and `A_Wins` (respectively) with `0`, `1` and `2` (respectively).

These implicitly defined functions and the carefully chosen order of the outcomes
make it possible to compute the outcome with this “magic”-looking function:

```
let winner = (handA : Hand, handB : Hand) : Outcome => {
  Outcome.ofNat((Hand.toNat(handA) + (4 - Hand.toNat(handB))) % 3) }
```

Without using the conversion to integers, we could have used a more verbose definition
using pattern-matching as follows:

```
let winner = (handA : Hand, handB : Hand) : Outcome => {
  switch([handA, handB]) {
  | [Rock,     Rock]     => Draw
  | [Rock,     Paper]    => B_Wins
  | [Rock,     Scissors] => A_Wins
  | [Paper,    Rock]     => A_Wins
  | [Paper,    Paper]    => Draw
  | [Paper,    Scissors] => B_Wins
  | [Scissors, Rock]     => B_Wins
  | [Scissors, Paper]    => A_Wins
  | [Scissors, Scissors] => Draw }
```

Now comes the definition of the interaction `rockPaperScissors`.
As the annotation specifies, it has two roles called `A` and `B`,
and a single parameter, the `wagerAmount`.
We will keep calling Alice that participant with role `A`,
and Bob the participant with role `B`.

The first step is for Alice to enact, as follows:

```
  @A let handA = Hand.input("First player, pick your hand");
  @A let salt = randomUInt256();
  @verifiably!(A) let commitment = digest([salt, handA]);
  publish! A -> commitment;
  deposit! A -> wagerAmount;
```

Note that the variable definitions preceded by `@A`
only take place on Alice’s computer.
Their values are jealously kept secret by Alice,
who stashes them safely in her private database.
They are not revealed to Bob or to anyone,
until and unless Alice reveals them as part of the protocol,
if she ever does.
And indeed, the variable `commitment` is published
by the fourth statement `publish! A -> commitment;`

Moreover, notice how the commitment definition is annotated by `@verifiably!`.
This ensures that the formula used to define it can later be verified with `verify!`.

Also, in the beginning, the `input` statement means that Alice will choose a hand.
This invokes a private interaction on Alice's computer,
between the *Glow* runtime that handles the blockchain layer,
and the user interface of Alice's DApp.
Note that `input` statements are only allowed within the private context
of a single participant, in this case, `@A`.
It is not meaningful and not allowed by the language for an `input` statement
to happen within the consensual part of the interaction, without an `@<participant>` annotation.

In the middle, the `commitment` is computed using the special function `digest`,
applied to the pair of the cryptographic digest of the salt and hand.
A `digest` is computed by a “one-way hash function”,
such that the odds that two different values of the same type have the same digest
is astronomically unlikely.

In the end, Alice publishes her commitment, and deposits her wager into the contract,
that manages “the pot”.

The second step is simpler:

```
  @B let handB = Hand.input("Second player, pick your hand");
  publish! B -> handB;
  deposit! B -> wagerAmount;
```

Bob just reveals his hand and deposits his share into the pot.

The third step in only slightly involved:

```
  publish! A -> salt, handA;
  verify! commitment;
  let outcome = winner(handA, handB);
  switch(outcome) {
    | B_Wins => withdraw! B <- 2 * wagerAmount
    | Draw =>   withdraw! A <- wagerAmount;
                withdraw! B <- wagerAmount
    | A_Wins => withdraw! A <- 2 * wagerAmount }}
```

Alice reveals her salt and hand.

Everybody verifies that this is indeed correct
(or else Alice’s transaction attempt fails and it is as if she did nothing).
The outcome is computed, and depending on it, the pot is redistributed.

Now, what if Alice never starts the interaction? Then she times out,
and she just wasted a bit of her and Bob’s time.

What if Alice does her first step, but Bob never replies? Then Bob times out,
Alice invokes the contract to get her money back, and
all Bob did was waste a bit of her time and some gas she spent
to create the contract and get her money back.

What if Alice does the first step, Bob does the second step,
but Alice never does the third step? Then she times out, and
Bob can invoke the contract to get the entire pot since she’s in default.

Now, what if Alice, after Bob reveals his hand, realizes that she has lost?
Why should she bother to publish the last message? She loses anyway,
she could as well just let Bob waste his time and pay the transaction fees
to collect his winnings after she times out.

To disincentivize Alice from behaving in this way,
*Glow* can (and in the near future, will) automatically
transform the above contract into the below, wherein
Alice has to deposit an additional amount of money in escrow
to ensure she would lose more by timing out than by participating, and
that Bob would be compensated more this way than he would have wasted while waiting.
Compare with the above, and/or look for lines that include the new variable `escrowAmount`.

```
@interaction([A, B])
let rockPaperScissors = (wagerAmount, escrowAmount) => {
  @A let handA = Hand.input("First player, pick your hand");
  @A let salt = randomUInt256();
  @verifiably!(A) let commitment = digest(salt, handA);
  publish! A -> commitment;
  deposit! A -> wagerAmount;
  deposit! A -> escrowAmount;

  @B let handB = Hand.input("Second player, pick your hand");
  publish! B -> handB;
  deposit! B -> wagerAmount;

  publish! A -> salt, handA;
  verify! commitment;
  let outcome = winner(handA, handB);
  switch(outcome) {
    | B_Wins => withdraw! B <- 2 * wagerAmount
    | Draw =>   withdraw! A <- wagerAmount;
                withdraw! B <- wagerAmount
    | A_Wins => withdraw! A <- 2 * wagerAmount }
  withdraw! A <- escrowAmount }
```

Notice how Bob does *not* need to deposit an escrow,
because his first message is also his last message,
so there is no opportunity for him to stop cooperating after he started.
