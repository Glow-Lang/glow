@(require "glow-code.rkt"
          "glow-docs.rkt"
          (for-label glow))
@title{Glow Contracts Supported}

Currently GLOW supports three example contracts.

@bold{@itemize[
    @item{Buy Signature Agreement}
    @item{Coin Flip Game}
    @item{Rock Paper Scissors Game}
]}

@bold{Buy Signature Agreement}

A buy-sig contract is an interaction between two parties, where the contract 
specifies the sale price and the hash of the document to be signed. When the
seller provides the correct signature proving that they have signed the correct
document, the contract releases moves the price amount from the buyer, to the
seller.

Parameters
@itemize[
    @item{Buyer Address}
    @item{Seller Address}
    @item{Sale Price}
    @item{Document Hash to be Signed}
]

@bold{Coin Flip Game}

A coin flip game is an interaction between two parties, where the contract
specifies the wager amount and calculates the winner based on integers chosen
by each player. When both parties provide their choice, called a "hand", the
contract computes the winner and sends the wager to that party.

Parameters
@itemize[
    @item{Player 1 Address}
    @item{Player 1 Hand}
    @item{Player 2 Address}
    @item{Player 2 Hand. This is provided after the DApp is deployed.}
    @item{Wager. This amount will be sent from the loser to the winner after execution.}
]

@bold{Rock Paper Scissors Game}

A rock-paper-scissors game is an interaction between two parties, where the
 contract specifies the wager amount and calculates the winner based on simple
 rules (Rock > Scissors, Scissors > Paper, Paper > Rock). When both parties
 provide their choice, called a "hand", the contract computes the winner and
 sends the wager to that party. In the event of a tie, the interaction is
 complete and the wager does not exchange between the players.

Parameters
@itemize[
    @item{Player 1 Address}
    @item{Player 1 Hand}
    @item{Player 2 Address}
    @item{Player 2 Hand. This is provided after the DApp is deployed.}
    @item{Wager. This amount will be sent from the loser to the winner after execution.}
]
