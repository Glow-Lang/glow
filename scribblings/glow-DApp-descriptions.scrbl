#lang scribble/manual

@(require "glow-code.rkt"
          "glow-docs.rkt"
          (for-label glow))
@title{Glow DApps Supported}

Currently Glow supports three example DApps.

@itemize[
    @item{@secref{Buy_Sign}}
    @item{@secref{Coin_Flip}}
    @item{@secref{Rock_Paper_Scissors}}
]

@section[#:tag "Buy_Sign"]{Buy Signature Agreement}

A buy-sig agreement is an interaction between two parties, where the DApp specifies
the sale price and the hash of the document to be signed. When the seller provides
the correct signature proving that they have signed the correct document, the DApp
transfers the assets previously deposited by the Buyer to the Seller.

Parameters
@itemize[
    @item{Buyer Address}
    @item{Seller Address}
    @item{Sale Price}
    @item{Document Hash to be Signed}
]

@section[#:tag "Coin_Flip"]{Coin Flip Game}

A coin flip game is an interaction between two parties, where the DApp specifies
the wager amount and calculates the winner based on integers chosen by each player.
Each party provides their choice (randomly generated integer) from which the DApp
computes the winner and sends the wager to that party. This DApp illustrates how
untrusting participants can generate a number mutually trusted to be random. The
uses for this protocol are far ranging in a wide number of situations, and the bet
on whether the number is odd is only meant for illustration, being the simplest way
to use the randomness to control the distribution of digital assets.

Parameters
@itemize[
    @item{Player 1 Address}
    @item{Player 1 Integer Choice}
    @item{Player 2 Address}
    @item{Player 2 Integer Choice - This is provided after the DApp is deployed.}
    @item{Wager - This amount will be sent from the loser to the winner after execution.}
]

@section[#:tag "Rock_Paper_Scissors"]{Rock Paper Scissors Game}

A rock-paper-scissors game is an interaction between two parties, where the DApp
specifies the wager amount and calculates the winner based on simple rules
(Rock > Scissors, Scissors > Paper, Paper > Rock). When both parties provide their
choice, called a "hand", the DApp computes the winner and sends the wager to that
party. In the event of a tie, the interaction is complete and the wager does not
exchange between the players.

Parameters
@itemize[
    @item{Player 1 Address}
    @item{Player 1 Hand}
    @item{Player 2 Address}
    @item{Player 2 Hand - This is provided after the DApp is deployed.}
    @item{Wager - This amount will be sent from the loser to the winner after execution.}
]
