#lang scribble/manual

@(require "glow-code.rkt"
          "glow-docs.rkt"
          (for-label glow))

@title{Glow FAQ}

@itemize[
    @item{Is GLOW a platform? A language? Both? Both. GLOW is a DSL, a domain-specific language. It is also a platform with libraries. In the long-run, we might name these two differently to avoid confusion.

    @item{What can I do with GLOW? DApps with GLOW with native tokensInteractions: transaction from one person to another who don't trust each otherDirect-style code generation: the contract checks every transaction.}

    @item{What is the product of GLOW? Is it a file? You enter settings in the command line and GLOW executes on the specified blockchain (currently the GLOW testnet, later, we'll publish a list of the supported networks)}
    @item{What type of contract can I build with GLOW? Currently GLOW supports 3 contract types: Buy Signature, Coin Flip, Rock-Paper-Scissors}

    @item{So I don't compile a file in GLOW? Currently, the smart contracts are compiled as they are executed. Your computer will locally keep track of the executed transactions.}

    @item{Do I have to install other languages or modules to use GLOW? We install everything for you when you type a single command line. Technically, GLOW needs a few compilers to work (Gerbil, Scheme and C compiler), but they will all be installed automatically.}

    @item{Can I save my custom smart contract in GLOW? Yes, you can create a text file, in your favorite text editor.}

    @item{If I am a DApp developer willing to make a new DApp, how do I import a .GLOW contract? For now, you can :- in any language (be it C++, python, PHP, whatever...) from the command line, you can import the required smart contract. If you program in Gerbil-scheme you can invoke our API. In the future, you will be able to do that in our User Interface (coming around June 2020), and GLOW will have an API invoking GLOW contracts from Javascript APIs for a specific language can be considered by our team, and we imagine our community will probably develop specific APIs.}

    @item{Will GLOW allow me to make a full DApp from scratch? Ultimately, you will be able to use GLOW on ANY DApp, but for blockchain-related functions. If you are making a video game and willing to create a physics engine, or if you are looking for neat design features, these aren't what GLOW does. For that, you'll still have to use other languages.}

    @item{Will the .GLOW files that I make in the beta be compatible with the next versions of GLOW? Normally, yes! If we ever introduce incompatibilities, we'll do our best to drive the community towards a new workflow.}
]
