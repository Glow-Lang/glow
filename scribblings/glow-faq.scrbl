#lang scribble/manual

@(require "glow-code.rkt"
          "glow-docs.rkt"
          (for-label glow))

@title{Glow FAQ}

@itemize[
    @item{Is @(Glow) a platform? A language? Both? Both! @(Glow) is a DSL, a domain-specific
    language. It is also a platform with libraries. In the long-run, we might name these two
    differently to avoid confusion.}

    @item{What can I do with @(Glow)? DApps with @(Glow) with native tokensInteractions:
    transaction from one person to another who don't trust each otherDirect-style code
    generation: the contract checks every transaction.}

    @item{What is the product of @(Glow)? Is it a file? You enter settings in the command line
    and @(Glow) executes on the specified blockchain (currently the @(Glow) testnet, later,
    we'll publish a list of the supported networks)}

    @item{What type of contract can I build with @(Glow)? Currently @(Glow) supports 3 contract
    types: Buy Signature, Coin Flip, Rock-Paper-Scissors}

    @item{So I don't compile a file in @(Glow)? Currently, the smart contracts are compiled as
    they are executed. Your computer will locally keep track of the executed transactions.}

    @item{Do I have to install other languages or modules to use @(Glow)? We install everything
    for you when you type a single command line. Technically, @(Glow) needs a few compilers to
    work (Gerbil, Scheme and C compiler), but they will all be installed automatically.}

    @item{Can I save my custom smart contract in @(Glow)? Yes, you can create a text file,
    in your favorite text editor.}

    @item{If I am a DApp developer willing to make a new DApp, how do I import a @literal{.glow}
    contract? For now, you can :- in any language (be it C++, python, PHP, whatever...) from the
    command line, you can import the required smart contract. If you program in Gerbil-scheme you
    can invoke our API. In the future, you will be able to do that in our User Interface (coming
    around June 2021), and @(Glow) will have an API invoking @(Glow) contracts from Javascript
    APIs for a specific language can be considered by our team, and we imagine our community will
    probably develop specific APIs.}

    @item{Will @(Glow) allow me to make a full DApp from scratch? Ultimately, you will be able to
    use @(Glow) on ANY DApp, but for blockchain-related functions. If you are making a video game
    and willing to create a physics engine, or if you are looking for neat design features, these
    aren't what @(Glow) does. For that, you'll still have to use other languages.}

    @item{Will the @literal{.glow} files that I make in the beta be compatible with the next
    versions of @(Glow)? Normally, yes! If we ever introduce incompatibilities, we'll do our best
    to drive the community towards a new workflow.}

    @item{At this point how much time do you need to support another blockchain? That depends on
    the blockchain. For most EVM blockchains it should only require a few lines in a config file.
    For something else, roughly two to three months of work for the initial version. It would
    also require two annual contracts, one to prevent the code from bitrotting, and another to
    maintain the backend as we add language features.
    
    @item{Bitcoin? Its script language sucks, so it would require roughly 6 months to work around
    all the quirks, and even then only the forks would be fully supported. Bitcoin itself is
    missing some essential capabilities. That is typically something we're considering because of
    the popularity of Bitcoin, though.}

    @item{Bitcoin, really? OK, we can actually do it, but it's 6 more months of work to use clever
    tricks to overcome the limitations, and then the transaction fees will be so high that you'll
    only want to use that for very high value contracts, such as side-chain maintenance. Once
    that is done, keep tripling the price compared to Bitcoin-without-BTC.}
    
    @item{Is there a @(Glow) library for NFT, and ERC20 contracts? Not yet, but this is next on the
    list of features we're working on.}

    @item{Can you quickly sum-up the benefits of @(Glow)? First, writing a DApp that is 10x safer
    for 1/10th the effort than using say Solidity. Second, writing it once, and having it work
    everywhere, and not just on EVM blockchains. You don't have to guess where the liquidity will
    be and then bank on a technology that only works on one network. Third, generating not just the
    "smart contract", but the client and server code that goes with it for each participant. Fourth,
    having a chance at formal methods.}

    @item{Does this work on EVM? Just saw that it mentioned Cardano only? It works on the EVM.
    Our default test network is a devnet for a side-chain on Cardano. But it will work on any
    EVM, really.}

    @item{How are you generating JavaScript? Are you directly writing it or are you using
    something like reasonml? We have a Scheme to JS compiler, with a good FFI, and we write
    direct JS where we need it.}

    @item{Are you compiling the contract part directly to EVM bytecode? Yes we are. The compiler
    is lacking in micro optimizations, but we're making up for it in macro optimizations instead,
    with plenty of ideas how to do even better.}

    @item{How do you go about the contract code being self-certifying or certified by the likes
    of Etherscan? You certify your client. The client certifies the contracts it deals with. We
    also register our source code off-chain, as doing it on-chain would be worthless and
    expensive: no amount of useful on-chain metadata can possibly be worth its cost in "helping"
    you look up the hash of the contract's initcode in your offchain database. In this regard,
    you will find many structural differences in Glow, compared to say Solidity.}

    @item{If @(Glow) is CLI based for interacting with the blockchain then how is a smart contract
    deployed? The CLI client for @(Glow) can generate a contract between you and the other
    participant, or reuse existing contracts. As part of your interaction, the first participant
    creates the contract, the last participant deletes it. In other words, the @(Glow) CLI talks
    to the blockchain for you, so you don't have to talk to the blockchain yourself, which is a
    very safety-critical activity.}

    @item{Is there a @(Glow) compiler? @(Glow) dynamically compiles code into blockchain smart
    contracts. @(Glow) interprets code on the client side (where it is 1000000 cheaper than on
    the blockchain, so if you could accept the 1000000x cost of running on the blockchain, you can
    easily afford the 10x cost of interpreting vs compilation). Today, you can invoke @(Glow) from
    any language by spawning a process. Or you can invoke it from Gerbil Scheme REPL. Tomorrow,
    we'll add an HTTP API and a JS API.}


]
