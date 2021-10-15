# Tutorial: Buying a Signature

Ensure you have [`glow`](https://github.com/Glow-Lang/glow/blob/master/INSTALL.md) installed.

*Glow* comes prepackaged with several smart contracts.

This tutorial will bring you through executing [`buy_sig`](https://github.com/Glow-Lang/glow/blob/master/dapps/buy_sig.glow).

## Understanding the "buy_sig" contract

### Scenario

Suppose we have 2 parties, alice and bob.
alice has a message digest which she wants bob to sign.
For that, bob wants to be paid.

Hence they participate in an interaction, via the [`buy_sig`](https://github.com/Glow-Lang/glow/blob/dapps/buy_sig.glow) smart contract.

First we declare the participants,
a buyer who wants a digest signed,
and a seller, who signs the digest for a fee.

``` sh
#lang glow
@interaction([Buyer, Seller])
```

For the buyer to get their digest signed,
they will need to supply both the digest and the fee,
as indicated by the function signature.

```sh
let buySig = (digest : Digest, price : Nat) => {

```

First, the buyer has to deposit the seller's fee into escrow.

```sh
  deposit! Buyer -> price;
```

This produces an `handshake`, which has to be sent to the seller, to indicate payment is made.

Upon receiving this, the Seller publicly signs the digest and withdraws their fee.
```sh
  @publicly!(Seller) let signature = sign(digest);
  withdraw! Seller <- price;
};
```

*The whole contract is available [`here`](https://github.com/Glow-Lang/glow/blob/master/dapps/buy_sig.glow).*

We will proceed to actually running this interaction on the `Cardano EVM Testnet`.

## Setting up our identity

We will first need identities, to uniquely identify participants.

``` sh
> glow help generate-identity
Glow v0.1.0-120-g43060af
Usage: glow generate-identity [command-option ...]
       Generate identity

Command Options:
 -I --identities <identities>     file to load and store identities [default: #f]
 --test                           enable testing including test identities
 -N --nickname <nickname>         nickname of identity [default: #f]
 -P --prefix <prefix>             desired hex prefix of generated address [default: #f]
```

Let us create 3 participants: Alice, Bob, Charles.

``` sh
> glow generate-identity -N alice
Generated identity: Alice [ 0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac ]

> ./glow generate-identity -N bob
Generated identity: Bob [ 0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9 ]

> ./glow generate-identity -N charles
Generated identity: Charles [ 0x47312084A1026E2B5ae7E3A7ef6f328421740961 ]
```

## Funding our accounts

Next, we want to fund our new account holders. We can then do transactions and computations on the EVM.
``` sh
> glow faucet -t alice
Connecting to the Cardano EVM Devnet at https://rpc-evm.portal.dev.cardano.org/ ...

Requesting 1.0 CED token from faucet https://faucet-evm.portal.dev.cardano.org/ to address 0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac:


Initial balance: 0 CED

["to","https://faucet-evm.portal.dev.cardano.org/","request","{\"id\":5,\"jsonrpc\":\"2.0\",\"method\":\"faucet_sendFunds\",\"params\":[\"0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac\"]}"]
["from","https://faucet-evm.portal.dev.cardano.org/","response","{\"jsonrpc\":\"2.0\",\"result\":\"0x84099d6f7e0de9c2d6eef90f86bb20bfd76ee08d05a25d752fb46519a24a99d2\",\"id\":5}"]
Transaction:
  (instance TransactionInformation hash: (bytes<-0x "0x84099d6f7e0de9c2d6eef90f86bb20bfd76ee08d05a25d752fb46519a24a99d2") nonce: 31 from: (address<-0x "0x218796890F4c147adE276B43f27051C598FCa629") to: (address<-0x "0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac") value: 1000000000000000000 gasPrice: 20000000000 gas: 90000 input: (bytes<-0x "0x"))
Waiting for confirmation...

Final balance: 1 CED

> glow faucet -t bob
... # Similar to above
```

## Viewing the available applications

We want to see the available applications:

``` sh
> glow list-applications
buy_sig     /path/to/glow/dapps/buy_sig.glow
coin_flip   /path/to/glow/dapps/coin_flip.glow
rps_simple  /path/to/glow/dapps/rps_simple.glow
```

In this practice, we will do [`buy_sig`](https://github.com/Glow-Lang/glow/blob/master/dapps/buy_sig.glow).

## Deploying the contract

We deploy the contract first, via `glow start-interaction`.
``` sh
> glow start-interaction
Connecting to the Cardano EVM Devnet at https://rpc-evm.portal.dev.cardano.org/ ...

Choose application:
1) buy_sig
2) coin_flip
3) rps_simple
Enter number
> 1
Choose your identity:
1) alice - 0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac # Note, your signatures might defer here
2) bob - 0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9   # and here.
Enter number
> 1
Choose your role:
1) Buyer
2) Seller
Enter number
> 1
Assign roles
Select address for Seller:
1) alice - 0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac
2) bob - 0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9
3) charles - 0x47312084A1026E2B5ae7E3A7ef6f328421740961
Enter number
> 2
Define parameters
Enter digest
> 0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607 # You can reuse this / see "glow help digest" to generate one.
> Enter price
> 1
Max initial block [ Current block number is 107850 ]
> 107900 # Just indicate a block after the current one.
One line command for other participants to generate the same agreement:
glow start-interaction --agreement '{"glow-version":"Glow v0.1.0-120-g43060af","interaction":"buy_sig#buySig","participants":{"Buyer":"0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac","Seller":"0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9"},"parameters":{"digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607","price":"0x1"},"reference":{},"options":{"blockchain":"Cardano EVM Devnet","timeoutInBlocks":"0x3e8","maxInitialBlock":"0x1a572"},"code-digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607"}'

Executing code block begin0 ...
(add-to-deposit price)
(@debug-label dlb1)
```

Done! We are now going to perform the transactions for alice and bob.

### Buying (alice)

Next, we start a new terminal window, and call the contract with the buyer.
This can be done with the above *one line command*, after we deployed the contract.

Remember to use separate databases, as shown by the `--database alice`.

``` sh
> $ ./glow start-interaction --agreement '{"glow-version":"Glow v0.1.0-120-g43060af","interaction":"buy_sig#payForSign
ature","participants":{"Buyer":"0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac","Seller":"0x0C7A123580a2A6E40b053b2dE913fd
0e2B8b91e9"},"parameters":{"digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607","price":"0x1"
},"reference":{},"options":{"blockchain":"Cardano EVM Devnet","timeoutInBlocks":"0x3e8","maxInitialBlock":"0x1a572"},"
code-digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607"}' --database alice
Connecting to the Cardano EVM Devnet at https://rpc-evm.portal.dev.cardano.org/ ...

Choose your identity:
1) alice - 0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac
2) bob - 0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9
3) charles - 0x47312084A1026E2B5ae7E3A7ef6f328421740961
Enter number
> 1
Choose your role:
1) Buyer
2) Seller
Enter number
> 1

Executing code block begin0 ...
(add-to-deposit price)
(@debug-label dlb1)

Send the handshake below to the other participant:
{"agreement":{"glow-version":"Glow v0.1.0-120-g43060af","interaction":"buy_sig#buySig","participants":{"Buyer":"0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac","Seller":"0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9"},"parameters":{"digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607","price":"0x1"},"reference":{},"options":{"blockchain":"Cardano EVM Devnet","timeoutInBlocks":"0x3e8","maxInitialBlock":"0x1a572"},"code-digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607"},"contract-config":{"contract-address":"0x3798bbAa4e3a3Aec6a84f96750a5C51C2bA7436C","code-hash":"0x69cd922d5fbf72be7795910f76ff653ed6e41880d84fcbd7392a8650341b1ffe","creation-hash":"0xeed82abee1fb5a4b121f43790801b94ebb7e31440c8bf364178707f48ab27798","creation-block":"0x1a54e"},"published-data":"0x"}
```

We have now done our part to put our funds in escrow. It is now *bob's* turn to sign and receive the funds.

### Signing and Receiving (bob)

Remember to use separate databases, as shown by the `--database bob`.

``` sh
> $ ./glow start-interaction --agreement '{"glow-version":"Glow v0.1.0-120-g43060af","interaction":"buy_sig#buySig","participants":{"Buyer":"0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac","Seller":"0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9"},"parameters":{"digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607","price":"0x1"},"reference":{},"options":{"blockchain":"Cardano EVM Devnet","timeoutInBlocks":"0x3e8","maxInitialBlock":"0x1a572"},"code-digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607"}' --database bob
Connecting to the Cardano EVM Devnet at https://rpc-evm.portal.dev.cardano.org/ ...

Choose your identity:
1) alice - 0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac
2) bob - 0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9
3) charles - 0x47312084A1026E2B5ae7E3A7ef6f328421740961
Enter number
> 2
Choose your role:
1) Buyer
2) Seller
Enter number
> 2

Paste below the handshake sent by the other participant:
{"agreement":{"glow-version":"Glow v0.1.0-120-g43060af","interaction":"buy_sig#buySig","participants":{"Buyer":"0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac","Seller":"0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9"},"parameters":{"digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607","price":"0x1"},"reference":{},"options":{"blockchain":"Cardano EVM Devnet","timeoutInBlocks":"0x3e8","maxInitialBlock":"0x1a572"},"code-digest":"0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607"},"contract-config":{"contract-address":"0x3798bbAa4e3a3Aec6a84f96750a5C51C2bA7436C","code-hash":"0x69cd922d5fbf72be7795910f76ff653ed6e41880d84fcbd7392a8650341b1ffe","creation-hash":"0xeed82abee1fb5a4b121f43790801b94ebb7e31440c8bf364178707f48ab27798","creation-block":"0x1a54e"},"published-data":"0x"}

Executing code block begin0 ...
(expect-deposited price)
(@debug-label dlb1)

Executing code block cp0 ...
(set-participant Seller)
(def signature (sign digest0))
(add-to-publish 'signature signature)
(def tmp (@app isValidSignature Seller digest0 signature))
(require! tmp)
(@debug-label dlb2)
(participant:withdraw Seller price)
(return (@tuple))
(@label end0)

buy_sig#buySig interaction finished
Final environment:
Buyer => (address<-0x "0x4c371dA6E338F19B77BC78498DaFcFB05E8bd2Ac")
Seller => (address<-0x "0x0C7A123580a2A6E40b053b2dE913fd0e2B8b91e9")
digest => (bytes<-0x "0x16c5659f6e3c70f0c53ac5abf3977e658093f1f5880bd478de8d3a87c92d9607")
price => 1
signature => (<-json Signature "d6258bfec4ae0b97b8ddb131362020f147f26d94424ef7b8bf1d98a07338cc8f44335346ec71f1f1319ae7ead3c34409b7a33d3654b216938371a1ff911613721c")
```

The signed digest is now made available for `alice`.
