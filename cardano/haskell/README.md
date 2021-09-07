# glow-cardano-pab

Glow Cardano Backend. Supported by Plutus Application Backend and a Plutus State Machine.

## Setting up

### VSCode devcontainer

Use the provided VSCode devcontainer to get an environment with the correct tools set up.

- Install Docker
- Install VSCode
  - Install the [Remote Development extension pack](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack)
  - You do *not* need to install the Haskell extension
- Ensure you have a `~/.cabal/packages` folder. You can create this via `mkdir -p ~/.cabal/packages`; it's used to cache cabal packages.
- Clone this repository and open it in VSCode
  - It will ask if you want to open it in the container, say yes.
  - The first time it will take a few minutes to download the devcontainer image from dockerhub,
  - `cabal build` from the terminal should work (unless you didn't have a `~/.cabal` folder, in which case you'll need to run `cabal update` first.)
  - Opening a Haskell file should give you IDE features (it takes a little while to set up the first time)

Note: This uses the [plutus-starter-devcontainer image on dockerhub](https://hub.docker.com/r/inputoutput/plutus-starter-devcontainer), if
you wish to build the image yourself, you can do so as follows:
  - Clone https://github.com/input-output-hk/plutus,
  - Set up your machine to build things with Nix, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!),
  - Build and load the docker container: `docker load < $(nix-build default.nix -A devcontainer)`,
  - Adjust the `.devcontainer/devcontainer.json` file to point to your local image.

### Cabal+Nix build

Alternatively, use the Cabal+Nix build if you want to develop with incremental builds, but also have it automatically download all dependencies.

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
$ nix-shell
```

Otherwise, you can use [direnv](https://github.com/direnv/direnv) which allows you to use your preferred shell. Once installed, just run:

```
$ echo "use nix" > .envrc # Or manually add "use nix" in .envrc if you already have one
$ direnv allow
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor.
See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.

## Setting up the GlowContract

Here's an example of running and interacting with this contract via the API. For this it will help if you
have `jq` installed.

1. Build the PAB executable:

```
cabal build glow-contract
```

2. Run the PAB binary:

```
cabal exec -- glow-contract
```

This will then start up the server on port 8080. The devcontainer process will then automatically expose this port so that you can connect to it from any terminal (it doesn't have to be a terminal running in the devcontainer).

First, let's verify that the game is present in the server:

3. Check what contracts are present:

```
curl -s http://localhost:8080/api/new/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.

We're interested in the `GlowContract` one.

## Running the GlowContract

The game has two players (wallets). One will initialise the contract and lock a value inside. Another
wallet will then make guesses. Supposing they guess correctly, they'll receive the funds that were
locked; otherwise, they won't!

1. Start the instances:

```sh
# Wallet 1
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GlowContract", "caWallet":{"getWallet": 1}}' \
  http://localhost:8080/api/new/contract/activate | jq

# Wallet 2
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GlowContract", "caWallet":{"getWallet": 2}}' \
  http://localhost:8080/api/new/contract/activate | jq
```

From these two queries you will get back two contract instance IDs:

``` sh
# Wallet 1
{
  "unContractInstanceId": "a241655c-d45a-4aeb-a957-5789b0c7cc4e"
}

# Wallet 2
{
  "unContractInstanceId": "dd84b9e1-f7e5-4531-b6ed-38b2171ca660"
}

```

These will be needed
in the subsequent steps for running actions against. We can optionally take a look at the state
of the contract with the `status` API:

2. Get the status

```
export INSTANCE_ID="2f4f2b54-2010-40e8-8865-c817bec93343"
curl -s http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq

export INSTANCE_ID_2="6ede7bc2-44fd-45c3-bf29-1c912efd228a"
curl -s http://localhost:8080/api/new/contract/instance/$INSTANCE_ID_2/status | jq
```

This has a lot of information; and in particular we can see what endpoints are still available
to call.

=========== WIP ======================================

3. Start the game by locking some value inside

Now, let's call the `lock` endpoint to start the game. In order to do so, we need to construct
a JSON representation of the `LockParams` that the endpoint takes (look at `Game.hs`). The easiest
way is to simply build the term in haskell and ask `aeson` to encode it.

From the terminal:
```sh
nix-shell
cabal v2-repl
```

`cabal v2-repl:`
```haskell
import Client (CreateParams(..))
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map
import Types
import Data.Text (Text)
import Ledger.Ada
args = CreateParams { datatypes = datatypeMap
                    , participants = addressMap
                    , arguments = variableMap
                    , contract = glowContract
                    , timeoutLength = defaultTimeoutLength
                    }
    where
      datatypeMap :: DatatypeMap
      datatypeMap = Map.empty
      
      glowContract :: GlowContract
      glowContract = undefined
      
      variableMap :: VariableMap
      variableMap = undefined
      
      addressMap :: Map Text Ledger.PubKey
      addressMap = undefined

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
BSL.putStrLn $ encode args
{"amount":{"getValue":[[{"unCurrencySymbol":""},[[{"unTokenName":""},90]]]]},"secretWord":"eagle"}
```

Great! This is all we need to call the `lock` endpoint, so let's do that now with
the instance from Wallet 1:

4. Lock some value (Wallet 1)

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"amount":{"getValue":[[{"unCurrencySymbol":""},[[{"unTokenName":""},90]]]]},"secretWord":"eagle"}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/lock
```

We can do likewise to work out what the JSON for `GuessParams` is, and then make a guess from
Wallet 2:

5. Make a guess (Wallet 2)

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"guessWord": "duck"}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/guess
```

Note that this guess is wrong, so in the log of the server we will see that the transaction
didn't validate.

As an exercise, you can now spin up another instance for Wallet 2 and make a correct guess, and
confirm that the transaction validates and the Ada is transferred into the right wallet.

Note that you can verify the balances by looking at the log of `plutus-starter-pab`
when exiting it by pressing return.

Finally, also node that the PAB also exposes a websocket, which you can read about in
the general [PAB Architecture documentation](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).
