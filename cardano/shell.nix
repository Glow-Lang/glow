{ addNodeScripts ? false
, addDemoScripts ? false
}:
let
  pkgs = import ../pkgs.nix;

  cardano-node = import (pkgs.thunkSource ../dep/cardano-node) {};
  cardano-wallet = import (pkgs.thunkSource ../dep/cardano-wallet) {};
  gerbil-cardano = import ./default.nix;
  plutus = import (pkgs.thunkSource ../dep/plutus) { rev = "unknown"; };

  start-testnet-node-script = pkgs.writeScriptBin "start-testnet-node" ''
    ${cardano-node.scripts.testnet.node}
  '';
  start-wallet-script = pkgs.writeScriptBin "start-wallet" ''
    ${cardano-wallet.cardano-wallet}/bin/cardano-wallet serve \
      --listen-address 127.0.0.1 \
      --port 3002 \
      --node-socket ./state-node-testnet/node.socket --testnet ${cardano-node.environments.testnet.networkConfig.ByronGenesisFile} \
      --database ./wallet-database
  '';
  node-scripts = if addNodeScripts then [ start-testnet-node-script start-wallet-script] else [];
  demo-scripts = if addDemoScripts then (plutus.plutus-scb.demo-scripts "./demo").installPhase else "";
in
  pkgs.mkShell {
    inputsFrom = [
      gerbil-cardano
    ];
    buildInputs = [
      pkgs.thunkExe
    ] ++ node-scripts;
    shellHook = gerbil-cardano.postConfigure + "\n" + demo-scripts;
    GERBIL_APPLICATION_HOME = "./";
  }